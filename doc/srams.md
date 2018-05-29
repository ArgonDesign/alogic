<p align="center">
<a href="memories.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="builtins.md">Next</a>
</p>

# SRAMs

SRAMs are a commonly used building block. To aid designer productivity, Alogic
has native language support for working with SRAMs.

### Declaring SRAMs

The language supports 2 different variants of SRAMs:
  - SRAMs driven from registers
  - SRAMs driven from combinatorial signals

SRAMs can be declared with the following syntax:

```
// SRAM driven by registers
sram <type> <identifier>[<depth>];

// SRAM driven combinatorially
sram wire <type> <identifier>[<depth>];
```

The `type` of an SRAM determines the element type, and therefore the width,
of the SRAM. The `type` of an SRAM can be either a sized integer type, or a
`struct` type. The `depth` of the SRAM is given by a constant expression.

A combinatorially driven 1024 entry deep SRAM of 16 bit unsigned integers
called `storage` could be declared as:

```
sram wire u16 storage[1024];
```

### SRAM semantics

All Alogic SRAM declarations refer to an instance of a single port SRAM with a
registered read data bus. The resulting hardware interface includes 5 signals:
  - `ce` clock enable signal (active high)
  - `we` write enable signal (active high)
  - `addr` address bus
  - `wdata` write data bus
  - `rdata` read data bus

The width of the address bus is `$clog2(<depth>)`, and the width of the data
buses is determined by the data type. Note in particular that there are no
byte enable signals to use in write cycles.

SRAMs are expected to behave as described by the following Verilog model:

```
module sram #(
  parameter WIDTH,
  parameter DEPT
) (
  input wire clk,
  input wire ce,
  input wire we,
  input wire [$clog2(DEPTH)-1:0] addr,
  input wire [WIDTH-1:0] wdata,
  output reg [WIDTH-1:0] rdata
);

  reg [WIDTH-1:0] mem [DEPTH-1:0];

  always @(posedge clk) begin
    if (ce) begin
      if (we) begin
        mem[addr] <= wdata;
        rdata <= 'bx;
      end else begin
        rdata <= mem[addr];
      end
    end
  end

endmodule
```

### Working with SRAMs

Using SRAMs is Alogic code is done through interface methods and properties.

SRAMs provide the `.write` method with the following signature. This can be
used to write to an SRAM:

```
  void write(uint($clog2(<depth>)) addr, <type> data);
```

To write the value `16'h0123` to address `87` in  the `storage` SRAM
declared earlier, one can use:

```
  storage.write(10'd87, 16'h0123);
```

Given that the read data of an SRAM is registered, reading data from SRAMs
is slightly more involved. The `.read` method is provided the initiate a read
access. This has the following signature:

```
  void read(uint($clog2(<depth>)) addr);
```

Due to the registered output from the SRAM, when the read cycle is performed,
the read data is available on the following cycle, and can be consumed through
the `.rdata` property of the SRAM, which is of the same type as the SRAM.

A simple read from the `storage` SRAM could be performed like this:

```
  // Issue read access
  storage.read(10'd87);
  fence;
  // Given that storage was declared as combinatorially driven, stroage.rdata
  // is available on the following cycle. x should now be incremented by
  // whatever value is in the SRAM at address 87
  x += storage.rdata;
```

### Combinatorially driven vs registered SRAMs

The difference between combinatorially driven SRAMs and registered SRAMs is
important.

Combinatorially driven SRAMs have their inputs (that is the `ce`, `we`, `addr`
and `wdata` signals) potentially driven from combinatorial circuits based on
local entity state. Whether this is acceptable from a timing perspective is
dependent on the design and the physical SRAM implementation. The `.read` and
`.write` methods of combinatorially driven SRAMs drive the input signals of the
SRAM directly.

Inputs to registered SRAMs are driven from a set of local registers, and the
`.read` and `.write` methods write to these local registers. This means that
registered SRAMs have an additional cycle of latency before read and write
accesses take effect.

### Generated SRAM models and instances

To implement combinatorially driven SRAMs, the compiler does the following:
  1. Construct an SRAM model similar to the one describe previously, but with
     specific width and depth values. If an SRAM of the same shape already
     exist anywhere in the design, that model is resued.
  1. Instantiate the SRAM model just constructed in the entity containing the
     `sram` declaration.
  1. Translate interface methods and properties to the appropriate signal values

Generated SRAM models are emitted together with the compiled design. These can
be used for simulation purposes.

In addition to the above, the compiler does the following to implement SRAMs
driven by registers:
  1. Construct and instantiate a set of output registers, the same way as to
     what  registered output port with flow control type `sync` would translate
     into. These local registers holds the `we`, `addr`, and `wdata` values
     driven to the SRAM. Note that by default this does not add an extra output
     port to the enclosing entity.
  1. Connect the `valid` signal of the created output register to the SRAM
     instance `ce` signal, and connect the other signals from the output
     register to the corresponding SRAM instance ports.

### Lifting SRAMs

It is common practice that instead of instantiating SRAMs in the design
hierarchy, they are wired out through the top level to be instantiated in the
testbench. The Alogic compiler provides automation for this, through the use
of the `liftsrams` attribute. An entity annotated with the `liftsrams` attribute
will cause the compiler to automatically extract all SRAM instances from all
entities instantiated below such an annotated entity, and instead wire the SRAM
signals through ports automatically inserted. This will result in SRAM instances
being placed in the entities that are instantiating the entities highest in the
design hierarchy, which have the `liftsrams` attribute.

### A simple SRAM based FIFO example

To demonstrate the use of SRAMs, an example implementation of a simple but
generic FIFO, using a single SRAM as the backing store is provided:

```
(* liftsrams *)
fsm sfifo {
  param u32 WIDTH = 8;
  param u32 DEPTH = 2;

  // Number of address bits
  const u32 ABITS = $clog2(DEPTH);

  // The input port
  in  sync ready               uint(WIDTH) p_in;
  // The output port
  out sync ready bslice fslice uint(WIDTH) p_out;

  // The full/empty status bits
  bool full = false;
  bool empty = true;

  // The read and write pointers
  uint(ABITS) rdptr = @zx(ABITS, 1'b0);
  uint(ABITS) wrptr = @zx(ABITS, 1'b0);

  // The highers valid poitner
  const u32 MAXPTR = DEPTH - 1;

  // The actual storage SRAM
  sram wire uint(WIDTH) storage[DEPTH];

  // Give priority to reads
  const bool RDPRI = true;

  // Marker noting a new datum is available on storage.rdata
  bool rdavail = false;

  fence {
    // If read data is available, write it to the output port
    if (rdavail) {
      p_out.write(storage.rdata);
    }
  }

  void main() {
    // Check whether we can read or write this cycle
    bool can_rd = !p_out.full && !empty;
    bool can_wr = p_in.valid && !full;

    // Arbitrate between read and write
    bool rd = can_rd && ( RDPRI || !can_wr);
    bool wr = can_wr && (!RDPRI || !can_rd);

    // Do the read if required
    if (rd) {
      storage.read(rdptr);
      rdptr = rdptr == MAXPTR[0 +: ABITS] ? @zx(ABITS, 1'b0)
                                          : rdptr + @zx(ABITS, 1'b1);
    }

    // Do the write if required
    if (wr) {
      storage.write(wrptr, p_in.read());
      wrptr = wrptr == MAXPTR[0 +: ABITS] ? @zx(ABITS, 1'b0)
                                          : wrptr + @zx(ABITS, 1'b1);
    }

    // If we just did a read, mark there will be
    // a read datum available on the next cycle
    rdavail = rd;

    // Check the pointers are equal
    bool eq = rdptr == wrptr;

    // full/empty only change if reading or writing on a cycle but not both
    if (rd ^ wr) {
      full = wr & eq;
      empty = rd & eq;
    }

    fence;
  }
}
```

A not very thorough but simple test-bench for the FIFO above can be written as:

```
network sfifo_tb {

  const u32 WIDTH = 8;
  const u32 DEPTH = 32;

  dut = new sfifo(WIDTH=WIDTH, DEPTH=DEPTH);

  // FSM to feed the FIFO input with some random data
  new fsm feed {
    out sync ready uint(WIDTH) item;

    void main() {
      for (u32 i = 0 ; i < 10000 ; i++) {
        item.write(i[0 +: WIDTH]);
      }

      loop { fence; }
    }
  }

  // Checker to ensure the expected data is coming out the FIFO
  new fsm check {
    in sync ready uint(WIDTH) item;

    void main() {
      for (u32 i = 0 ; i < 10000 ; i++) {
        item.read();
        if (item.valid && item != i[0 +: WIDTH]) {
          $display("@@@FAIL");
          $finish();
        }
      }

      $display("@@@PASS");
      $finish();
      fence;
    }
  }

  feed.item -> dut.p_in;
  dut.p_out -> check.item;
}
```

Given that the FIFO entity has the `liftsrams` attribute, it will not actually
contain an SRAM instance. If the Alogic compiler is invoked on the FIFO itself,
the instances of the backing store SRAM will be dropped, as they will be lifted
past the top level module. In this case, the integrator of the FIFO will have
to instantiate and connect the appropriate SRAM model directly. If the Alogic
compiler is invoked on the testbench, the SRAM instances will be lifted past
the FIFO, and put down in the testbench itself, as the testbench is the lowest
entity in the hierarchy that does not have the `liftsrams` attribute, nor is it
instantiated by any entities with the `liftsrams` attribute.

<p align="center">
<a href="memories.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="builtins.md">Next</a>
</p>
