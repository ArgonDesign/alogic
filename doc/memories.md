# Distributed memories

Alogic has native language support for working with distributed memories. Alogic
distributed memories support immediate, combinatorial reading of an arbitrary
number of entries, and sequential writing of a single entry in a single clock
cycle.

### Declaring distributed memories

Distributed memories are declared using an array style syntax:

```
<type> <identifier>[<depth>];
```

The _\<type>_ of a memory determines the type of its elements, and therefore the
width, of the memory. _\<type>_ must be an integer type. The _\<depth>_ of the
memory must be a constant expression.

A 32 entry memory of 16 bit unsigned integers called `mem` could be declared as:

```
u16 storage[32];
```

### Working with distributed memories

To get the value at a memory location, use the indexing syntax:

```
  u16 sum = storage[1] + storage[0];
```

Reading a distributed memory by indexing is a combinatorial operation and the
index expression evaluates to the value stored at the addressed location. There
is no limit imposed by Alogic on the number of look-ups that can be performed
simultaneously. The designer however should keep in mind the implications of
multiple parallel accesses in terms of circuit timing and area.

Distributed memories can be updated (written) one entry per cycle. Writing to a
distributed memory is performed using the `.write` method, which has the
following signature:

```
  void write(uint($clog2(<depth>)) addr, <type> data);
```

Writing the value `16'habcd` to address `9` in the distributed memory called
`storage` declared earlier, is performed with:

```
  storage.write(9, 16'habcd);
```

Writes take effect only on the subsequent clock cycle:

```
  storage.write(9, 16'h0123);
  fence;
  // storage[9] is now 16'h0123

  storage.write(9, 16'habcd); // The write only commits at the end of the cycle

  u16 x = storage[9]; // 'x' becomes 16'h0123
  fence;

  u16 y = storage[9]; // 'y' becomes 16'habcd
  fence;
```

### Implementation of distributed memories

In the Verilog implementation emitted by the compiler, distributed memories are
implemented using unpacked arrays. Direct indexing is used for look-ups, but
additional write enable (`we`), write data (`wdata`) and write address (`waddr`)
signals are used to handle writes. The `storage` distributed memory declared
earlier would be implemented in the generated Verilog as:

```verilog
  // Memory
  reg [15:0] storage_q [31:0];

  // Write bus
  reg storage_we;
  reg [4:0] storage_waddr;
  reg [15:0] storage_wdata;

  // Memory update
  always @(posedge clk) begin
    if (storage_we) begin
      storage_q[storage_waddr] <= storage_wdata;
    end
  end
```

### A simple distributed memory based FIFO example

As an example, a simple 32 entry deep, 8-bit wide distributed memory backed fifo
could be written in Alogic as:

```
fsm dfifo {
  in  sync ready               u8 p_in;
  out sync ready bslice fslice u8 p_out;

  // The full/empty status bits
  bool full = false;
  bool empty = true;

  // The backing distributed memory
  u8 storage[32];

  // The read and write pointers
  u5 rdptr = 5'd0;
  u5 wrptr = 5'd0;

  void main() {
    // Check whether we can read or write this cycle
    bool rd = !p_out.full && !empty;
    bool wr = p_in.valid && !full;

    // Do the read if required
    if (rd) {
      p_out.write(storage[rdptr]);
      rdptr++;
    }

    // Do the write if required
    if (wr) {
      storage.write(wrptr, p_in.read());
      wrptr++;
    }

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
