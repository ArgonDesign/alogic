<p align="center">
<a href="gen.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="preproc.md">Next</a>
</p>

# Interoperability with low level HDLs

The aim of Alogic is not to be a replacement for a lower level HDL. There are
important design opportunities which require the precise control and
lower abstraction provided by a low level HDL such as Verilog. Alogic is
designed to be fully and easily interoperable with entities written natively in
Verilog.

Interoperability is achieved through two mechanisms:

1. There is a well defined translation of interface signals from Alogic to the
target language. This allows Alogic generated modules to be easily instantiated
in the target language.

1. Alogic provides syntax to include arbitrary target-language source text in
the generated output. Therefore, the implementation of an Alogic entity can be
written entirely in the target language, relying on the Alogic compiler only to
generate the interface signals. Alternatively, an Alogic wrapper can be created
around a module written in the target language, by simply instantiating the
native module in the Alogic wrapper, again relying on the Alogic compiler to
generate the interface signals.

These interoperability mechanisms are described one at a time in this chapter.

### Translation of interface signals from Alogic to Verilog

As described in the documentation of [parameters](params.md), Alogic performs
parameter specialization, and as a result will never emit a Verilog module which
has a parametrized interface. Therefore the only interface mapping that needs to
be considered is input and output ports between Alogic and the target language.

#### Alogic ports with non-`struct` type

Any interface port of a non-`struct` type will be emitted as an interface
signal with a packed width equal to the width of the Alogic port. Ports of width
1 are emitted as a plain signal. The name of the interface signal in the
compiled output is the same as the name of the port in the Alogic source code,
as demonstrated by the following:

<a href="http://afiddle.argondesign.com/?example=interop_nonstruct.alogic">Fiddle with this code here.</a>

```
fsm foo {
  in u128 data;
  in u32  addr;
  in bool last;

  out u64  item;
  out bool first;

  ...
}
```

This translates into a Verilog module with the following interface:

```verilog
module foo(
  input  wire         clk,
  input  wire         rst_n,

  input  wire [127:0] data,
  input  wire [31:0]  addr,
  input  wire         last,

  output wire [63:0]  item,
  output wire         first
);
  ...
endmodule
```

Note that generated Verilog `output` signals can be emitted either as Verilog
output nets (`output wire`) or Verilog output variables (`output reg`),
depending on whether they are driven from a continuous assignment, or a
procedural assignment. The Alogic compiler takes care of deriving the required
form.

The clock and reset signals are omitted from output modules that are purely
combinatorial.

The storage specifier of output ports has no effect on the interface of the
emitted output.

#### Ports with flow control

Alogic ports with `sync` flow control have an associated _valid_ signal, and
Alogic ports with `sync ready` flow control have associated _valid_ and _ready_
signals. These signals are added by the Alogic compiler, as shown in the
following example:

<a href="http://afiddle.argondesign.com/?example=interop_ports.alogic">Fiddle with this code here.</a>

```
fsm foo {
  in sync       bool a;
  in sync ready u128 b;

  out sync       u2   c;
  out sync ready bool d;

  ...
}
```

This translates into:

```verilog
module foo(
  input  wire         clk,
  input  wire         rst_n,

  input  wire         a,
  input  wire         a__valid,

  input  wire [127:0] b,
  input  wire         b__valid,
  output wire         b__ready,

  output wire [1:0]   c,
  output wire         c__valid,

  output wire         d,
  output wire         d__valid,
  input  wire         d__ready
);
  ...
endmodule
```

The separator used to join parts of signal names constructed from multiple parts
can be provided explicitly by the `--sep` compiler option, which defaults to
`__`. To use single `_` separators, invoke the compiler with `--sep _`. This
would yield the following for the previous example:

```verilog
module foo(
  input  wire         clk,
  input  wire         rst_n,

  input  wire         a,
  input  wire         a_valid,

  input  wire [127:0] b,
  input  wire         b_valid,
  output wire         b_ready,

  output wire [1:0]   c,
  output wire         c_valid,

  output wire         d,
  output wire         d_valid,
  input  wire         d_ready
);
  ...
endmodule
```

Alogic ports with flow control, but with `void` type, have no payload signals,
and generate only _valid_ and _ready_ signals (<a href="http://afiddle.argondesign.com/?example=interop_void.alogic">fiddle here</a>):

```
fsm foo {
  in sync       void a;
  in sync ready void b;

  out sync       void c;
  out sync ready void d;

  ...
}
```

This yields:

```verilog
module foo(
  input  wire clk,
  input  wire rst_n,

  input  wire a__valid,

  input  wire b__valid,
  output wire b__ready,

  output wire c__valid,

  output wire d__valid,
  input  wire d__ready
);
  ...
endmodule
```

#### Ports with `struct` type

Ports (and in general signals) which are of a `struct` type are emitted as
multiple signals by splitting the structure into its fields. The separator used
to join the parts of the split signal names is given by the `--sep` compiler
option, and defaults to `__`, the same way as for flow control signals described
above. Observe the example (<a href="http://afiddle.argondesign.com/?example=interop_struct.alogic">fiddle here</a>):

```
struct point_t {
  u10 x;
  u10 y;
}

struct req_t {
  u32 addr;
  u3  len;
  u4  prop;
}

fsm foo {
  in sync point_t p;

  out sync ready req_t r;

  ...
}
```

Which turns into:

```verilog
module foo(
  input  wire         clk,
  input  wire         rst_n,

  input  wire [9:0]   p__x,
  input  wire [9:0]   p__y,
  input  wire         p__valid,

  output wire [31:0]  r__addr,
  output wire [2:0]   r__len,
  output wire [3:0]   r__prop,
  output wire         r__valid,
  input  wire         r__ready
);
  ...
endmodule
```

Nested structures are split recursively as demonstrated by the following (<a href="http://afiddle.argondesign.com/?example=interop_nested.alogic">fiddle here</a>):

```
struct point_t {
  u10 x;
  u10 y;
}

struct rect_t {
  point_t top_left;
  point_t bottom_right;
}

fsm foo {
  in sync rect_t r;

  ...
}
```

This results in the following Verilog interface:

```verilog
module foo(
  input  wire       clk,
  input  wire       rst_n,
  input  wire [9:0] r__top_left__x,
  input  wire [9:0] r__top_left__y,
  input  wire [9:0] r__bottom_right__x,
  input  wire [9:0] r__bottom_right__y,
  input  wire       r__valid
);
  ..
endmodule
```

### Verbatim blocks

Target language source text can be included in the output generated by the
compiler using a `verbatim` block. The `verbatim` block is followed by the name
of the target language (currently only `verilog` is supported), and the body of
the block is copied verbatim at the end of the generated output module.

<a href="http://afiddle.argondesign.com/?example=interop_verbatim.alogic">Fiddle with this verbatim block here.</a>

```
fsm v {
  void main() {
    ...
  }

  verbatim verilog {
    // Tick away
    always @(posedge clk) begin
      $display("tick");
    end
  }
}
```

The Verilog module output for the above would end as:

```verilog
  ...

    // Tick away
    always @(posedge clk) begin
      $display("tick");
    end

endmodule
```

Verbatim blocks can be used in any entity. The contents of multiple `verbatim`
blocks are concatenated as if they were written in a single `verbatim` block.

### Verbatim entities

A special entity type is available for writing entire Alogic entities in the
target language. This is declared using the `verbatim entity` keywords, and can
only contain the following contents:

A list of:

- `in` port declarations
- `out` port declarations
- `param` declarations
- `const` declarations
- `sram wire` declarations

Followed by an arbitrary number of `verbatim` blocks. For example:

```
verbatim entity foo {
  <declarations>

  verbatim verilog {
    <contents>
  }
}
```

The port declarations define the interface of the output Verilog module in the
usual way. Output ports have no associated storage elements.

`param` and `const` declarations are emitted as Verilog `localparam`
declarations after undergoing parameter specialization.

SRAM declarations in verbatim entities must use the `wire` qualifier. The
compiler automatically sets the `liftsrams` attribute on verbatim entities,
resulting in standard SRAM ports being added in the output Verilog. These SRAM
ports can then be referenced in verbatim blocks.

Verbatim entities can be instantiated in other Alogic entities without
restrictions.

#### Alogic entity implemented in Verilog

One use of verbatim entities is to implement Alogic entities, whose behaviour is
more easily described using the target language. A good example is a purely
combinatorial arbiter (<a href="http://afiddle.argondesign.com/?example=interop_verilog.alogic">fiddle here</a>):

```
verbatim entity arb {
  in  sync ready u8 i_0;
  in  sync ready u8 i_1;
  out sync ready u8 o;

  verbatim verilog {
    // i_0 has priority
    assign o = i_0__valid ? i_0 : i_1;
    assign o__valid = i_0__valid | i_1__valid;
    assign i_0__ready = o__ready;
    assign i_1__ready = o__ready & ~i_0__valid;
  }
}
```

This compiles into the following Verilog module:

```verilog
module arb(
  input  wire       clk,
  input  wire       rst_n,

  input  wire [7:0] i_0,
  input  wire       i_0__valid,
  output wire       i_0__ready,

  input  wire [7:0] i_1,
  input  wire       i_1__valid,
  output wire       i_1__ready,

  output wire [7:0] o,
  output wire       o__valid,
  input  wire       o__ready
);

    // i_0 has priority
    assign o = i_0__valid ? i_0 : i_1;
    assign o__valid = i_0__valid | i_1__valid;
    assign i_0__ready = o__ready;
    assign i_1__ready = o__ready & ~i_0__valid;

endmodule
```

Note that even though the output module is purely combinatorial, the Alogic
compiler still provided the clock and reset signals, as it has no understanding
of the contents of `verbatim` blocks.

For the same reason, it is important to note that in verbatim entities, all
generated Verilog module output signals are always declared as `output wire`,
and never as `output reg`. Consider this while writing the implementation.

#### Alogic wrapper for native Verilog modules

The other major use of verbatim entities is to write an Alogic wrapper around a
Verilog module, so that it can be instantiated in other Alogic entities. This is
done by defining the interface ports of the verbatim entity as Alogic ports, and
then instantiating the subject Verilog module in a `verbatim` block, connecting
the interface signals as appropriate. Imagine we want to instantiate the
following simple Verilog module in an Alogic module:

```verilog
module inc(
  input  wire [7:0] i;
  output wire [7:0] o;
);

  assign o = i + 8'd1;

endmodule
```

<a href="http://afiddle.argondesign.com/?example=interop_wrapper.alogic">Fiddle with an Alogic wrapper here.</a>
 
We can write an Alogic wrapper as:

```
verbatim entity inc_wrap {
  in  u8 ii;
  out u8 oo;

  verbatim verilog {
    // Instantiate the subject module and connect it
    inc inc_i(.i(ii), .o(oo));
  }
}
```

And then we can instantiate the wrapper as a normal Alogic entity:

```
network inc2 {
  in  u8 i;
  out u8 o;

  inc_0 = new inc_wrap;
  inc_1 = new inc_wrap;

  i -> inc_0.ii;
  inc_0.oo -> inc_1.ii;
  inc_1.oo -> o;
}
```

The example is deliberately simple, but the wrapped module can be arbitrarily
complex; its interface signals simply need to be mapped to Alogic ports by the
designer.

<p align="center">
<a href="gen.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="preproc.md">Next</a>
</p>
