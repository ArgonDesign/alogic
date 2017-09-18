
### Entity Parameters

Entities can declare typed parameters, introduced with the `param` keyword,
which can be specified at instantiation time. Parameters must have a default
value:

```
fsm foo {
  param u8 PARAM_BYTE = 8h7f;
}
```

Port declarations can depend on parameters:

```
fsm bar {
  param i32 WIDTH = 8;
  in sync uint(WIDTH) p_in;
  out sync uint(2*WIDTH) p_out;
  ...
}
```

Parameters translate directly into Verilog parameters, so the above definition
of `fsm bar` would compile into a Verilog module with the following interface:

```verilog
module bar #(
  parameter signed [31:0] WIDTH = 8
) (
  input wire clk,
  input wire rst_n,

  input wire [WIDTH-1:0] p_in,
  input wire p_in_valid,

  output wire [(2*WIDTH)-1:0] p_out,
  output wire p_out_valid
);
 ...
endmodule
```

### Constants

Constants are local aliases for values introduced with the `const` keyword. They
are stronger than text macros by having an associated type and hence bit width.
They translate directly into Verilog _localparam_ constructs. Port declarations
cannot depend on constants.

```
fsm baz {
  const u32 MAGIC = 32'habcd_0123;
	...
}
```

Would translate into:

```verilog
module baz (
  input wire clk,
  input wire rst_n,
  ...
);
  localparam [31:0] MAGIC = 32'habcd0123;
  ...
endmodule
```
