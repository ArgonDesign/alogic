<p align="center">
<a href="ports.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="fsms.md">Next</a>
</p>

# Constants and Parameters

### Constants

Constants are local aliases for values introduced with the `const` keyword. They
are stronger than text macros by having an associated type and hence bit width:

```
fsm baz {
  const u32 MAGIC = 32'habcd_0123;
	...
}
```

Constants are emitted in the generated Verilog as _localparam_ declarations:

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

Note however, that even though `const` declarations may be emitted in the target
language, the compiler will inline the value of `const` declarations where it
deems necessary, especially in declarations of signals.

### Entity Parameters

Entities can declare typed parameters, introduced with the `param` keyword.
These parameters must have a default value. However, it is then possible to
overwrite this at instantiation time by defining an instantiated module to have
a specific parameter value:

```
fsm foo {
  param u8 MARKER = 8'd10;
}

network bar {
  foo_i = new foo(MARKER = 8'd247);
  // creates an instance of foo which overwrites the default value of MARKER.
}
```

In this way, Alogic performs parameter specialization, meaning the compiler will emit
specific implementations of a parametrized module, based on the particular
parameter values it is instantiated with. This means that Verilog modules output
by the Alogic compiler will never contain _parameter_ declarations. Specialized
parameters are emitted as _localparam_ declarations in the output Verilog. For
the above example, the compiler would emit the following specialization of
entity `foo`:

```verilog
module foo__MARKER_247 (
  input wire clk,
  input wire rst_n,
  ...
);
  localparam [7:0] MARKER = 8'd247;
  ...
endmodule
```

One benefit of parameter specialization is that as opposed to Verilog, port
declarations in Alogic can depend on `const` values:

```
fsm bar {
  param u32 WIDTH_L2 = 8;
  const u32 WIDTH = 1<<WIDTH_L2;
  in sync uint(WIDTH) p_in;
  out sync uint(2*WIDTH) p_out;
  ...
}
```

Further benefits of parameter specialization include the possibility of further
compile time optimization of the specialized entities and elimination of Verilog
instance based code coverage holes arising from use of constant parameter values
in expressions.

<p align="center">
<a href="ports.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="fsms.md">Next</a>
</p>
