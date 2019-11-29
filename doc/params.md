<p align="center">
<a href="ports.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="fsms.md">Next</a>
</p>

# Constants and Parameters

#### Constants

Constants are local aliases for values introduced with the `const`
keyword. Constants can be declared with any packed type, or with any of
the 2 unsized integer types. As `const` values are immutable, their
declarations must have an initializer:

<a href="http://afiddle.argondesign.com/?example=params_constants.alogic">Fiddle with this code here.</a>

```
fsm baz {
  const uint MAGIC = 0x0123_abcd;
  const u32 MAGIC_PACKED = 32'habcd_0123;
	...
}
```

Constants with a packed type are emitted in the generated Verilog as
_localparam_ declarations:

```verilog
module baz (
  input wire clk,
  input wire rst_n,
  ...
);
  localparam [31:0] MAGIC_PACKED = 32'habcd0123;
  ...
endmodule
```

Note however, that even though `const` declarations may be emitted in
the target language, the compiler will inline the value of `const`
declarations where it deems necessary, especially in declarations of
signals.

#### Entity Parameters

Entities can declare typed parameters, introduced with the `param`
keyword. Parameters are similar to constants, but can be provided with
an alternative value at instantiation time. Parameter declarations can
have a default value if appropriate, or can be left without an
initializer. Actual parameter values can be specified at instantiation
time, which override any default parameter values. Parameters without a
default value must be provided at instantiation time:

```
fsm foo {
  param u7 A = 7'd10;
  param u8 B = 8'd11;
  param u9 C;
}

network bar {
  foo_i = new foo(A = 7'd107, C = 9'dff);
  // Creates an instance of foo which overwrites the default value of A,
  // but not B. The value of C is required as the declaration of C in
  // foo has no default value.
}
```

The top level module cannot have parameters without a default value.

#### Parameter specialization

Alogic performs parameter specialization, meaning the compiler will emit
specific implementations of a parametrized entity, based on the
particular parameter values it is instantiated with. This means that
Verilog modules output by the Alogic compiler will never contain
_parameter_ declarations. Specialized parameters with a packed type are
emitted as _localparam_ declarations in the output Verilog. For the
above example, the compiler would emit the following specialization of
entity `foo`:

```verilog
module foo__MARKER_107__B_11__C_255 (
  input wire clk,
  input wire rst_n,
  ...
);
  localparam [6:0] A = 7'd107;
  localparam [7:0] B = 8'd11;
  localparam [9:0] C = 9'd255;
  ...
endmodule
```

One benefit of parameter specialization is that as opposed to Verilog,
port declarations in Alogic can depend on `const` values:

<a href="http://afiddle.argondesign.com/?example=params_port_declarations.alogic">Fiddle with this code here.</a>

```
fsm bar {
  param uint WIDTH_L2 = 8;
  const uint WIDTH = 1<<WIDTH_L2;
  in sync uint(WIDTH) p_in;
  out sync uint(2*WIDTH) p_out;
  ...
}
```

Further benefits of parameter specialization include the possibility of
further compile time optimization of the specialized entities and
elimination of Verilog instance based code coverage holes arising from
use of constant parameter values in expressions.

#### Parameter and constant dependencies

Parameter and constant declarations can depend on each other in an
arbitrary way subject to declaration of names being provided before uses
of the name. Parameters and constants however cannot depend on any other
name declared inside the same entity, in particular this is
valid:

```
fsm dependent_param {
  param uint A;
  param uint B;
  const uint C = B - A;
  param uint(C) D;
  param uint(@bits(D)) E;

  in uint(C) port;
  ...
}
```

but this is not:

```
fsm bad_param {
  param uint A;
  param uint B;

  in uint(B - A) port; // Neither `param` nor `const`

  const uint C = @bits(port); // Error: depends on 'port'
  param uint(C) D;            // Error: depends on 'port' indirectly
  param uint(@bits(port)) E;  // Error: depends on 'port'
  ...
}
```

<p align="center">
<a href="ports.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="fsms.md">Next</a>
</p>
