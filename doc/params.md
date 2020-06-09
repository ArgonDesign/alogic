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
declarations wherever possible.

#### Parameterized definitions

Entities and structures can declare typed parameters, introduced with the
`param` keyword. Parameters are similar to constants, but can be provided
with an alternative value at instantiation time. Parameter declarations can
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

Structure parameters are provided similarly:

```
struct s {
  param uint W;
  uint(W) a;
  uint(W) b;
}

fsm bar {
  in s(W=10) i;
  ...
}
```

In addition to value parameters, Alogic also implements type parameters, which
are declared using the `type` keyword instead of the type specifier expression
in the parameter declaration. Type parameters can be used to implement generic
components:

```
fsm fifo {
  param type T;
  in  T i;
  out T o;
  storage T[10];
  ...
}
```

#### Parameter specialization

Alogic performs parameter specialization, meaning the compiler will emit
specific implementations of a parametrized entity, based on the
particular parameter values it is instantiated with. This means that
Verilog modules output by the Alogic compiler will never contain
_parameter_ declarations. Specialized parameters with a packed type are
emitted as _localparam_ declarations in the output Verilog if required.
For the above example, the compiler would emit the following specialization
of entity `foo`:

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

#### Actual parameter assignment

Wherever a parametrized type (e.g.: a parametrized entity or struct) is
used, it must be provided with an actual parameter list, even if use of
the default parameter values is desired. Parameter values usually need to
be given by name. The one exception to this requirement is if a type has
only a single declared parameter, then it can be provided as a single
positional value:

```
struct a_t {
  param uint X;
  ...
}

struct b_t {
  param uint Y = 0;
  ...
}

struct c_t {
  param uint Z;
  param uint W = 0;
}

a_t(X=1) a0; // OK: declares 'a0' with type 'a_t' with 'X' set to 1
a_t      a1; // Error: 'a_t' requires a parameter list
a_t()    a2; // Error: 'a_t' requires value for parameter 'X' with no default
a_t(2)   a3; // OK: declares 'a3' with type 'a_t' with 'X' set to 2


b_t(Y=1) b0; // OK: value of 'Y' specified explicitly
b_t()    b2; // OK: declares 'b2' with type 'b_t' with default value 0 for 'Y'
b_t(2)   b3; // OK: value of 'Y' specified explicitly

c_t(Z=1, W=0) c0; // OK: both parameters specified explicitly
c_t(Z=1)      c1; // OK: 'W' uses default value
c_t()         c2; // Error: 'Z' has no default value
c_t(2)        c3; // Error: 'c_t' has more than one parameters which requires
                  // named parameter assignment, even if some of them have
                  // default values
```

Note that parametrized types are not restricted to declarations, they are
accepted wherever a non-parametrized type is accepted as in the following
example:

```
struct t {
  param uint W;
  uint(W) a;
  uint(2*W) b;
}

const c = @bits(t(10)); // 'c' has the value 30
```

This also means that a specific parametrization of a type can be given
a name itself with typedef:

```
typedef t(10) t_10;

const c = @bits(t_10); // Same as above

t_10 a_t_10; // Declares 'a_t_10' with type t(W=10)
```

#### Parameterized top level entities

Top level entities can have parameters. Actual parameter values for top
level entities can be specified on the compiler command line with the
same call-style syntax as in the Alogic language:

```
fsm top {
  param uint P;
  param uint Q = 4;
  ...
}
```

```
alogic -o out 'top(P=1, Q=2)' 'top(P=3)'
```

The above compiler invocation will generate two specializations of entity
'top', both of which will be emitted in the output directory 'out'. Note
that the quiting is required by the shell to pass the expression as a
single argument to the alogic compiler. The top-level specifier expressions
are fully fledged Alogic expression as they would appear in an instance
definition in the Alogic language.

#### Parameter and constant dependencies

Parameter and constant declarations can depend on each other in an
arbitrary way. Parameters and constants however cannot depend on any other
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
