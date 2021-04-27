<p align="center">
<a href="assert.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="gen.md">Next</a>
</p>

# Built-in functions

Alogic provides a set of built-in functions, this section is a comprehensive
reference of their semantics.

### General semantics and concepts

Names of Alogic built-in functions start with either the `$` or `@` characters.

Built-in functions starting with a `$` are borrowed from the Verilog language,
and are intended to behave as close to the corresponding Verilog function as
possible. Any relevant differences from the Verilog semantics of these function
are described below. In particular, function calls must be written with Alogic
syntax and a pair of empty `()` is required when calling function with no
arguments.

Built-in functions whose name starts with an `@` are provided by Alogic and
their semantics are defined below.

All built-in function calls are combinational statements when used in statement
position.

Some built-in functions are overloaded, i.e.: they are available with multiple
signatures, or have a generic signature.

### List of `$` built-in functions

#### Built-in `$clog2`

Signature:

```
uint $clog2(arg);
```

Returns the number of address bits required for a memory of the depth given by
the argument. The argument (`arg`) must be a constant expression.

#### Built-in `$signed`

Signature:

```
int(N) $signed(expr);
```

Cast argument to a signed integer. The result is the same width as the argument.
The argument can have any packed type.

#### Built-in `$unsigned`

Signature:

```
uint(N) $unsigned(expr);
```

Cast argument to an unsigned integer. The result is the same width as the
argument. The argument can have any packed type.

### List of `@` built-in functions

#### Built-in `@bits`

Signatures:

```
uint @bits(type);
uint @bits(expr);
```

`@bits` can be invoked on any packed type, or an expression of packed type, and
returns the width (in bits) of the argument. `@bits` is always evaluated at
compile time.

##### Built-in `@msb`

Signature:

```
bool @msb(expr);
```

Evaluates to the MSB of the expression. Can only be called on expressions to
which a `[ ]` index can be applied.

#### Built-in `@ex`

Signatures:

```
uint(N) @ex(bit, N, expr);
 int(N) @ex(bit, N, expr);
```

Extend the third argument to the width specified by the second argument,
inserting the bit value given as the first argument into the MSBs of the result.
The second argument must be a constant expression. The third argument can be any
packed type with a width no greater than N. The result is unsigned if the third
argument is unsigned, and the result is signed if the third argument is signed.
For example:

```
u5 a = 5'b100;
u10 b = @ex(1'b1,10,a); // b = 11111 00100
```

##### Built-in `@zx`

Signatures:

```
uint(N) @zx(N, expr);
 int(N) @zx(N, expr);
```

Zero-extend the second argument to the width specified by the first argument.
The first argument must be a constant expression. The second argument can be any
packed type with a width no greater than N. The result is unsigned if the second
argument is unsigned, and the result is signed if the second argument is signed.

`@zx(N, a)` is the same as `@ex(1'b0, N, a)`

##### Built-in `@sx`

Signatures:

```
uint(N) @sx(N, expr);
 int(N) @sx(N, expr);
```

Sign extend the second argument to the width specified by the first argument.
The first argument must be a constant expression. The second argument can be any
packed type with a width no greater than N. The result is unsigned if the second
argument is unsigned, and the result is signed if the second argument is signed.

Sign extending an unsigned value is defined as extension by the MSB.

`@sx` can only be called on arguments for which `@msb` is defined.

`@sx(N, a)` is the same as `@ex(@msb(a), N, a)`

<p align="center">
<a href="assert.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="gen.md">Next</a>
</p>
