<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="widths.md">Next</a>
</p>

# Data types and simple variables

Every identifier and expression (also called a value or term) in Alogic
has a well defined type.
 
The type of a variable is determined by the declaration of that
variable. Variables are declared in the usual style, using a type
specifier denoting the type of the variable followed by the name of the
variable:

```
  foo_t bar; // Declaration of a variable called 'bar' with type 'foo_t'
``` 

The result types of expressions are determined by a set of typing rules
presented later (TODO).

#### Packed types
 
Values which can exist in the generated design must be of a packed type.
Packed types are simply those that are represented by a finite linear
sequence of bits. For example, a 6-bit number would have a packed type,
and so would a structure holding two 10-bit numbers, but a memory or a
function identifier would have non-packed (or unpacked) types. The
number of bits in the binary representation of a packed type is called
the *width* of the type, and can be retrieved using the `@bits`
[built-in function](builtins.md). Apart from width, packed types also
have a *signedness* which can be either *signed* or *unsigned*.

Similarly to Verilog, Alogic is a weakly typed language in that any
variable of a packed type can be assigned a value of another packed
type, so long as the width of the two types are the same.

#### Sized integer types - <a href="http://afiddle.argondesign.com/?example=types_sized_integers.alogic">fiddle here.</a>

The fundamental packed types in Alogic are signed or unsigned sized
integers, introduced with the `int(N)` or `uint(N)` type specifiers. The
argument `N` of the type specifier must be a compile time constant but
otherwise arbitrary expression, and determines the width of the sized
integer:

```
  int(5)    a; // A 5 bit signed integer
  uint(8)   b; // An 8 bit unsigned integer
  uint(A+2) c; // Unsigned integer, with width depending on parameter A.
```

Since integer types of varying length are the norm in digital design,
there is a shorthand for writing them when the width is known up front.
A keyword starting with the letter `u` followed by the number of bits
can be used to denote an unsigned sized integer with that width.
Similarly, a keysord starting with the letter `i` can be used for signed
sized integers.

```
  i5 a; // Same as 'int(5) a'
  u8 b; // Same as 'uint(8) b'
```

The canonical parenthesized format is only advised if the width of the
type is computed based on values of parameters or constants. Prefer the
shorthand whenever possible.

#### Unsized integer types

Two notable non-packed types in Alogic are those of unsized integers.
Members of these types are infinite precision integers, and are
differentiated as signed or unsigned. Signed and unsigned unsized
integers are declared with the type specifiers `int` and `uint`
respectively, with no width argument. All variables and expressions of
an unsized integer type must have values that can be computed during
compilation time. This means that for example
[parameters and constants](params.md) can be declared with an unsized
integer type, but arbitrary design variables may not:

```
  param uint A = 1; // OK: parameter values are known at compilation time
  const int B = 2;  // OK: similar to parameters
  uint C = 3;       // Error: C can be re-assigned to values determined at run-time
```

The Alogic compiler computes the values of all expressions of an unsized
integer type at compilation time. In any context where a packed value is
required, but a value of an unsized integer type is provided, the
compiler will infer the width of the value and will either substitute a
sized integer of the same signedness as the original unsized type, if
the value can be represented by the inferred sized integer type, or
issue an error otherwise.

The contexts where width inference of unsized integer values is
performed are listed below. The inferred widths of sub expressions are
those that satisfy the [expression width constrains](widths.md) of the
language:

| Context | Example | Inferred width |
|---------|---------|----------------|
| Strict width binary operators\[\*\] with one sized and one unsized operand | `x + 1` | width of sized operand |
| Ternary operator with one sized and one unsized operand in the branches | `c ? x : 1` | width of sized branch operand |
| Index expressions | `x[1]` | max($clog2(size of indexed dimension), 1) |
| Slice position expressions | `x[1:0]` | max($clog2(size of sliced dimension), 1) |
| Slice width expressions | `x[i+:1]` | $clog2(size of sliced dimension + 1) |
| Function arguments with a packed formal type | `f(1)` | width of formal argument |
| Instance parameter assigment to packed parameter | `inst = new foo(A=1)` | width of parameter | 
| Initializer expression in declaration of packed variable | `u8 x = 1` | width of declared variable | 
| Assignment to packed right hand side | `x = 1` | widht of target of assignment |
| Operator assignment with strict width operators\[\*\] | `x += 1` | width of target of assignment | 

\[\*\]: Strict width binary operators are the arithmetic (`*`, `/`, `%`,
`+`, `-`), bitwise logical (`&`, `|`, `^`) and comparison (`>`, `>=`,
`<`, `<=`, `==`, `!=`) operators.

#### Bool type

The keyword `bool` can also be used to represent a boolean type, and is
simply a synonym for `u1`.

#### Struct types - <a href="http://afiddle.argondesign.com/?example=types_struct.alogic">fiddle here.</a>

Alogic supports grouping related values into structures. A structure
type is defined with the `struct` keyword, followed by the name of the
structure type, and the structure fields in curly braces:

```
struct point_t {
  u16 x;
  u16 y;
}

struct rect_t {
  point_t topleft;
  u8 width;
  u8 height;
}
```

Note that the above definitions define *types*  (with the names `point_t` and
`rect_t`) respectively, and not variables. The analogous C language statements
would be:

```C
typedef struct {
  uint16_t x;
  uint16_t y;
} point_t;

typedef struct {
  point_t topleft;
  uint8_t width;
  uint8_t height;
} rect_t;
```

To declare a simple variable with a structure type one would further write:

```
  rect_t rect; // A variable called 'rect' with type 'rect_t'
```

Fields of a structure type variable are accessed with the usual dot notation:

```
  rect.topleft = some_point; // Set 'rect.topleft' to the value of 'some_point'
  rect.width = 8'd9; // Set 'rect.width' to 9
  rect.height = 8'd1; // Set 'rect.height' to 1
```

All structures are packed, and the first field corresponds to the most
significant bits, and the last field to the least significant bits, so the
above example could have been written as follows using the bit concatenation
expression and sized constants:

```
  rect_t rect = {some_point.x, some_point.y, 8'd9, 8'd1};
```

or equivalently:

```
  rect_t rect = {some_point, 8'd9, 8'd1};
```

#### Vector types

Alogic supports multi-dimensional packed vectors of integer types.
Variables of a vector type can be declared by adding the vector sizes
following the type specifier in a declaration (<a
href="http://afiddle.argondesign.com/?example=types_vectors1.alogic">fiddle
here</a>):

```
// Vectors can be defined with one or more dimensions:

  u4[8] va;
  u3[6][9] vb;
   
// Vectors can be used as a whole:
   
  va = 32'd0; // 'va' is a 32-bit variable
  va <<= 1;
  
// Vectors can be indexed and used as elements:
  
  va[0] = 4'd0;  // [_] indexing yields the underlying element type (u4)
  va[1] = (va[0] << 1);

// Vectors can be partially indexed:

  vb = 162'd0;      // 'vb' is a 162 bit variable
  vb[0] = 27'd0;    // Partial indexing yields vectors of type u3[9]
  vb[0][0] = 3'd0;  // Full indexing yields primitive elements of type u3

// All of the following are allowed

  vb++;         // Increment 'vb' as a 162 bit variable
  vb[0]++;      // Increment 'vb[0]' as a 27 bit variable
  vb[0][0]++;   // Increment the 3-bit primitive element 
```

Note that indices are 0 based. Multi-dimensional vectors can be used
after partial indexing, which yield vectors of lower dimensions. Vectors
are linearized to a bit-string using row-major order, with lower order
indices packed towards the LSBs. For example, if we defined x and y as
(<a
href="http://afiddle.argondesign.com/?example=types_vectors2.alogic">fiddle
here</a>):

```
  u2[3][4] x = ...;
  u24 y = x;
```
Then the vector x is unpacked as follows:

 ![Vectors](vectors.svg)

And the following equivalencies hold:

```
  x[0] == y[7:0];
  x[1] == y[15:8];
  x[2] == y[23:16];

  x[0][0] == y[1:0];    // Lowest element
  x[1][2] == y[13:12];  // A middle element
  x[2][3] == y[23:22];  // Highest element
  
  x[0][0][0] == y[0];   // LSB
  x[2][3][1] == y[23];  // MSB
  x[i][j][k] == y[8*i + 2*j + k];
```

#### Void type

The `void` type can be used where no meaningful value is necessary. It
is used as the return type of state functions in FSMs, and as the type
of ports where the flow control signals carry all required information.

#### typedefs - <a href="http://afiddle.argondesign.com/?example=types_typedef.alogic">fiddle here.</a>

Similarly to the C language, a `typedef` definition can be used to
create an alias for a type, giving it a new name:

```
  typedef <existing type> <new type>;
  
  typedef u19 mytype_t; // 'mytype_t' is defined to be equivalent to u19
  mytype_t a; // A variable 'a' of type 'mytype_t'
```

#### Mapping to Verilog types

All packed Alogic types map to packed Verilog types with descending
range specifiers. Alogic only ever emits `reg [9:0] foo` or similar, and
never `reg [0:9] bar`.

Variables with a structure type are emitted as multiple Verilog
variables with field names adjoined with an `_`.

An example of mappings using the definitions from above is as follows:

<a href="http://afiddle.argondesign.com/?example=types_mapping.alogic">
Fiddle with these mappings here.</a>

<table>
  <tr>
    <th>Alogic</th><th>Verilog</th>
  </tr>
  <tr>
    <td><code>bool a;</code></td><td><code>reg a;</code></td>
  </tr>
  <tr>
    <td><code>u8 b;</code></td><td><code>reg [7:0] b;</code></td>
  </tr>
  <tr>
    <td><code>i5 c;</code></td><td><code>reg signed [4:0] c;</code></td>
  </tr>
  <tr>
    <td><code>uint(N) d;</code></td><td><code>reg [N-1:0] d;</code></td>
  </tr>
  <tr>
    <td><code>point_t e;</code></td><td><code>reg [15:0] e_x;<br>
                                              reg [15:0] e_y;</code></td>
  </tr>
  <tr>
    <td><code>rect_t f;</code></td><td><code>reg [15:0] f_topleft_x;<br>
                                             reg [15:0] f_topleft_y;<br>
                                             reg [7:0] f_width;<br>
                                             reg [7:0] f_height;</code></td>
  </tr>
  <tr>
    <td><code>u4[8] va;</code></td><td><code>reg [31:0] va;</code></td>
  </tr>
  <tr>
    <td><code>u3[6][9] vb;</code></td><td><code>reg [161:0] vb;</code></td>
  </tr>
</table>

<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="widths.md">Next</a>
</p>
