<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>

# Data types and simple variables

Alogic makes a distinction between packed types and unpacked (or non-packed)
types. This page is about packed types. 

## Packed Types

Packed types are those that are used to specify the bit level representation of
variables. For example, a 6-bit number would be a packed variable. Every packed
type maps to a one dimensional sequence of bits in a well defined way. The
number of bits in the binary representation of a packed type is called the
*width* of the type, and can be retrieved using the `@bits` [built-in
function](builtins.md).

Similarly to Verilog, Alogic is a weakly typed language in that any variable of
a packed type can be assigned to any variable of another packed type, as long as
the width of the 2 types are the same.

### Declarations

Variables are declared in the usual style, using a type specifier denoting the
type of the variable followed by the name of the variable:

```
  foo_t bar; // A variable called 'bar' with type 'foo_t'
```

### Supported packed types

#### Sized integer types - <a href="http://afiddle.argondesign.com/?example=types_sized_integers.alogic">fiddle here.</a>

The fundamental data types in Alogic are signed or unsigned integers introduced
with the `int` or `uint` keywords, with the number of bits used to represent
them specified in parentheses. The representation can depend on parameters or
constants:

```
  int(5)    a; // A 5 bit signed integer
  uint(8)   b; // An 8 bit unsigned integer
  uint(N+2) c; // Unsigned integer, with width depending on a parameter.
```

Since integer types of varying length are the norm in digital design,
there is a shorthand for writing them. A word starting with the letter `u`
followed by the number of bits can be used to denote an unsigned integer type
with that amount of bits. Similarly, the letter `i` can be used for signed
integers.

```
  i5 a; // Same as 'int(5) a'
  u8 b; // Same as 'uint(8) b'
```

The canonical parenthesized format is usually only used if the width
of the type is computed based on values of parameters or constants.

#### Bool type

The keyword `bool` can also be used to represent a boolean type, and is simply a
synonym for `u1`.

#### Struct types - <a href="http://afiddle.argondesign.com/?example=types_struct.alogic">
fiddle here.</a>

Alogic supports grouping related values into structures. A structure type is
defined with the `struct` keyword, followed by the name of the structure type,
and the structure fields in curly braces, ending with a semicolon:

```
struct point_t {
  u16 x;
  u16 y;
};

struct rect_t {
  point_t topleft;
  u8 width;
  u8 height;
};
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
  rect.width = 8'd9; // Set 'rect.w' to 9
  rect.height = 8'd1; // Set 'rect.h' to 1
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

#### Vector types - <a href="http://afiddle.argondesign.com/?example=types_sized_integers.alogic">
fiddle here.</a>

Alogic supports multi-dimensional packed vectors of integer types. Variables of
a vector type can be declared by adding the vector sizes following the type
specifier in a declaration:

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

Note that indices are 0 based. Multi-dimensional vectors can be used after
partial indexing, which yield vectors of lower dimensions. Vectors are
linearized to a bit-string using row-major order, with lower order indices
packed towards the LSBs. For example, if we defined x and y as:

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

### Void type

The `void` type can be used where no meaningful value is necessary. It is used
as the return type of state functions in FSMs, and as the type of ports where
the flow control signals carry all required information.

### typedefs - <a href="http://afiddle.argondesign.com/?example=types_typedef.alogic">
fiddle here.</a>

Similarly to the C language, a `typedef` declaration can be used to create an
alias for packed type, giving it a new name:

```
  typedef <existing type> <new type>;
  
  typedef u19 mytype_t; // 'mytype_t' is defined to be equivalent to u19
  mytype_t a; // A variable 'a' of type 'mytype_t'
```

### Mapping to Verilog types

All packed Alogic types map to packed Verilog types with descending range
specifiers. Alogic only ever emits `reg [9:0] foo` or similar, and never
`reg [0:9] bar`.

Variables with a structure type are emitted as multiple Verilog variables with
field names adjoined with an `_`.

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
<a href="literals.md">Next</a>
</p>
