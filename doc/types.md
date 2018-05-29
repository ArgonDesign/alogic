<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>

# Data types and simple variables

Alogic makes a distinction between packed types and unpacked (or non-packed)
types. Packed types are those that are used to specify the bit level
representation of variables. Every packed type maps to a one dimensional
sequence of bits in a well defines way. The number of bits in the binary
representation of a packed type is called the **width** of the type, and can be
retrieved using the `@bits` built-in function.

Similarly to Verilog, Alogic is a weakly typed language in that any variable of
a packed type can be assigned to any variable of another packed type, as long as
the width of the 2 types are the same.

### Declarations of simple variable of packed types

Simple variables are declared in the usual style, using a type specifier denoting
the type of the variable followed by the name of the variable:

```
  foo_t bar; // A variable called 'bar' with type 'foo_t'
```

### Supported packed types

#### Sized integer types

The fundamental data type in Alogic are signed or unsigned integers introduced
with the `int` or `uint` keywords, with the number of bits used to represent
them specified in parentheses. The representation can depend on parameters or
constants:

```
  int(5)    b; // A 5 bit signed integer
  uint(8)   a; // An 8 bit unsigned integer
  uint(N+2) c; // Unsigned integer, with width depending on a parameter.
```

Since integer types of widely varying length are the norm in digital design,
there is a shorthand for writing them. A word starting with the letter `u`
followed by the number of bits can be used to denote an unsigned integer type
with that amount of bits. Similarly, the letter `i` can be used for signed
integers.

```
  u8 a; // Same as 'uint(8) a'
  i5 b; // Same as 'int(5) a'
```

The canonical parenthesized format is usually only used if the width
of the type is computed based on values of parameters or constants.

#### Bool type

The keyword `bool` can also be used to represent a boolean type, and is simply a
synonym for `u1`.

#### Struct types

Alogic support grouping related values into structures. A structure type is
defined with the `struct` keyword, followed by the name of the structure type,
and the structure fields in curly braces:

```
struct point_t {
  u16 x;
  u16 y;
}

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
  rect.w = 9; // Set 'rect.w' to 9
  rect.h = 1; // Set 'rect.h' to 1
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

Alogic supports multi-dimensional packed vectors of integer types. Variables of
a vector type can be declared by adding the vector sizes following the type
specifier in a declaration:

```
  u4[8] va;  // A vector of 8 4-bit unsigned integers

  u3[6][9] vb; // A 6-vector of 9-vectors of 3-bit integers (or a 6x9 matrix of 3-bit integers)
```

Vectors can be indexed (indices are 0 based), or used as a whole.
Multi-dimensional vectors can be used after partial indexing, which yield
vectors of lower dimensions. Assuming the definitions from above:

```
  va = 32'd0;    // 'va' is a 32-bit variable
  va[0] = 4'd0;  // [_] indexing yields the underlying element type, here the u4

  va <<= 1;             // 'va' can be used as a whole;
  va[1] = va[0] << 1;   // Or as elements

  vb = 162'd0;      // 'vb' is 162 bit variable
  vb[0] = 27'd0;    // The elements of 'vb' are vectors of type u3[9]
  vb[0][0] = 3'd0;  // They can be further index to get to the primitive element

  // All of the following is allowed
  vb++;         // Incremet 'vb' as a 169 bit variable
  vb[0]++;      // Increment 'vb[0]' as a 27 bit variable
  vb[0][0]++;   // Increment the 3-bit underlying element 
```

Vectors are linearized to a bit-string using row-major order, with lower order
indices packed towards the LSBs, observe:

```
  u2[3][4] x = ...;

  u24 y = x;

  // Now the following equivalencies hold

  x[0][0][0] == y[0];   // LSB
  x[2][3][1] == y[31];  // MSB

  x[0][0] == y[1:0];    // Lowest element
  x[2][3] == y[31:30];  // Highest element

  x[0] == y[7:0];
  x[1] == y[15:8];
  x[2] == y[23:16];

  x[0][0] == y[1:0];
  x[0][1] == y[3:2];
  x[0][2] == y[5:4];
  x[0][3] == y[7:6];
  x[1][0] == y[9:8];
  x[1][1] == y[11:10];
  x[1][2] == y[13:12];
  x[1][3] == y[15:14];
  x[2][0] == y[17:16];
  x[2][1] == y[19:18];
  x[2][2] == y[21:20];
  x[2][3] == y[23:22];
```

### Void type

The `void` type can be used where no meaningful value is necessary. It is used
as the return type of state functions in FSMs, and as the type of ports where
the flow control signals carry all required information.

### typedefs

Similarly to the C language, a `typedef` declaration can be used to create an
alias for packed type, giving it a new name:

```
  typedef u19 mytype_t; // 'mytype_t' is equivalent to u19
  mytype_t a; // A variable 'a' of type 'mytype_t'
```

### Mapping to Verilog types

All packed Alogic types map to packed Verilog types with descending range
specifiers. i.e.: Alogic only ever emits `reg [9:0] foo` or similar, and never
`reg [0:9] bar`.

Variables with a structure type are emitted as multiple Verilog variables with
field names adjoined with an `_`.

An example of mappings using the definitions from above is as follows:

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
