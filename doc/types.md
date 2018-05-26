<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>

# Data types and simple variables

### About typing

Similarly to Verilog, Alogic is a weakly typed language. Data types are used
to specify the binary representations of variables, but are otherwise loosely
checked. A value of any data type can be assigned to a variable of any other,
type, though semantically the assignment might still be invalid.

### Simple variables

Simple variables are declared in the C style, with a word denoting
the type of the variable followed by the name of the variable.

```
  foo_t bar; // A variable called 'bar' with type 'foo_t'
```

### Integer types

Alogic supports 2 kinds of integer literals:
 - *Sized integers* are represented in a finite, defined number of bits
 - *Unsized integers* are infinite precision

Both kind of integers are further divided into signed and unsigned types,
yielding a total of 4 possible integer types:
 - Unsized unsigned
 - Unsized signed
 - Sized unsigned
 - Sized signed

The fundamental data types are signed or unsigned integers introduced with
the `int` or `uint` keywords, with the number of bits used to represent them
specified in parentheses. The representation can depend on parameters or
constants:

```
  int(5)    b; // A 5 bit signed integer
  uint(8)   a; // An 8 bit unsigned integer
  uint(N+2) c; // Unsigne integer, with width depending on a parameter.
```

Since integer types of widely varying length are the norm in digital design,
there is a shorthand for their writing. A word starting with the letter
`u` followed by the number of bits can be used to denote an unsigned integer
type with that amount of bits. Similarly, the letter `i` can be used for signed
integers.

```
  u8 a; // Same as 'uint(8) a'
  i5 b; // Same as 'int(5) a'
```

The canonical parenthesized format is usually only used if the width
of the type is computed based on values of parameters or constants.

The keyword `bool` can also be used as a synonym for `u1`.

### Void type

The `void` type can be used where no meaningful value is necessary. It is used
as the return type of state functions in FSMs, and as the type of ports where
the flow control signals carry all required semantics.

### typedefs

A `typedef` declaration can be used to alias an integer type to a new name,
similarly to the C language:

```
  typedef u19 mytype_t; // 'mytype_t' is equivalent to u19
  mytype_t a; // A variable 'a' of type 'mytype_t'
```

### Struct types

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

### Mapping to Verilog types

All Alogic types (except arrays) map to packed Verilog types with descending
range specifiers. i.e.: Alogic only ever emits `reg [9:0] foo` or similar, and
never `reg [0:9] bar`.

Variables with a structure type are emitted as multiple Verilog variables
with field names adjoined with an `_`.

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
</table>

<p align="center">
<a href="entities.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>
