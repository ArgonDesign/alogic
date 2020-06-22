<p align="center">
<a href="control.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="networks.md">Next</a>
</p>

# Expressions

Alogic supports the same kind of expressions (and using the same syntax) as
Verilog, with a few restrictions. These are documented in this section.

### Unary operators

Alogic supports the following unary prefix operators, with the same meaning as
their Verilog equivalent:

- `+` unary plus (does nothing)
- `-` unary minus
- `!` unary logical not
- `~` unary bit invert
- `&` unary bit reduce and
- `|` unary bit reduce or
- `^` unary bit reduce xor

### Binary operators

Alogic supports the following binary operators, with the same meaning as
their Verilog equivalent:

- `*` Binary multiplication
- `/` Binary division
- `%` Binary remainder
- `+` Binary addition
- `-` Binary subtraction
- `<<` Logical shift left
- `>>` Logical shift right
- `<<<` Arithmetic shift left (same as `<<`)
- `>>>` Arithmetic shift right
- `>` Compare greater
- `>=` Compare greater or equals
- `<` Compare less
- `<=` Compare less or equals
- `==` Compare equals
- `!=` Compare not equals
- `&` Bitwise and
- `^` Bitwise xor
- `|` Bitwise or
- `&&` Logical and
- `||` Logical or

### Conditional operator

Alogic supports the usual ternary conditional `?:` operator:

```
  cond ? thenExpr : elseExpr ;
```

### Indexing and slicing

<a href="http://afiddle.argondesign.com/?example=expr_indexing.alogic">Fiddle with indexing and slicing here.</a>

Values with appropriate type can be indexes with the usual `[_]` syntax,
possibly multiple times.

```
 u2 a = 2'b0;
 bool b = a[1];
```

Similarly a range can be extracted from variables with appropriate type using
the `[_:_]` slice syntax:

```
 u4 c = 4'b0;
 u2 d = c[2:1];
```

In an Alogic `[_:_]` slice expression, the left hand side index must be greater
than or equal to the right hand side index.

`[_+:_]` and `[_-:_]` are also supported, with the same meaning as in Verilog.

As opposed to Verilog, Alogic allows indexing or slicing of arbitrary
sub-expressions with a packed type:

```
 u8 a;
 u8 b;

 u4 topOfSum = (a + b)[7:4];
 bool lsbOfDifference = (a - b)[0];
```

### Bit concatenations and repetition

Alogic supports the Verilog bit concatenation `{a, b, c}`, and repetition
`{4{d}}` operators. A combination of the two is also supported: `{8{e,f}}`.

### Attribute selection

Structure fields, methods and properties of appropriate type can be selected
with the usual `.` syntax:

```
width = rectangle.bottom_right.x - rectangle.top_left.x;
```

### Function call expressions

Function calls are performed using the usual postfix `()` syntax. No named
parameter assignment is allowed. For example, to call the `@zx()` function to
zero-extend a variable:

```
u8 a = @zx(8, b[1:0]);
```

### Operator precedence

The precedence of Alogic operators is the same as in the Verilog language.

<p align="center">
<a href="control.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="networks.md">Next</a>
</p>
