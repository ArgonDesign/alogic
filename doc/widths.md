<p align="center">
<a href="types.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>

# Expression typing rules

Digital design involves working with a multitude of signals with various
bit widths. Combining expressions of variables with various widths can
be error prone and difficult to understand due to the complex semantics
of evaluation of expressions of arbitrarily sized values. Alogic aims to
improve clarity and reduce errors caused by unexpected behaviour by
adopting three mechanisms:

1. The Alogic compiler enforces a small, well defined set of constraints
   about the widths of values in certain contexts in the language.
2. All but one operator in the language have a well defined result type
   dependent only on the operands in the expression.
3. The single exception to rule 2 above is the unary prefix `'` (tick)
   operator which is provided as a standard mechanism for alleviating
   the tedium of maintaining well sized expressions throughout when the
   proper size is obvious.

#### Basic type constraints

The fundamental type constraint in Alogic is that every assignment,
initialization or name binding where the target is of a packed type must
have a value that is the same width as the target.

This includes
- Assignment: `a = b`
- Variable initialization: `u8 a = b`
- Function parameter passing: `port.write(b)`
- Instance parameter assignment: `inst = new foo(A=A)`

Indexing and slicing expressions require that indices be of an unsigned
type, and that indices are exactly as wide as required to address every
element of the target of the expression as defined by the `$clog2`
[built-in function](builtins.md).

To help reduce designer effort, everywhere where a packed value of a
given width is required, but a value of an unsized integer type is
provided, the compiler will
[infer the width](types.md#unsized-integer-types) of the unsized value
to be the required width. Note however that width inference never
changes the signedness of the value provided. Values with an unsized
type are always compile time constant so width inference will either
always succeed, or the compiler will raise an error if the value cannot
be represented by the target type.

#### Operator type constraints and result types

The Alogic compiler enforces the following constraints over the width of
packed operands over every expression in the language. A violation of
these constraints is signalled as an error by the compiler. The
enumeration below is exhaustive and lists all operators in the language,
including ones with no width constraints. The constraints are described
using the `@bits`, `@max` and `$clog2` built-in functions.

| Operator                                                           | Example     | Constraint |
|--------------------------------------------------------------------|-------------|-------------|
| Indexing                                                           | `a[b]`      | @bits(b) == @max(1, $clog2(size of indexed dimension)) |
| Slice (all 3 forms `:` `+:` `-:`)                                  | `a[b:c]`    | @bits(b) == @bits(c) == @max(1, $clog2(size of indexed dimension)) |
| Unary `+` `-` `~` `&` `\|` `^`                                     | `-a`        | none |
| Unary `'`                                                          | `'a`        | @bits(a) <= result width (see later sections) |
| Binary `*` `/` `%` `+` `-` `&` `\|` `^` `>` `>=` `<` `<=` `==` `!=`| `a + b`     | @bits(a) == @bits(b) |
| Binary `<<` `<<<` `>>` `>>>` `&&` `\|\|`                           | `a << b`    | none |
| Ternary                                                            | `a ? b : c` | @bits(b) == @bits(c) |
| Repetition                                                         | `{N{a}}`     | none |
| Concatenation                                                      | `{a, b}`    | none |

Every operator detailed above also yields a result which has a well
defined type dependent only on the operands (save for unary `'`). The
result types of operators are:

| Operator                                  | Example     | Result type |
|-------------------------------------------|-------------|-------------|
| Indexing of vector or array               | `a[b]`      | type of element |
| Indexing of any other packed value        | `a[b]`      | `u1` |
| Slice `:`                                 | `a[b:c]`    | `uint(b - c + 1)` |
| Slice `+:` `-:`                           | `a[b+:c]`   | `uint(c)` |
| Unary `+` `-` `~`                         | `-a`        | type of operand |
| Unary `&` `\|` `^`                        | `&a`        | `u1` |
| Unary `'`                                 | `'a`        | `int(result width)` if `a` is signed else `uint(result width)` |
| Binary `*` `/` `%` `+` `-` `&` `\|` `^`   | `a + b`     | `int(@bits(a))` if both `a` and `b` are signed else `uint(@bits(a))` |
| Binary `>` `>=` `<` `<=` `==` `!=`        | `a + b`     | `u1` |
| Binary `<<` `<<<` `>>` `>>>`              | `a << b`    | type of left hand operand |
| Binary `&&` `\|\|`                        | `a && b`    | `u1` |
| Ternary `?:`                              | `a ? b : c` | `int(@bits(b))` if both `b` and `c` are signed, else `uint(@bits(b))` |
| Repetition                                | `{N{a}}`    | `uint(N*@bits(a))` |
| Concatenation                             | `{a, b}`    | `uint(sum of widths of operands)` |

#### Expressions with unsized integer operands

The widths of operands with an unsized integer type will be inferred to
satisfy the operator width constraints whenever possible. The exact
context where width inference is performed is described in the
discussion of [unsized integer types](types.md#unsized-integer-types).

The unary `&`, `|`, `^` and `~` operators cannot be applied to unsized
integer types. Indexing and slicing of values with unsized integer type
are only allowed with compile time constant indices, and yield the bit
values from the binary representation of the value (2's complement for
signed values).

The result type of the following operators will be an unsized integer
type under the following conditions. The signedness of the result is
determined by the operator as detailed above and is not influenced by
whether the result is sized or unsized:

| Operator                                  | Condition for result to be unsized type |
|-------------------------------------------|-----------------------------------------|
| Unary `+` `-`                             | operand is unsized |
| Binary `*` `/` `%` `+` `-` `&` `\|` `^`   | both operands are unsized |
| Binary `<<` `<<<` `>>` `>>>`              | left hand operand is unsized |
| Ternary `?:`                              | both branch operands are unsized |

Remember that every expression of an unsized integer type must be a
compile time constant and will be evaluated at compilation time. This
means that while `5 << 2` or `1 ? 2 : 3` are always valid, `5 << A` or
`A ? 2 : 3` are only valid if `A` is a compile time constant (i.e.: it's
a `param` or a `const`).

#### The unary `'` operator

The unary `'` (tick) operator when applied to packed values, is used to
automatically sign or zero extend its operand to a width determined by
the context in which the `'` operator is used. The unary `'` is the only
operator in Alogic whose result type depends on context other than the
operands of the operator.

The width of the result of the unary `'` operator is defined in the
following contexts:

| Context                                                   | Example | Width of result of unary `'` |
|-----------------------------------------------------------|---------|------------------------------|
| Index expressions                                         | `x['a]` | max($clog2(size of indexed dimension), 1) |
| Slice position expressions                                | `x['a:0]` | max($clog2(size of sliced dimension), 1) |
| Slice width expressions                                   | `x['a+:1]` | max($clog2(size of sliced dimension), 1) |
| Function arguments with a packed formal type              | `f('a)` | width of formal argument |
| Instance parameter assignment to packed parameter         | `inst = new foo(A='A)` | width of parameter |
| Initializer expression in declaration of packed variable  | `u8 x = 'a` | width of declared variable |
| Assignment to packed right hand side                      | `x = 'a` | widht of target of assignment |
| Operator assignment with strict width operators\[\*\]     | `x += 'a` | width of target of assignment |

\[\*\]: Strict width binary operators which define the width of a unary
`'` are the arithmetic (`*`, `/`, `%`, `+`, `-`), and bitwise
logical (`&`, `|`, `^`).

It is illegal to use the unary `'` operator outside of one of the above
contexts. Furthermore, the unary `'` cannot be used for narrowing, i.e.:
it cannot yield a result with smaller width then the operand. The
compiler will raise an error when the use of `'` would result in a
narrower result.

The unary `'` operator can be an arbitrarily nested sub-expression
inside a compound expression, and its width will be determined by the
closest enclosing context. Here are some examples:

```
a = 'b;     // b is widened to the width of a
a = 'b + c; // b is widened, the '+' constraints apply, so c must be
            // the same width as 'b, which has the width of a
a = 'b + 2; // b is widened, the width of the unsized constant is
            // then inferred to be the width of 'b
a = 'b + 2 * ('c - d); // b and c are both widened to the width of a,
            // d must have the same width, and the 2 is inferred to have
            // the same width as well
a = 'b['c]  // c is widened to the number of bits required to index b,
            // then the result of b['c] is widened to the width of a
```

Observe that the above contexts and result widths are the same as the
width inference contexts for unsized values, but note crucially that
they do not include the binary and ternary operators. This omission is
deliberate and is intended to reduce the cognitive load on the designer
by keeping the width of the result of the `'` simple to understand.

The result of the unary `'` operator always have the same signedness as
its operand. Unsigned values are widened using zero extension in the
MSBs, and signed values are widened using sign extension in the MSBs.

#### Using packed values where an unsized type is expected

In certain contexts, the language requires an unsized value. Examples
include parameter assignments to unsized parameters or initializers of
unsized `const` declarations. In these contexts the unary `'` operator
can be used to convert a packed value to an unsized value. For example:

```
param u32 TOP;
param u32 BOT;
const uint SIZE = '(BOT - TOP);
```

As unsized values need to be compile time constant, this use of the
unary `'` operator is essentially computing the constant value of the
operand and returns it as an unsized value. This can be used to precisely
control how an expression is evaluated:

```
param u8 A;
param u8 B;

const uint C = '(A + B); // The result type of the '+' is u8, hence the
                         // value of the sum is evaluated on 8 bits,
                         // wrapping on overflow.
const uint D = 'A + 'B;  // A and B are converted to unsized values,
                         // then the sum is evaluated on infinite
                         // precision avoiding oveflow
```

<p align="center">
<a href="types.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="literals.md">Next</a>
</p>
