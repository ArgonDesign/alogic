<p align="center">
<a href="types.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ports.md">Next</a>
</p>

# Literal values

### Integer literals

As described in the chapter on [types](types.md), Alogic supports both sized,
and unsized integers, both of which can further be split in to signed and
unsigned variants.

There is literal syntax to write values of either of the 4 integer type
variants.

The syntax for sized and unsized integer literals is similar, with the following
commonalities:
- Literals start with an optional `+` or `-` sign
- Base is denoted by one of the characters `b`, `d`, or `h` following the `'`,
for binary, decimal, or hexadecimal bases respectively
- An optional `s` signed modifier can be used between the `'` and the base
specifier character

Common rules for integer literals:
- A leading `+` does nothing, and is allowed to improve clarity where required
- Omitting the sign is the same as writing `+`
- Whitespace is allowed between the sign and the rest of the literal, but
whitespace is illegal elsewhere
- Integer literals that do use `s` modifier are of an unsigned type, while
those with an `s` modifier are signed
- Digits standing for the value must conform to the specified base
    - `0-1` can be used with binary base `b`
    - `0-9` can be used with decimal base `d`
    - `0-9`, `a-f`, `A-F` can be used with hexadecimal base `h`
- Sequences of digits standing for the value can further contain `_` delimiter
character, except as the first or last digit.

#### Unsized integer literals

The generic syntax of unsized integer literals is composed of:
- Optional `+` or `-` sign
- Optional `'` followed by optional `s` and the base specifier `b`, `d` or `h`
- Required list of digits standing for the value of the literal

If the base specifier is omitted, decimal base is implied. Using a `-` sign with
an unsigned unsized literal is illegal, unless the value is 0. The value of a
literal with a `-` sign is always the negative of the value of the literal
without the `-` sign.

The following table provides an exhaustive set of examples:

|   literal | value | type | note                                                |
|----------:|------:|-----:|:----------------------------------------------------|
|       17  |    17 | uint |                                                     |
|      -17  |   N/A |  N/A | Error - negative unsigned                           |
|     'd17  |    17 | uint | Same as 17                                          |
|     'sd17 |    17 |  int |                                                     |
|    -'d17  |   N/A |  N/A | Same as -17, and is an Error                        |
|    -'sd17 |   -17 |  int |                                                     |
|  'b10001  |    17 | uint |                                                     |
|  'sb10001 |    17 |  int |                                                     |
| -'b10001  |   N/A |  N/A | Error - negative unsigned                           |
| -'sb10001 |   -17 |  int |                                                     |
|     'h11  |    17 | uint |                                                     |
|     'sh11 |    17 |  int |                                                     |
|    -'h11  |   N/A |  N/A | Error - negative unsigned                           |
|    -'sh11 |   -17 |  int |                                                     |
|        0  |     0 | uint |                                                     |
|        0s |     0 |  int |                                                     |
|       -0  |     0 | uint | OK - special case of negative unsigned with value 0 |
|       -0s |     0 |  int |                                                     |

#### Sized integer literals

The generic syntax of sized integer literals is composed of:
- Optional `+` or `-` sign
- Required list of decimal digits standing for the width of the literal
- Required `'` followed by the base specifier `b`, `d` or `h`
- Required list of digits standing for the value of the literal (see about
interpretation below)
- Optional `s` signed suffix

The list of digits standing for the value specifies the bit pattern of the
value. It is important to understand the meaning of the previous sentence. In
particular, observe that for signed literals, the syntax can yield a negative
value for a seemingly positive literal, e.g. in `4'sd15`, which is numerically
equivalent to the decimal `-1`, or `4'sd8`, which is actually equivalent to
`-8`. Similarly for unsigned literals, applying a `-` sign will still yield a
positive value, as the result is of unsigned type. The compiler will warn if the
sign of the literal is different from the sign of the value it represents.

The value of a sized literal can always be derived by taking the binary bit
pattern of the digits standing for the value (as an unsigned number in the
specified base), and interpreting this bit pattern as an unsigned or 2's
complement signed value (depending on the presence of the `s` suffix), on the
specified number of bits. It is illegal for the digits standing for the value
to specify a bit pattern than does not fit in the specified width as an
unsigned number.

The value of a sized integer literal with a `-` sign is always the 2's
complement of the value of the literal without the `-` sign, represented on
the specified number of bits.

The following table provides an exhaustive set of examples:

|      literal | bit pattern | value in 4 bit binary | equivalent decimal | type | note                     |
|-------------:|--------:|---------:|---------:|-----:|:----------------------------------------------------|
| decimal base |         |          |          |      |                                                     |
|    4'd3      |    0011 |     0011 |        3 |   u4 |                                                     |
|    4'sd3     |    0011 |     0011 |        3 |   i4 |                                                     |
|   -4'd3      |   -0011 |     1101 |       13 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sd3     |   -0011 |     1101 |       -3 |   i4 |                                                     |
|    4'd0      |    0000 |     0000 |        0 |   u4 |                                                     |
|    4'sd0     |    0000 |     0000 |        0 |   i4 |                                                     |
|   -4'd0      |   -0000 |     0000 |        0 |   u4 | OK - special case of negative unsigned with value 0 |
|   -4'sd0     |   -0000 |     0000 |        0 |   i4 |                                                     |
|    4'd1      |    0001 |     0001 |        1 |   u4 |                                                     |
|    4'sd1     |    0001 |     0001 |        1 |   i4 |                                                     |
|   -4'd1      |   -0001 |     1111 |       15 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sd1     |   -0001 |     1111 |       -1 |   i4 |                                                     |
|    4'd15     |    1111 |     1111 |       15 |   u4 |                                                     |
|    4'sd15    |    1111 |     1111 |       -1 |   i4 | Warning - sign of literal and value do not match    |
|   -4'd15     |   -1111 |     0001 |        1 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sd15    |   -1111 |     0001 |        1 |   i4 | Warning - sign of literal and value do not match    |
|    4'd7      |    0111 |     0111 |        7 |   u4 |                                                     |
|    4'sd7     |    0111 |     0111 |        7 |   i4 |                                                     |
|   -4'd7      |   -0111 |     1001 |        9 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sd7     |   -0111 |     1001 |       -7 |   i4 |                                                     |
|    4'd8      |    1000 |     1000 |        8 |   u4 |                                                     |
|    4'sd8     |    1000 |     1000 |       -8 |   i4 | Warning - sign of literal and value do not match    |
|   -4'd8      |   -1000 |     1000 |        8 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sd8     |   -1000 |     1000 |       -8 |   i4 |                                                     |
|    4'd16     |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|    4'sd16    |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |
|   -4'd16     |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|   -4'sd16    |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |
|  binary base |         |          |          |      |                                                     |
|    4'b11     |    0011 |     0011 |        3 |   u4 |                                                     |
|    4'sb11    |    0011 |     0011 |        3 |   i4 |                                                     |
|   -4'b11     |   -0011 |     1101 |       13 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sb11    |   -0011 |     1101 |       -3 |   i4 |                                                     |
|    4'b0      |    0000 |     0000 |        0 |   u4 |                                                     |
|    4'sb0     |    0000 |     0000 |        0 |   i4 |                                                     |
|   -4'b0      |   -0000 |     0000 |        0 |   u4 | OK - special case of negative unsigned with value 0 |
|   -4'sb0     |   -0000 |     0000 |        0 |   i4 |                                                     |
|    4'b1      |    0001 |     0001 |        1 |   u4 |                                                     |
|    4'sb1     |    0001 |     0001 |        1 |   i4 |                                                     |
|   -4'b1      |   -0001 |     1111 |       15 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sb1     |   -0001 |     1111 |       -1 |   i4 |                                                     |
|    4'b1111   |    1111 |     1111 |       15 |   u4 |                                                     |
|    4'sb1111  |    1111 |     1111 |       -1 |   i4 | Warning - sign of literal and value do not match    |
|   -4'b1111   |   -1111 |     0001 |        1 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sb1111  |   -1111 |     0001 |        1 |   i4 | Warning - sign of literal and value do not match    |
|    4'b111    |    0111 |     0111 |        7 |   u4 |                                                     |
|    4'sb111   |    0111 |     0111 |        7 |   i4 |                                                     |
|   -4'b111    |   -0111 |     1001 |        9 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sb111   |   -0111 |     1001 |       -7 |   i4 |                                                     |
|    4'b1000   |    1000 |     1000 |        8 |   u4 |                                                     |
|    4'sb1000  |    1000 |     1000 |       -8 |   i4 | Warning - sign of literal and value do not match    |
|   -4'b1000   |   -1000 |     1000 |        8 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sb1000  |   -1000 |     1000 |       -8 |   i4 |                                                     |
|    4'b10000  |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|    4'sb10000 |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |
|   -4'b10000  |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|   -4'sb10000 |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |
|     hex base |         |          |          |      |                                                     |
|    4'h3      |    0011 |     0011 |        3 |   u4 |                                                     |
|    4'sh3     |    0011 |     0011 |        3 |   i4 |                                                     |
|   -4'h3      |   -0011 |     1101 |       13 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sh3     |   -0011 |     1101 |       -3 |   i4 |                                                     |
|    4'h0      |    0000 |     0000 |        0 |   u4 |                                                     |
|    4'sh0     |    0000 |     0000 |        0 |   i4 |                                                     |
|   -4'h0      |   -0000 |     0000 |        0 |   u4 | OK - special case of negative unsigned with value 0 |
|   -4'sh0     |   -0000 |     0000 |        0 |   i4 |                                                     |
|    4'h1      |    0001 |     0001 |        1 |   u4 |                                                     |
|    4'sh1     |    0001 |     0001 |        1 |   i4 |                                                     |
|   -4'h1      |   -0001 |     1111 |       15 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sh1     |   -0001 |     1111 |       -1 |   i4 |                                                     |
|    4'hf      |    1111 |     1111 |       15 |   u4 |                                                     |
|    4'shf     |    1111 |     1111 |       -1 |   i4 | Warning - sign of literal and value do not match    |
|   -4'hf      |   -1111 |     0001 |        1 |   u4 | Warning - sign of literal and value do not match    |
|   -4'shf     |   -1111 |     0001 |        1 |   i4 | Warning - sign of literal and value do not match    |
|    4'h7      |    0111 |     0111 |        7 |   u4 |                                                     |
|    4'sh7     |    0111 |     0111 |        7 |   i4 |                                                     |
|   -4'h7      |   -0111 |     1001 |        9 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sh7     |   -0111 |     1001 |       -7 |   i4 |                                                     |
|    4'h8      |    1000 |     1000 |        8 |   u4 |                                                     |
|    4'sh8     |    1000 |     1000 |       -8 |   i4 | Warning - sign of literal and value do not match    |
|   -4'h8      |   -1000 |     1000 |        8 |   u4 | Warning - sign of literal and value do not match    |
|   -4'sh8     |   -1000 |     1000 |       -8 |   i4 |                                                     |
|    4'h10     |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|    4'sh10    |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |
|   -4'h10     |     N/A |      N/A |      N/A |   u4 | Error - too many bits specified                     |
|   -4'sh10    |     N/A |      N/A |      N/A |   i4 | Error - too many bits specified                     |

### Boolean literals

The literal `true` can be used as a synonym for `1'b1`, and the literal `false`
can be used as a synonym for `1'b0`, but otherwise `true` and `false` have no
no special meaning.

### String literals

The language accepts string literals enclosed in double quotes `"`. Alogic
does not support interpreting strings as a bit vector. String literals should
only be used as arguments to builtin function that require a string argument,
e.g. `$display`:

```
$display("Help!");
```

<p align="center">
<a href="types.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ports.md">Next</a>
</p>
