<p align="center">
<a href="widths.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ports.md">Next</a>
</p>

# Literal values

#### Unsized integer literals

A simple decimal number (e.g.: `1`) is interpreted as an unsigned unsized
integer. It can optionally be preceded by a `+` sign and optionally followed by
a suffix `u`. Both the `+` prefix and the `u`
suffix have no effect, but are allowed to enhance clarity where appropriate. A
decimal signed integer can be written the same way but with a suffix `s` (
e.g.: `1s`), and with an optional prefix `+` or `-`
sign. Again, the `+` prefix is allowed but has not effect.

Alternatively, the number can be preceded by one of the `0b`, `0o`, `0d`
or `0x` base specifiers for binary, octal, decimal or hexadecimal bases
respectively.

A simple decimal literal with a leading zero but without a base specifier is
illegal. Use `0o` for octal or `0d` for decimal with leading zeros.

#### Sized integer literals

Sized integer literals follow the Verilog syntax.

An unsigned sized integer literal begins with an optional `+` sign, which has no
effect, followed by the `'` tick character, followed by a base specifier
character which must be one of `b`, `d` or `h` for binary, decimal and
hexadecimal base, followed by a list of digits in the specified base to denote
the value.

Signed sized literals are written similarly, but with an `s` between the
`'` tick and the base specifier to mark a signed literal, and may have an
optional `+` or `-` sign. Signed sized numbers are represented with the 2's
complement binary format. The value of a signed sized literal with a `-` sign is
the 2's complement of the value of the literal without the `-` sign.

To get the fixed width bit pattern representing the literal, take the list of
digits denoting the value, interpret it as an unsigned unsized literal, and take
the trailing LSBs of the binary representation according to the width. This
means that for sized literals, the digits specify the bit pattern of the value
as opposed to the numerical value, which can sometimes give unexpected results
with signed sized literals. In particular, observe that for signed literals, the
syntax can yield a negative value for a seemingly positive literal: `4'sd15`
would use the 4-wide bit pattern 1111, which in the 4-bit 2's complement
representation stands for the value `-1`. Similarly the numerical value
of `-4'sd8` is `8`. The compiler will issue a warning for such signed sized
literals. It is illegal for the digits standing for the value to specify a bit
pattern whose set than does not fit in the specified width as an unsigned
number.

#### General rules about sized and unsized integer literals

An unsigned literal written with a `-` sign is illegal, unless the value is 0.
The literal value (but not the size specifier) may contain non-consecutive `_`
characters to be used as digit separators, but not as the first or last
character. No whitespace is allowed inside an integer literal, except between a
leading `+` or `-` sign and the rest of the literal.

#### Example unsized integer literals

|   literal | value | type | note                                                |
|----------:|------:|-----:|:----------------------------------------------------|
|`      17 `|    17 |`uint`|                                                     |
|`      17u`|    17 |`uint`| Same as `17`                                        |
|`     +17 `|    17 |`uint`| Same as `17`                                        |
|`     +17u`|    17 |`uint`| Same as `17`                                        |
|`     -17 `|   N/A |  N/A | Error - negative unsigned literal                   |
|`      17s`|    17 |` int`|                                                     |
|`     +17s`|    17 |` int`| Same as `17s`                                       |
|`     -17s`|   -17 |` int`|                                                     |
|` 0b10001 `|    17 |`uint`| unsigned binary                                     |
|` 0b10001u`|    17 |`uint`| Same as `0b10001`                                   |
|`+0b10001 `|    17 |`uint`| Same as `0b10001`                                   |
|`+0b10001u`|    17 |`uint`| Same as `0b10001`                                   |
|`-0b10001 `|   N/A |  N/A | Error - negative unsigned literal                   |
|` 0b10001s`|    17 |` int`| signed binary                                       |
|`+0b10001s`|    17 |` int`| Same as `0b10001s`                                  |
|`-0b10001s`|   -17 |` int`|                                                     |
|`    0o21 `|    17 |`uint`| unsigned octal                                      |
|`   -0o21s`|   -17 |` int`| signed octal                                        |
|`    0d17 `|    17 |`uint`| unsigned decimal, same as `17`                      |
|`0d000017 `|    17 |`uint`| With leading zeros, same as `17`                    |
|`    0x11 `|    17 |`uint`| unsigned hexadecimal                                |
|`    0x11s`|    17 |` int`| signed hexadecimal                                  |
|`       0 `|     0 |`uint`|                                                     |
|`       0 `|     0 |` int`|                                                     |
|`      -0 `|     0 |`uint`| OK - special case of negative unsigned with value 0 |
|`      -0s`|     0 |` int`|                                                     |
|` 0x1a_2b `|  6699 |` int`| Digit separator                                     |

#### Example sized integer literals

|      literal | bit pattern | value in 4 bit binary | equivalent decimal | type | note                     |
|-------------:|--------:|---------:|---------:|-----:|:----------------------------------------------------|
| decimal base |         |          |          |      |                                                     |
|`   4'd3     `|    0011 |     0011 |        3 |`  u4`|                                                     |
|`   4'sd3    `|    0011 |     0011 |        3 |`  i4`|                                                     |
|`  -4'd3     `|     N/A |      N/A |      N/A |  N/A | Error - negative unsigned literal                   |
|`  -4'sd3    `|   -0011 |     1101 |       -3 |`  i4`|                                                     |
|`   4'd0     `|    0000 |     0000 |        0 |`  u4`|                                                     |
|`   4'sd0    `|    0000 |     0000 |        0 |`  i4`|                                                     |
|`  -4'd0     `|   -0000 |     0000 |        0 |`  u4`| OK - special case of negative unsigned with value 0 |
|`  -4'sd0    `|   -0000 |     0000 |        0 |`  i4`|                                                     |
|`   4'sd15   `|    1111 |     1111 |       -1 |`  i4`| Warning - sign of literal and value do not match    |
|`  -4'sd15   `|   -1111 |     0001 |        1 |`  i4`| Warning - sign of literal and value do not match    |
|`   4'd16    `|     N/A |      N/A |      N/A |  N/A | Error - too many bits specified                     |
|  binary base |         |          |          |      |                                                     |
|`   4'b11    `|    0011 |     0011 |        3 |`  u4`|                                                     |
|`   4'sb11   `|    0011 |     0011 |        3 |`  i4`|                                                     |
|`  -4'b11    `|     N/A |      N/A |      N/A |`  u4`| Error - negative unsigned literal                   |
|`  -4'sb11   `|   -0011 |     1101 |       -3 |`  i4`|                                                     |
|`   4'sb1111 `|    1111 |     1111 |       -1 |`  i4`| Warning - sign of literal and value do not match    |
|`  -4'sb1111 `|   -1111 |     0001 |        1 |`  i4`| Warning - sign of literal and value do not match    |
|`   4'b11111 `|     N/A |      N/A |      N/A |  N/A | Error - too many bits specified                     |
|     hex base |         |          |          |      |                                                     |
|`   4'h3     `|    0011 |     0011 |        3 |`  u4`|                                                     |
|`   4'sh3    `|    0011 |     0011 |        3 |`  i4`|                                                     |
|`  -4'h3     `|     N/A |      N/A |      N/A |  N/A | Error - negative unsigned literal                   |
|`  -4'sh3    `|   -0011 |     1101 |       -3 |`  i4`|                                                     |
|`   4'shf    `|    1111 |     1111 |       -1 |`  i4`| Warning - sign of literal and value do not match    |
|`  -4'shf    `|   -1111 |     0001 |        1 |`  i4`| Warning - sign of literal and value do not match    |
|`   4'hff    `|     N/A |      N/A |      N/A |  N/A | Error - too many bits specified                     |

#### Boolean literals

The literal `true` can be used as a synonym for `1'b1`, and the literal
`false` can be used as a synonym for `1'b0`, but otherwise `true` and
`false` have no special meaning.

#### String literals

The language accepts string literals enclosed in double quotes `"`. Alogic does
not support interpreting strings as a bit vector. String literals should only be
used as arguments to builtin functions that require a string argument,
e.g. `$display`:

```
$display("Help!");
```

<p align="center">
<a href="widths.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ports.md">Next</a>
</p>