# Literal values

### Integer literals

Literal values of integer type in Alogic are the same as in the Verilog
language. In particular, Alogic supports both sized/unsized, and signed/unsigned
integers:

The generic syntax for sized integer literals is:
- number of bits
- followed by `'`
- optionally followed by `s` for signed values
- followed by a base specifier `b` for binary `d` for decimal,
  `h` for hexadecimal
- followed by digits compatible with base, optionally delimited by `_`
  characters

Or in regular expression notation:
```
[0-9]+'s?[bdh][0-9a-fA-F_]+
```

Unsized literals can be written by omitting the number of bits specifier from
the above schema.

Note that as opposed to Verilog, upper case base specifiers and the `o` octal
base specifier are not supported.

Simple numbers e.g. `12` are considered unsized, and signed.

Combining sizedness and signedness, there are 4 different kinds of integer
literals in the language, all of which behave the same as their Verilog
counterparts:

```
  2'd3          // Sized, unsigned, width: 2, value: 3
  8'sh7f        // Sized, signed, width: 8, value: 127
  'b1000_0000   // Unsized, unsigned, width: context dependent, value: 128
  'sd42         // Unsized, signed, width: context dependent, value: 42
  32            // Unsized, signed, width: contet dependent, value: 32
```

### String literals

The language accepts string literals enclosed in double quotes `"`. Alogic
does not support string types, or interpreting strings as a bit vector. String
literals should only be used as arguments to Verilog system functions that
require a string argument, e.g. `$display`:

```
$display("Help!");
```
