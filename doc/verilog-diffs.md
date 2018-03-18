# Notable differences from Verilog

### Integer literals

Naked integer literals (e.g.: `0` or `1`) are considered to have an infinite
number of bits, and are either coerced to the bit width required by their use,
or a compiler error is issued.

Naked integer literals in Alogic and are unsigned. In Verilog they are signed.

Syntax for signed integer literals uses `s` as a suffix, rather than standing
between the `'` and the base specifier.
