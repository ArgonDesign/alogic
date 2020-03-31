<p align="center">
<a href="srams.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="builtins.md">Next</a>
</p>

# Assertions

Alogic support some two kinds of simple assertion constructs, immediate
assertion statements, and static assertions.

### Assertion statements

Assertion statements are written with the `assert` keyword, and take a
condition expression, optionally followed by a string used as the error message
if the assertion fails:

```
  assert x > 0;                       // Check x is positive non-zero
  assert x > 0, "x is not positive";  // Same with above, with explicit message
```

### Semantics of assertion statements

Assertion statements are similar to, but not equivalent to Verilog immediate
assertions. In particular, every Alogic assertions statement will evaluate at
most once per clock cycle in a synchronous fashion, and will not evaluate their
condition if the containing `fsm` is stalled. Take for example:

```
fsm a {
  in sync u8 i;

  void main() {
    i.read();
    assert i >= 8'd10;
    fence;
  }
}
```

The assertion in the above will only fail if `i` is both valid and has a value
less than 10, but will never fail when `i` is not valid.

The Alogic compiler by default will synthesize Alogic assertion statements
into the output. This can be suppressed by the `--no-assert` compiler option.

#### Using assertion statements during static code analysis

The Alogic compiler will take advantage of information derived from assertion
statements to perform static optimization. This can be used to give the
compiler hints about the properties of the design that otherwise cannot be
determined statically:

```
void main() {
    ...
    assert x == 2;
    ... // No assignments to x
    if (x) {
       ...
    } else {
       ...
    }
    ...
}
```

With the assertion in place and no further assignments possible to `x`, the
compiler can infer that the condition of the if statement is always true, and
will therefore replace the if statement with the contents of the 'then' branch.

The scope of the optimizations and value inference the compiler is capable of
is not defined and is performed on a best effort basis.

### Static assertions

Static assertions are introduced with the `static assert` keywords. As opposed
to assertion statements, static assertions can appear in any context, and
will be checked at compilation time, causing a compile time error if violated.
This makes them useful to check assumptions about values dependent on
parameters:

```
fsm fifo {
  param uint DEPTH;
  static assert DEPTH > 4, "This fifo implementation only works for DEPTH > 4";
  ...
}
```

<p align="center">
<a href="srams.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="builtins.md">Next</a>
</p>
