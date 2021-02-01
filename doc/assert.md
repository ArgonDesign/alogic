<p align="center">
<a href="srams.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="builtins.md">Next</a>
</p>

# Assertions

Alogic support some simple assertion constructs: immediate assertion
statements, the `unreachable` statement and static assertions.

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

Assertions with conditions that the compiler can prove to be holding based on
information available at compilation time will be dropped. Similarly,
assertions conditions that are known at compilation time to fail will raise a
compiler error. The remaining assertions (i.e.: those which cannot be proven or
disproven by the compiler) will be synthesized into assertion statements in the
output. Synthesis of unknown assertions can be suppressed by the `--no-assert`
compiler option.

#### Using assertion statements during static code analysis

The Alogic compiler will take advantage of information derived from assertion
statements to perform static optimization.  This can be used to give the
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
compiler can infer that the condition of the `if` statement is always true, and
will therefore replace the if statement with the contents of the 'then' branch.

The scope of the optimizations and value inference the compiler is capable of
is not defined and is performed on a best effort basis.

### The `unreachable` statement

We mentioned above that assertion statements will be checked by the compiler if
their condition can be evaluated at compilation time. This means that an
assertion statement such as `assert false;` will always raise a compiler error.

It is sometimes useful to be able to mark code paths that are not supposed to
be executed. Alogic provides the `unreachable` statement for this purpose. An
example is a case statements over a variable that is known to hold only sparse
values:

```
u8 x = ...
case (x) {
  10, 20: {
    ...
  }
  30 : {
    ...
  }
  default : unreachable;
}
```

Similarly to assertions, the `unreachable` statement can have an optional
string argument which provides an error message.

```
unreachable "Shouldn't have reached this";
```

As opposed to assertion statements, an `unreachable` statement is considered a
control statement inside of control functions.

Like assertion statements, the compiler will drop `unreachable` statements that
it can statically prove to be unreachable. Similarly, `unreachable` statements
that are known to be reached will raise a compiler error. The remaining
`unreachable` statements will be synthesized into assertion statements in the
output, unless disabled by the `--no-assert` compiler option.

### Static assertions

Static assertions are introduced with the `static assert` keywords. As opposed
to assertion statements, static assertions can appear in any context, and will
be checked at compilation time, causing a compile time error if violated.  This
makes them useful to check assumptions about values dependent on parameters:

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
