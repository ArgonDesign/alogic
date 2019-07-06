<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="interop.md">Next</a>
</p>

# Compile time code generation

Alogic supports compile time code generation using the `gen` construct,
which is similar to the Verilog generate construct.

The compiler will process all `gen` constructs to produce expanded
source files prior to compilation. `gen` constructs are expanded after
[parameter specialization](params.md#parameter_specialization) but
before type checking or control flow conversion. This allows `gen`
constructs to depend on final parameter values and produces arbitrary
code fragments that are syntactically valid.

The constructs are introduced with examples producing Alogic statements,
but `gen` constructs can be used to produce various language fragments.
The places where `gen` constructs can be used are described later in
this section.

#### Simple conditionals with `gen if`

The simplest `gen` construct is the conditional `gen if`, which can be
used to conditionally include some source content. Braces around the
branches are required. The `else` branch is optional:

```
fsm delay_or_inverter {
  param bool P;
  in  bool p_i;
  out bool p_o;

  void main() {
    gen if (P) {
      p_o = ~p_i;
    } else {
      fence;
      fence;
      p_o = p_i;
    }
    fence;
  }
}
```

This is equivalent to one of the following depending on the actual
parameter value:

```
// If 'P' is true:
fsm inverter {
  in  bool p_i;
  out bool p_o;

  void main() {
    p_o = ~p_i;
    fence;
  }
}

// If 'P' is false
fsm delay {
  in  bool p_i;
  out bool p_o;

  void main() {
    fence;
    fence;
    p_o = p_i;
    fence;
  }
}
```

Observe that the then branch of the `gen if` is a combinatorial
statement, while the else branch is a control statement, which cannot be
expressed with a simple `if` statement, even if the condition is a
compile time constant.

Multi-way branches can be expressed with the `else if` syntax:

```
gen if (P == 0) {
  ...
} else if (P == 1) {
  ...
} else if (P == 2) {
  ...
} else {
  ...
}
```

#### Looping with `gen for`

The `gen for` loop has the same syntax as the standard `for` statement,
except for the following restrictions:
- Only declaration initializers are allowed in the initializer list
- At least one declaration initializer must be present
- The stop condition must be present
- At least one step statement must be present

```
fsm invert_a_lot {
  param uint P;
  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_in;
    gen for (uint N = 0 ; N < P ; N++) {
      b = ~b;
    }
    fence;
  }
}
```

Note that the above `gen for` yields combinatorial statements and is
equivalent to the following if P is say 3:

```
fsm silly_inverter {
  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_in;
    b = ~b;
    b = ~b;
    b = ~b;
    fence;
  }
}
```

The variables declared in the `gen for` headers are available as
constants in the body:

```
fsm adding {
  param uint P;
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_in;
    gen for (u8 N = 0 ; N <= P ; N++) {
      b += N;
      fence;
    }
    p_o.write(x);
    fence;
  }
}
```

Which for P set to 3 is the same as:

```
fsm adding__P_3 {
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_in;
    b += 8'd0;
    fence;
    b += 8'd1;
    fence;
    b += 8'd2;
    fence;
    b += 8'd3;
    fence;
    p_o.write(x);
    fence;
  }
}
```

#### Ranged `gen for`

As using `gen for` with an incrementing variable is common, there is a
shorthand syntax for this case:

```
gen for (uint N < 4) {
  ...
}
```

This yields the body of the construct with N set to incrementing values
between 0 and 3 inclusive. The end value can be made inclusive by using
`<=` instead of `<`:

```
gen for (uint N <= 4) { // N is set to 0, 1, 2, 3, and then 4
  ...
}
```

The ranged `gen for` is usually equivalent to the standard `gen for`
with the following rewriting:

```
// Ranged 'gen for'
gen for (<type> <name> <op> <end>) {
  <body>
}

// Equivalent standard 'gen for'
gen for (<type> <name> = 0 ; <name> <op> <end> ; <name>++) {
  <body>
}
```

The only case when a ranged `gen for` is not equivalent to a standard
`gen for` is when the end value is larger than the most positive number
representable on the declared type:

```
gen for(u3 i < 8) {
  ...
}
```

When this happens, the loop variable is set to all values between 0 and
the most positive value representable on the declared type (i.e.: the
loop variable of the ranged `gen for` will never overflow). In in the
above example, `i` would be set to values 0 to 7 inclusive.

#### Nesting `gen` constructs

`gen` constructs can be arbitrarily nested. Inner `gen` constructs can
depend on variables defined in outer gen constructs:

```
fsm faster_adding {
  param uint P;
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_in;
    gen for (u8 N = 0 ; N <= P ; N++) {
      b += N;
      gen if (N % 2 && N != P) {
        fence;
      }
    }
    p_o.write(x);
    fence;
  }
}
```

Assuming P is 6, this is equivalent to:

```
fsm faster_adding__P_6 {
  param uint P;
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_in;
    b += 8'd0;
    b += 8'd1;
    fence;
    b += 8'd2;
    b += 8'd3;
    fence;
    b += 8'd4;
    b += 8'd5;
    p_o.write(x);
    fence;
  }
}
```

#### Where `gen` constructs can appear

A `gen` construct can appear in any of the following positions.

Where a statement is expected (except in `for` loop headers):

```
fsm toggle {
  param bool SLOW;

  out bool p_o = false;

  void main() {
    gen if (P) {
      fence;
    }
    p_o = ~p_o;
    fence;
  }
}
```

Generating case clauses:

```
fsm twiddle {
  param uint P;

  in  u8 p_i;
  out u8 p_o;

  void main() {
    case (p_i) {
      gen for (u8 N = 1; N < P; N++) {
        N : p_o.write(p_i ^ N);
      }
      default: p_o.write(p_i);
    }
    fence;
  }
}
```

### Variable scopes

Any name defined inside a `gen` construct have a scope limited to the
defining `gen` construct.

```
gen if (P) {
  u8 a = ...;
  p_out.write(a); // Uses 'a' above
}

u8 b = a; // 'a' is undefined
```

Furthermore, names defines in `gen` loops have a separate copy for each
iteration, with a scope limited to the defining iteration:

```
fsm scoping {
  param uint P;

  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_i;
    gen for (uint N = 0 ; N <= P; N++) {
      bool b = ~b;  // Redefinition of outer 'b' on every iteration,
                    // initialized to the outer 'b' on every iteration
      p_o.write(b); // Will always write ~p_i;
      fence;
    }
  }
}
```

Assuming the parameter value is 2, the above is equivalent to the
following ignoring variable names:

```
fsm scoping__P_2 {
  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_i;
    bool b0 = ~b;
    p_o.write(b0);
    fence;
    bool b1 = ~b;
    p_o.write(b1);
    fence;
    bool b2 = ~b;
    p_o.write(b2);
    fence;
  }
}
```
<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="interop.md">Next</a>
</p>
