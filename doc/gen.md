<p align="center">
<a href="import.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ffi.md">Next</a>
</p>

# Compile time code generation

Alogic supports compile time code generation using the `gen` construct, which is
similar to the Verilog generate construct.

The compiler will process all `gen` constructs to produce expanded source code
prior to compilation. `gen` constructs are expanded during
[parameter specialization](params.md#parameter-specialization) but before global
type checking or control flow conversion. This allows `gen`
constructs to depend on actual parameter values and produce arbitrary code
fragments that are syntactically valid in the context of the `gen`
construct.

The constructs are introduced with examples producing Alogic statements,
but `gen` constructs can be used to produce various language fragments. The
places where `gen` constructs can be used are described
[later](#where-gen-constructs-can-appear) in this section.

Lexical scopes introduced by `gen` constructs can be named on unnamed. `gen`
scopes have special lexical rules which allow certain names declared inside
`gen` blocks to escape into the scope containing the `gen` construct. This
makes `gen` constructs more powerful and is
explained [later](#lexical-scopes-of-gen-constructs) in this section.

#### Simple conditionals with `gen if`

The simplest `gen` construct is the conditional `gen if`, which can be used to
conditionally include some source content. Braces around the branches are
required. The condition must be a 1-bit wide value. The `else`
or `else if` branches are optional:

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

This is equivalent to one of the following depending on the actual parameter
value:

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

Observe that the then branch of the `gen if` is a combinational statement, while
the else branch is a control statement, which cannot be expressed with a
simple `if` statement, even if the condition is a constant known at compilation
time.

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

The `gen for` loop has the same syntax as the standard `for` statement, except
for the following restrictions:

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
    bool b = p_i;
    gen for (uint N = 0 ; N < P ; N++) {
      b = ~b;
    }
    p_o = b;
    fence;
  }
}
```

Note that the above `gen for` yields combinational statements and is equivalent
to the following if P is say 3:

```
fsm silly_inverter {
  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_in;
    b = ~b;
    b = ~b;
    b = ~b;
    p_o = b;
    fence;
  }
}
```

The variables declared in the `gen for` headers are available as constants in
the body:

```
fsm adding {
  param uint P = 3;
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_i;
    gen for (u8 N = 0 ; N <= P ; N++) {
      x += N;
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
    u8 x = p_i;
    x += 8'd0;
    fence;
    x += 8'd1;
    fence;
    x += 8'd2;
    fence;
    x += 8'd3;
    fence;
    p_o.write(x);
    fence;
  }
}
```

#### Ranged `gen for`

As using `gen for` with an incrementing variable is common, there is a shorthand
syntax for writing this:

```
gen for (uint N < 4) {
  ...
}
```

This yields the body of the construct with N set to incrementing values between
0 and 3 inclusive. The end value can be made inclusive by using
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

The only case when a ranged `gen for` is not equivalent to a standard `gen for`
is when the loop variable would overflow, causing an infinite loop. The
ranged `gen for` prevents this and will terminate after setting the loop
variable to the largest positive representable value. As an example the
following would iterate 8 times, with 'i' set to the values 0 to 7 inclusive:

```
gen for (u3 i < 8) {
  ...
}
```

#### Nesting `gen` constructs

`gen` constructs can be arbitrarily nested. Inner `gen` constructs can depend on
variables defined in outer gen constructs:

```
fsm faster_adding {
  param uint P;
  in  u8 p_i;
  out u8 p_o;

  void main() {
    u8 x = p_i;
    gen for (u8 N = 0 ; N <= P ; N++) {
      x += N;
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
    x += 8'd0;
    x += 8'd1;
    fence;
    x += 8'd2;
    x += 8'd3;
    fence;
    x += 8'd4;
    x += 8'd5;
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
    gen if (SLOW) {
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

Generating entity contents:

```
network optional_buffer {
  param bool BUFFERED;
    
  in  sync bool i;
  out sync bool o;
    
  gen if (BUFFERED) {
    // This will cause 'o' to have an output register by default
    // acting as a buffer and causing a one cycle delay
    new fsm delay {
      void main() {
        o.write(i);
        fence;
      }
    }
  } else {
    // Wire straight through
    i -> o;
  }
}
```

Generating `struct` contents:

```
struct unpacked_vector {
  uint param N;
  gen for (uint n < N) {
    u8 element#[N];
  }    
}
```

In file scope:

```
gen if (debug) {
  import bool checker(u8 a, u8 b);
}
```

### Dictionary identifiers

To enhance the capabilities of `gen` constructs, especially `gen` loops, Alogic
supports what we refer to as dictionary identifiers. Dictionary identifiers are
dependent names, where the name of a symbol (identifier) depends on a sequence
of values. Dictionary identifiers use a simple base identifier, followed by a
comma separated list of one or more compile time constant indices enclosed in
`#[` `]`.

Dictionary identifiers can be used anywhere where a regular identifier is
expected. This means that we could define two one bit variables as:

```
bool variable#[0];
bool variable#[1];
```

The name of the first variable is `variable#[0]` and the name of the second
variable is `variable#[1]`, we could just as easily have called them `a` and
`b`. Apart from the form of the name, there is no other difference between
simple identifiers and dictionary identifiers.

What gives dictionary identifiers their power is the ability to use any compile
time constant expression as actual indices. The following generates 8 `bool`
variables with names `a#[0]` to `a#[7]`:

```
gen for (uint N < 8) {
  bool a#[N]; 
}
```

The indices must be unique for each copy of the definition, but can otherwise be
arbitrary (including sparse indices):

```
network reverse {
  param uint N;
  gen for (uint n < N) {
    in  bool i#[n];
    out bool o#[n];
    i#[n] -> o#[N - 1 - n];
  }
}
```

Each dictionary identifier definition may be unique, there is no required
relations between definitions introducing dictionary identifiers with the same
base name:

```
network even_wide_odd_narrow_and_a_spare {
  param uint N;
  static assert N % 2 == 0, "N must be even"; 
  gen for (uint n = 0 ; n < N ; n += 2) {
    in  u4 i#[n];     // Even numbered ports are 4 bit wide
    in  u2 i#[n + 1]; // Odd numbered ports are 2 bit wide
    out u4 o#[n];
    out u2 i#[n + 1];
    i#[n] -> o#[n];
    i#[n + 1] -> o#[n + 1];
  }

  // Additional special port is 1 bit wide
  in  bool i#[-1];
  out bool o#[-1];
  i#[-1] -> o#[-1];
}
```

Here is an example of the power of the mechanism to generate a pipelined binary
tree of adders to sum a number of values:

```
network dictident_adder_tree {
  param uint INPUTS; // Number of inputs (must be power of 2)
  param uint IWIDTH; // Width of each input

  const uint LEVELS = $clog2(INPUTS);

  const uint OWIDTH = IWIDTH + LEVELS;

  gen for (uint n < INPUTS) {
    in uint(IWIDTH) p_i#[n];
  }
  out uint(OWIDTH) p_o;

  fsm adder {
    param uint IW;
    in uint(IW) a;
    in uint(IW) b;
    out uint(IW+1) s;

    void main() {
      s = 'a + 'b;
      fence;
    }
  }

  gen for (uint level < LEVELS) {
    gen for (uint n < (INPUTS >> level + 1)) {
      add#[level, n] = new adder(IW = IWIDTH + level);
      gen if (level == 0) {
        p_i#[2*n + 0] -> add#[level, n].a;
        p_i#[2*n + 1] -> add#[level, n].b;
      } else {
        add#[level - 1, 2*n + 0].s -> add#[level, n].a;
        add#[level - 1, 2*n + 1].s -> add#[level, n].b;
      }
    }
    gen if (level == LEVELS-1) {
      add#[level, 0].s -> p_o;
    }
  }
}
```

### Generating interface symbols

It is possible to generate any definition in a `gen` construct, including
`param`, `const`, `in` and `out` definitions. The generated definitions can in
turn depend on actual parameter values:

```
struct bus {
  u20 addr;
  u8  data;
}

fsm gen_decoder {
  param uint N_PORTS;

  in sync bus i;
  gen for (uint n < N_PORTS) {
    out u8 o#[n];
    param u20 BASE#[n];
  }

  void main() {
    i.read();
    case(i.addr) {
      gen for (uint n < N_PORTS) {
        BASE#[n]: o#[n].write(i.data);
      }
      default: {} // Do nothing
    }
    fence;
  }
}
```

To instantiate the above with 3 ports, use:

```
decoder = gen_decoder(N_PORTS=3, BASE#[0]=2, BASE#[1]=10, BASE#[2]=150);
```

### Lexical scopes of `gen` constructs

All examples so far have used unnamed `gen` scopes. The `{}` block scopes
introduced by a `gen` construct can be given explicit name if desired. For
`gen if` conditionals, the scope name follows the initial `if` condition:

```
gen if (a) : scopename {
  ...
} else if (b) {
  ...
} else {
  ...
}
```

In `gen` loops, the scope name follows the loop header:

```
gen for (...) : scopename {
  ...
}
```

The `{}` block scopes introduced by `gen` constructs have special lexical rules,
which affect the visibility of names declared in these scopes. The rules are
slightly different for `gen if` conditionals and `gen` loops, and for named or
unnamed scopes. The following sections detail the rules in full.

#### Scoping rules for `gen if` conditionals

All names introduced inside _unnamed_ `gen if` scopes will be inserted into the
enclosing lexical scope. This allows for example to use `gen if` to change types
of definitions based on compile time conditions:

```
gen if (SIGNED) {
  i8 a;
  i8 b;
  i16 c;
} else {
  u8 a;
  u8 b;
  u16 c;
}
...
c = 'a * 'b; // Signed or unsigned variables depending on SIGNED
```

This mechanism of escaping the `gen if` scopes works with any definition that
introduces a name (including type, entity, instance or function definitions), so
the above is equivalent to the following:

```
gen if (SIGNED) {
  typedef i8 i_t;
  typedef i16 o_t;
} else {
  typedef u8 i_t;
  typedef u16 o_t;
}
i_t a; // i_t and o_t are signed or unsigned depending on SIGNED
i_t b;
o_t c;
...
c = 'a * 'b;
```

Names introduced inside a _named_ `gen if` scope will not be automatically
inserted into the enclosing scope, but can be accessed via a hierarchical
reference:

```
gen if (SIGNED) : types {
  typedef i8 i_t;
  typedef i16 o_t;
} else {
  typedef u8 i_t;
  typedef u16 o_t;
}
types.i_t a;
types.i_t b;
types.o_t c;
...
c = 'a * 'b;
```

Semantically, an unnamed `gen if` is equivalent to the same `gen if` named with
a temporary name, followed by a wildcard `using` directive, except that the
temporary name is not accessible:

```
gen if (...) {
  ...
}

// Is the same as the following, except 'tmpname' is not visible:

gen if (...) : tmpname {
  ...
}

using tmpname.*;
```

#### Scoping rules for `gen` loops

Simple names defined in `gen` loops have a separate copy for each iteration.
Within the loop body, references are resolved to the symbols created in the
current iteration.

```
fsm scoping {
  param uint P;

  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_i;
    gen for (uint N = 0 ; N <= P; N++) {
      bool c = ~b; // Separate copy of 'c' in each iteration.
      p_o.write(c);
      fence;
    }
  }
}
```

Assuming the parameter value is 2, the above is equivalent to the following
ignoring variable names:

```
fsm scoping__P_2 {
  in  bool p_i;
  out bool p_o;

  void main() {
    bool b = p_i;
    bool c__0 = ~b;
    p_o.write(c__0);
    fence;
    bool c__1 = ~b;
    p_o.write(c__1);
    fence;
    bool c__2 = ~b;
    p_o.write(c__2);
    fence;
  }
}
```

Names introduced using dictionary identifiers (but not simple identifiers)
inside _unnamed_ `gen` loop scopes will be inserted into the enclosing lexical
scope:

```
network swap {
  gen for (uint n < 2) {
    in  bool i#[n];
    out bool o#[n];
  }
  i#[0] -> o#[1];
  i#[1] -> o#[0];
}
```

Names introduced inside _named_ `gen` loop scopes will not be inserted into the
enclosing lexical scope. The definitions are still accessible via a hierarchical
reference, but the visibility of names depends on whether the
`gen` loop scope is named with a simple or a dictionary identifier. In
particular, names introduced inside `gen` loop scopes named with a simple
identifier are only accessible if using a dictionary identifier:

```
gen for (uint n < 10) : s {
  bool tmp = i ^ j;
  bool a#[n] = tmp;
  bool b#[n] = ~tmp;
}

bool x = s.a#[0]; // Ok: 'a#[0]' is accessible in scope 's' 
bool y = s.b#[0]; // Ok: 'b#[0]' is accessible in scope 's' 
bool z = s.tmp;   // Error: 'tmp' is not accessible in scope 's'
```

All names introduced inside a `gen` loop scope are accessible via a hierarchical
reference if the name of the `gen` loop scope uses a dictionary identifier:

```
gen for (uint n < 10) : s#[n] {
  bool tmp = i ^ j;
  bool a = tmp;
  bool b = ~tmp;
}

bool x = s#[0].a;   // Ok: 'a' is accessible in scope 's#[0]' 
bool y = s#[0].b;   // Ok: 'b' is accessible in scope 's#[0]' 
bool z = s#[0].tmp; // Ok: 'tmp'  is accessible in scope 's#[0]'
```

<p align="center">
<a href="import.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ffi.md">Next</a>
</p>
