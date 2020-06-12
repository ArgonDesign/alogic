<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ffi.md">Next</a>
</p>

# Compile time code generation

Alogic supports compile time code generation using the `gen` construct,
which is similar to the Verilog generate construct.

The compiler will process all `gen` constructs to produce expanded
source code prior to compilation. `gen` constructs are expanded during
[parameter specialization](params.md#parameter-specialization) but
before global type checking or control flow conversion. This allows `gen`
constructs to depend on actual parameter values and produce arbitrary
code fragments that are syntactically valid in the context of the `gen`
construct.

The constructs are introduced with examples producing Alogic statements,
but `gen` constructs can be used to produce various language fragments.
The places where `gen` constructs can be used are described
[later](#where-gen-constructs-can-appear) in this section.

Note that `gen` construct have special lexical scoping rules which allow
names declared inside `gen` blocks to escape into the scope containing
the `gen` construct. This makes `gen` constructs more powerful and is
explained [later](#lexical-scopes-of-gen-constructs) in this section.

#### Simple conditionals with `gen if`

The simplest `gen` construct is the conditional `gen if`, which can be
used to conditionally include some source content. Braces around the
branches are required. The condition must be a 1-bit value. The `else`
branch is optional
(<a href="http://afiddle.argondesign.com/?example=gen_if.alogic">fiddle here</a>):

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

Observe that the then branch of the `gen if` is a combinational
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

<a href="http://afiddle.argondesign.com/?example=gen_for.alogic">Fiddle with this code here.</a>

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

Note that the above `gen for` yields combinational statements and is
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
    p_o = b;
    fence;
  }
}
```

The variables declared in the `gen for` headers are available as
constants in the body
(<a href="http://afiddle.argondesign.com/?example=gen_for_var.alogic">fiddle here</a>):

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

As using `gen for` with an incrementing variable is common, there is a
shorthand syntax for writing this
(<a href="http://afiddle.argondesign.com/?example=gen_range.alogic">fiddle here</a>):

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
representable on the declared type
(<a href="http://afiddle.argondesign.com/?example=gen_range_i3_8.alogic">fiddle here</a>):

```
gen for (u3 i < 8) {
  ...
}
```

When this happens, the loop variable is set to all values between 0 and
the most positive value representable on the declared type (i.e.: the
loop variable of the ranged `gen for` will never overflow). In in the
above example, `i` would be set to values 0 to 7 inclusive.

#### Nesting `gen` constructs

`gen` constructs can be arbitrarily nested. Inner `gen` constructs can
depend on variables defined in outer gen constructs
(<a href="http://afiddle.argondesign.com/?example=gen_nested.alogic">fiddle here</a>):

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

Where a statement is expected (except in `for` loop headers)
(<a href="http://afiddle.argondesign.com/?example=gen_statement.alogic">fiddle here</a>):

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

Generating case clauses
(<a href="http://afiddle.argondesign.com/?example=gen_case.alogic">fiddle here</a>):

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

Generating entity contents
(<a href="http://afiddle.argondesign.com/?example=gen_entity.alogic">fiddle here</a>):

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

It is possible to generate `param` and `const` declarations, including
declarations dependent on actual values of other parameters:

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

The `{}` block scopes introduced by `gen` constructs are special and
introduce what we will refer to as _weak lexical scopes_ (as opposed to
regular lexical scopes introduced by other language constructs). Names
introduced inside weak scopes can escape into the enclosing scope. The
rules are slightly different for `gen if` conditionals and `gen` loops,
and are described in the following sections.

#### Scoping rules for `gen if` conditionals

All names introduced inside `gen if` constructs will be inserted into
the enclosing regular lexical scope. This allows for example to use `gen
if` to change types of definitions based on compile time conditions
(<a href="http://afiddle.argondesign.com/?example=gen_if_scope.alogic">fiddle here</a>):

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

This mechanism of escaping the `gen if` scopes works with any
declarations or definitions that introduces a name (including type,
entity, instance or function definitions), so the above is equivalent to
the following:

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

References within the weak scope of a `gen if` will always refer to
names within the same scope if those names are introduced within the
scope. The compiler however will issue an error when `gen if` constructs
yield missing or ambiguous definitions of names escaping the weak scope,
if those names are referred to outside of the introducing weak scope.
This means that the following would be valid:

```
gen if (A) {
    bool x = ...
    a = x;    
}
gen if (B) {
    bool x = ... 
    b = x;
}
```

While the following would raise an ambiguous definition error if both
`A` and `B` are true, or a missing definition error if both `A` and `B`
are false
(<a href="http://afiddle.argondesign.com/?example=gen_if_ambiguous_error.alogic">fiddle here</a>):

```
gen if (A) {
    bool x = ...
}
gen if (B) {
    bool x = ... 
}
c = x; // might be an error if A and B both have the same truth value
```

Note that any names defined inside a regular scope which is inside a
weak scope will not escape the enclosing scope, so the following is
invalid:

```
gen if (SIGNED) {{
  i2 a;
}} else {{
  u2 a;
}}
a = 0;  // ERROR: 'a' is not defined.
```

#### Scoping rules for `gen` loops

Simple names defined in `gen` loops have a separate copy for each
iteration and do not escape the weak scope of the `gen` loop. Within the
loop body, references are resolved to the symbols created in the current
iteration
(<a href="http://afiddle.argondesign.com/?example=gen_loop_simple_names.alogic">fiddle here</a>).

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

Assuming the parameter value is 2, the above is equivalent to the
following ignoring variable names:

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

Directly within the weak scopes of `gen` loops, _dictionary identifiers_
can be used when introducing names to disambiguate between iterations.
Dictionary identifiers use a simple base identifier, followed by a comma
separated list of one or more compile time constant indices enclosed in
`#[` `]`. The following generates 8 `bool` variables with names `a#[0]`
to `a#[7]`
(<a href="http://afiddle.argondesign.com/?example=gen_loop_dict_ident.alogic">fiddle here</a>):

```
gen for (uint N < 8) {
  bool a#[N]; 
}
```
 
The indices must be unique for each copy of the definition, but can
otherwise be arbitrary (including sparse indices). Dictionary
identifiers (and only dictionary identifiers) escape the weak scope of
the `gen` loop, and can be referred to with the same syntax within the
enclosing regular scope
(<a href="http://afiddle.argondesign.com/?example=gen_loop_dict_ident_escape.alogic">fiddle here</a>):

```
network connect_2 {
  gen for (uint n < 2) {
    in  bool i#[n];
    out bool o#[n];
  }
  
  i#[0] -> o#[0];
  i#[1] -> o#[1];
}
```

Dictionary identifiers can also be used to reference definitions across
loop iterations as well
(<a href="http://afiddle.argondesign.com/?example=gen_loop_dict_ident_across.alogic">fiddle here</a>):

```
network connect_N_backwards {
  param uint N;
  
  gen for (uint n < N) {
    in  bool i#[n];
    out bool o#[n];
    i#[n] -> o#[N - 1 - n];
  }  
}
```

It is an error to have more than one lexical definition of the same name
within the same `gen` loop scope, even if the dictionary indices are
unique
(<a href="http://afiddle.argondesign.com/?example=gen_multiple_names_error.alogic">fiddle here</a>):

```
gen for (uint n < 8) {
  bool a#[0, n];
  bool a#[1, n]; // ERROR: redefinition of 'a'
}
```

It is also an error to have multiple `gen` loops introducing the same
name, even if the dictionary indices are unique, if the name is referred
to from the enclosing scope:

```
gen for (uint n < 8) {
  bool a#[0, n];
}
gen for (uint n < 8) {
  bool a#[1, n];
}
a#[A, B] = ... // ERROR: 'a' is ambiguous
```

Remember however that references within the loop are resolved locally so
the following is OK, so long as there are no external references to `a`
(<a href="http://afiddle.argondesign.com/?example=gen_multiple_names_local.alogic">fiddle here</a>):

```
gen for (uint n < 8) {
  bool a#[n];
  gen if (n == 0) {
    a#[n] = x;
  } else {
    a#[n] = ~a#[n-1];
  }  
  gen if (n == 7) {
    b = a#[n];
  }
}

gen for (uint n < 8) {
  bool a#[n];
  gen if (n == 0) {
    a#[n] = x;
  } else {
    a#[n] = ~a#[n-1];
  }  
  gen if (n == 7) {
    c = a#[n];
  }
}
```

When working out how dictionary identifiers are resolved, it might be
helpful to think of names introduced with a dictionary identifier as a
single symbol, which is introduced by the corresponding lexical
definition. This means that an ambiguity error will be raised if there
are more than one lexical definitions active while resolving a
reference.

Here is an example of the power of the mechanism to generate a pipelined
binary tree of adders to sum a number of values
(<a href="http://afiddle.argondesign.com/?example=gen_adder_tree.alogic">fiddle here</a>):

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

<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="ffi.md">Next</a>
</p>
