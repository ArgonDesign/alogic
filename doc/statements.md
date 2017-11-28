# FSM statements

This is a comprehensive reference of all statements available in `fsm`
functions.

## Combinatorial vs Control statements

Statements can broadly be categorized as either combinatorial statements, or
control statements. This distinction is used to determine control unit
boundaries for the purposes of determining which statements execute together
in a single clock cycle. Let it suffice for now that a linear sequence of
combinatorial statements, followed by a single control statement is executed in
a single clock cycle. See the section on [control units](control.md) for
details.

Simple statements that do not contain nested statements can be lexically classed
either as control statements of combinatorial statements. Compound statements
that do contain other statements can be either combinatorial statements, if
they only contain other combinatorial statements, or control statements if they
contain a mix of combinatorial and control statements. If a compound statement
contains a mix of combinatorial and control statements, then the last nested
statement must be a control statement. FSM function bodies must end with a
control statement.

## Simple statements

The simples statements, which do not influence control flow are as follows.

### Declaration statement

A declaration statement can be used to introduce a new variable to be
used within the surrounding lexical scope. Declaration statements start with a
type name, followed by the variable identifier, optionally followed by `=` and
and initializer expression, and end in `;`:
```
  u8 a;       // Declare 8 bit unsigned integer variable 'a',
              // but do not initialize it.
  i16 b = -2; // Declare 16 bit signed integer variable 'b',
              // and initialize it to -2;
  foo_t bar;  // Declare variable 'bar' of type 'foo_t', where 'foo_t' is either
              // a typedef or the name of a struct
```
Note that while this looks like a local variable, the storage is in fact
statically allocated. This means that care must be taken when using local
variables in recursive functions. The C language equivalent of the declaration
of variable _b_ above is:
```c
  static int16_t b;
  b = -2;
```
Array variables cannot be declared using declarations statements, they must be
declared in the design entity scope.

Declaration statements are always combinatorial statements.

### Block statement

A `{}` block can be used to introduce a new lexical scope at any time. This
can be useful to scope local variable declarations, of group statements together
for clarity.
```
  a = 2;
  {
    u8 tmp = a ^ c;
    b = tmp + tmp << 2;
  }
  c = b;
```

A block statement is either a combinatorial statement or a control statement,
depending on its contents.

### Expression statements

Any expression can be used as a statement when followed by a `;`. Expressions
in statement position can only achieve anything through heir side-effect. The
compiler will signal an error if a pure expression is used in statement
position.
```
  p_in.read();        // Valid, as '.read()' has a side-effect of reading
                      // the input port. The read value is discarded.

  p_out.write(1'b1);  // Valid, writing to a port is a side-effect

  a + b;              // Compiler error: This is a pure expression with no
                      // side-effects.
```

Expression statements are always combinatorial statements. Note that function
calls in statement positions are not expression statements and are described
below.

### `let` statement

The `let` statement can be used to introduce a list of variable declarations
together with initializers (separated by `,`) to a new scope created by a
following statement:
```
  let (declarations-with-initializers>) <body>
```
The `<body>` following the `let` header must be either a `{}` block, or a
`loop`, `do`,`while` or `for` statement. The `let` statement is syntactic sugar
for:
```
  {
    <declarations-with-initializers>
    <body>
  }
```

The canonical use case is to aid with `do` loops to construct the equivalent of
a rear testing `for` loop:
```
  // Loop 8 times using a 3 bit loop variable
  let (u3 i = 0) do {
    foo[i] = 0;
    i++;
  } while (i);
```

This is equivalent to:
```
  {
    u3 i = 0;
    do {
      foo[i] = 0;
      i++;
    } while (i);
  }
```

To determine whether a `let` statement is a combinatorial or controls
statements, consider the corresponding rewriting.

## Assignment statements

An assignmet statement updates the value of some storage location. All
assignment statements are combinatorial statements.

### Simple assignments

The simplest assignment statements hab the usual form, using the `=` sign to
delimit the target of the assignment (lvalue), and the expression that yields
the value to be assigned. Similarly to the Verilog language, the left hand side
of an assignment can be either one of:
- Simple identifier: `foo`
- Indexed identifier: `foo[idx]`
- Identifier with range: `foo[hi:lo]` (`foo[hi-:w]` and `foo[lo+:w]` are also
  supported)
- Structure member access: `foo.bar`
- Concatenation formed for other valid lvalues (also known as unpacking
  assignment): `{foo, {bar[idx], baz.x}}`

Here are some examples:
```
  a = 2'd2;
  b[a] = 3'd2;
  c[a+:4] = 8'd42;
  d.bar = 4'd9;
  {a, b[1], c[3]} = 13'h1abc;
```

### Shorthand assignments

All binary arithmetic operators are available in the shorthand assignment form,
including when the target is a concatenation or other compound lvalue:
```
  a >>= 2;
  b[2] -= a;
  {sign, abs} += 1;
```

### Increment/Decrement statements

As a further shorthand, increment or decrement by 1 can be expressed using the
`++` and `--` notation. Note however that these operations are not expressions,
but proper statements (they do not yield a value), and as such can only be used
when standing alone in statement position. All lvalues are valid.
```
  a++;
  {sign, abs}--;
```

## The `fence` statement

The `fence` statement is the simplest control statement, and is used to indicate
the end of a control unit. All combinatorial statements before a `fence`
statement will belong to the current control unit (which also includes the
`fence` statement itself), and will execute in the current clock cycle. On the
next clock cycle, control is transferred to the statements following the `fence`
statement. The following example takes 2 cycles to execute:
```
  a = b + c;
  fence;
  d = a + e;
  fence;
```

## Branching statements

Control flow branches can be achieved with the `if` and `case` statements. These
branching statements are combinatorial statements if all branches contain only
combinatorial statements, and they are control statements if all branches
contain only control statements. Mixing combinatorial and control branches in
the same statement is invalid and yields a compiler error.

### `if` statement

The common `if` statement can be used to perform a 2-way branch:
```
  if (condition) <then-statement> else <else-statement>
```
The else clause is optional. If a control `if` statement does not contain an
else clause, then the compiler automatically inserts a single `fence` statement.
Which is to say that:
```
  if (cond) {
    a = 2;
    b = 3;
    fence;
  }
```
is compiled as:
```
  if (cond) {
    a = 2;
    b = 3;
    fence;
  } else {
    fence;
  }
```

Some legal examples are:
```
  // Combinatorial if statement:
  if (a) {
    b = 2;
  } else {
    c = 3;
  }

  // Control if statement:
  if (a) {
    b = p_in_0.read();
    fence;
  } else {
    c = p_in_0.read();
    fence;
  }

  // The above is the same as:
  if (a) {
    b = p_in_0.read();
  } else {
    c = p_in_0.read();
  }
  fence;

  // But the following does not have a simple equivalent:
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
    fence;
  } else {
    c = p_in_0.read();
    fence;
  }
  fence;

  // Combinatorial if without else clause:
  if (a) {
    b = 2;
  }

  // Control if without else clause:
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
    fence;
  }
```

Some invalid examples are:
```
  // Invalid due to mismatched combinatorial/control branches
  if (a) {
    b = 2;
    fence;
  } else {
    c = 2;
  }

  // Invalid: Control block must end in a control statement
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
  }
```

### `case` statement

Multi-way branches can be constructed using the `case` statement. This multi-way
branch is more similar to the analogous Verilog `case` statement, and less
similar to the C `switch` statement. The general syntax is:
```
  case (<cond>) {
   <case-clauses>
  }
```
Where each case clause is of the form:
```
  <selector> : <statement>
```
There can be a single `default` selector. Other selectors are comma separated
lists of expressions, which are evaluated in a top to down order, and the
statements of the first clause with a selector equal to the condition expression
are executed. As opposed to the C `switch` statement, the selectors do not need
to be constant expressions:
```
  // Assume foo is an u3
  case (foo) {
    3'd0, 3'd1, 3'd2: a = 0;
    bar + 1: a = 1;
    default: a = 2;
  }
```
Case clauses can contain arbitrarily complex statements using a block:
```
  case (foo) {
    bar: {
      // If foo == bar
    }
    baz: {
      // If foo == baz
    }
    default: {
      // Otherwise
    }
  }
```

## Function calls

Functions are used to encapsulate repetitive portions of FSM behaviour. All
statements relating to function call handling are control statements.

### The call statement

To end the current control unit, and transfer control to a function on the next
clock cycle, simply call it in statement position:
```
  void foo() {
    ...
    bar();  // Transfer control to function 'bar'
    ...
    fence;
  }

  void bar() {
    ...
  }
```

### `return` statement

The `return` statement can be used to end the control unit and transfer control
back to the call site on the next clock cycle. As mentioned in the description
of [FSMs](fsms.md), functions do not return automatically when they reach the
end of the function body. Without a `return` statement, control is transferred
back to the top of the function.

```
  void foo() {
    bar();  // Call 'bar',  when it returns, loop back to the top of 'foo'.
  }

  void bar() {
    return; // Return to call site
  }
```

### `goto` statement

The `goto` statement can be used to perform a tail call to a function. This
statement ends the current control unit, transfers control to the target
function, but does not push a return stack entry, and hence the callee will
return the the site of the preceding function call. The purpose of `goto` is
to eliminate wasted cycles where there is no work to be done other than
returning to an outer function:

```
  void a() {
    b();
  }

  void b() {
    c();
    return;
  }

  void c() {
    return;
  }
```

The body of function _b_ in the example above takes 2 cycles to execute, one
cycle to perform the call, and one cycle to perform the return. This can be
reduced to a single cycle by using `goto`, causing _c_ to return directly to
the call site of _b_ inside _a_:
```
  void b() {
    goto c;
  }
```

## Looping statements

All statements in this section are control statements. The bodies of all loops
must be `{}` blocks, even if they contain only a single statement.

### The fundamental `loop` statement

The fundamental looping construct is the infinite loop, introduced with the
`loop` keyword. The body of a `loop` must end in a control statement. To exit
the infinite loop, use the `break`, `return`, or `goto` statements:
```
u8 acc = 0;

loop {
  acc ^= p_in.read();
  if (acc == 0)
    break;
}
```

The `loop` keyword ends the current control unit and introduces the loop body,
so the above code would take 1 clock cycle to perform the initialization of
_acc_ and enter the loop, and from then on the loop body would execute once
every cycle (assuming no flow control stalls on p_in), until _acc_ becomes 0.

### Structured loops

Structured `do`, `while`, and `for` loops are syntactic sugar and are rewritten
by the compiler in terms of the primitive `loop` statement. When determining the
cycle behaviour of these structured loops, consider their rewriting. Given that
the rewritings introduce control statements after the loop body, structured
loops need not have a control statement at the end of their body.

#### `do` loop

The common rear testing `do` loop is written as:
```
  do {
    <body>
  } while (<cond>);
```
where _\<body>_ is a list of statements, and _\<cond>_ is an expression.
This is rewritten by the compiler to:
```
  loop {
    <body>
    if (<cond>) {
      fence;
    } else {
      break;
    }
  }
```

#### `while` loop

The syntax of the front testing `while` loop is as follows:
```
  while (<cond>) {
    <body>
  }
```
where _\<cond>_ is an expression, and _\<body>_ is a list of statements.
This is rewritten by the compiler to:
```
  if (<cond>) {
    loop {
      <body>
      if (<cond>) {
        fence;
      } else {
        break;
      }
    }
  }
```

#### `for` loop

For loops follow the common syntax:
```
  for (<init> ; <cond> ; <step>) {
    <body>
  }
```
where _\<init>_ is either a single assignment statement or a single variable
declaration with an initializer expression, _\<cond>_ is an expression,
_\<step>_ is an assignment statement, and _\<body>_ is a list of statements.
The rewriting of a `for` loop in terms of `loop` is:
```
  {
    <init>;
    if (<cond>) {
      loop {
        <body>
        <step>;
        if (<cond>) {
          fence;
        } else {
          break;
        }
      }
    }
  }
```

### `break` statement

The `break` statement can be used to immediately terminate the innermost active
loop and transfer control to the statement following the loop on the next
clock cycle.
