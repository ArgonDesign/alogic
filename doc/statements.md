<p align="center">
<a href="fsms.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="control.md">Next</a>
</p>

# FSM statements

This is a comprehensive reference of all statements available in `fsm`
functions.

### Combinatorial vs Control statements

Statements can broadly be categorized as either combinatorial statements, or
control statements. This distinction is used to determine control unit
boundaries for the purposes of determining which statements execute together in
a single clock cycle. Let it suffice for now that a linear sequence of
combinatorial statements, followed by a single control statement is executed in
a single clock cycle. See the section on [control flow conversion](control.md)
for details.

- Simple statements (i.e. statements that do not contain nested statements) can
be lexically classed either as control statements or combinatorial statements.

- Compound statements (i.e. statements containing other nested statements) can
be either combinatorial or control statements.

    - If they only contain other combinatorial statements they will be
    combinatorial statements.
    
    - If they contain a mix of combinatorial and control statements, they will
    be control statements. In this case, the last nested statement must be a
    control statement.

FSM function bodies must end with a control statement.

### Declaration statement (combinatorial)

A declaration statement can be used to introduce a new variable to be used
within the surrounding lexical scope. It could be declared in the entity scope
and used by all functions, or declared within an individual function.
Declaration statements start with a type name, followed by the variable
identifier, optionally followed by `=` and an initializer expression, and end in
`;`:

<a href="http://afiddle.argondesign.com/?example=statements_declaration.alogic">Fiddle with these declarations here.</a>

```
  u8 a;          // Declare 8 bit unsigned integer variable 'a',
                 // but do not initialize it.
  i16 b = -'sd2; // Declare 16 bit signed integer variable 'b',
                 // and initialize it to -2.
  foo_t bar;     // Declare variable 'bar' of type 'foo_t', where 'foo_t' is either
                 // a typedef or the name of a struct.
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

A declaration statement can be qualified with the `const` keyword.
`const` qualified declaration statements behave exactly the same as those
without the `const` qualifier, except they cannot be modified in any way after
the declaration. This also means that an initializer is required for `const`
qualified declaration statements. Note that `const` qualified declaration
statements are a different concept than
[constant declarations](params.md#constants) as their value can change at
run-time for every evaluation of the declaration statement. They will also
require storage if their value is used in a control unit other than the one
introducing the declaration.

### Block statement (combinatorial or control)

A `{}` block can be used to introduce a new lexical scope at any time. This can
be useful to scope local variable declarations, of group statements together for
clarity.

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

### Expression statements (combinatorial)

Expressions can be used as statements when ending in `;`. Since expression
statements do not return a value, they should only be used for their
side-effects (see examples below). The compiler will signal an error if a pure
expression (i.e. one with no side-effects) is used in statement position.

<a href="http://afiddle.argondesign.com/?example=statements_expression.alogic">Fiddle with these expressions here.</a>

```
  p_in.read();        // Legal, as '.read()' has a side-effect of reading
                      // the input port, although the read value is discarded.

  p_out.write(1'b1);  // Legal, writing to a port is a side-effect

  a + b;              // Compiler error: This is a pure expression with no
                      // side-effects.
```

Expression statements are always combinatorial statements. Note that function
calls in statement positions are not expression statements and are described
below.

### Assignment statements

An assignment statement updates the value of some storage location. All
assignment statements are combinatorial statements.

#### Simple assignments (combinatorial)

The simplest assignment statements have the usual form, using the `=` sign to
delimit the target of the assignment (lvalue), and the expression that yields
the value to be assigned. Similarly to the Verilog language, the left hand side
of an assignment can be either one of:

- Simple identifier, e.g. `foo = 2'd2;`

- Indexed identifier: `foo[idx]` e.g. `foo[3] = 1'b1;` or `foo[a] = 3'd2;` 

- Identifier with range (a slice):
    - `foo[msb:lsb]` e.g. `foo[10+:7] = 4'd3;`
    - `foo[msb -: width]` e.g. `c[10 -: 4] = 4'd3;`
    - `foo[lsb +: width]` e.g. `c[7 +: 4] = 4'd10;` 
  
- Structure member access, e.g. `foo.bar = 4'd9;`

- Unpacking assignment (concatenation of lvalues): `{foo, {bar[idx], baz.x}}` - <a href="http://afiddle.argondesign.com/?example=statements_unpacking.alogic">fiddle here:</a>
    ```
    u10 a;
    u2[3] b;
    bool[3] c;
    {a, b[1], c[3]} = 13'h1abc; // 10 bits + 2 bits + 1 bit
    ```

#### Shorthand assignments (combinatorial)

All binary operators are available in the shorthand assignment form, including
when the target is a concatenation or other compound lvalue (<a href="http://afiddle.argondesign.com/?example=statements_shorthand.alogic">fiddle here</a>):

```
  a >>= 2;
  b[2] -= a;
  {sign, abs} += 1;
```

#### Increment/Decrement statements (combinatorial)

As a further shorthand, increment or decrement by 1 can be expressed using the
`++` and `--` notation. Note however that these operations are not expressions,
but proper statements (they do not yield a value), and as such can only be used
when standing alone in statement position. All lvalues are valid.

```
  a++;
  {sign, abs}--;
```

### The `fence` statement (control)

The `fence` statement is the simplest control statement, and is used to indicate
the end of a control unit. All combinatorial statements before a `fence`
statement will belong to the current control unit (which also includes the
`fence` statement itself), and will execute in the current clock cycle. On the
next clock cycle, control is transferred to the statements following the `fence`
statement. The following example takes 2 cycles to execute (<a href="http://afiddle.argondesign.com/?example=statements_fence.alogic">fiddle here</a>):

```
  a = b + c;
  fence;
  d = a + e;
  fence;
```

### Branching statements

Control flow branches can be achieved with the `if` and `case` statements. These
branching statements are combinatorial statements if all branches contain only
combinatorial statements, and they are control statements if all branches
contain (and in particular, end in) control statements. If one or more branches
contains a control statement, but not all branches end in a control statement,
the branch statement is invalid and yields a compile time error.

#### `if` statement (combinatorial or control)

The common `if` statement can be used to perform a 2-way branch:

```
  if (condition) <then-statement> else <else-statement>
```

The else clause is optional, and omitting the else clause results in an implicit
fence, as follows (<a href="http://afiddle.argondesign.com/?example=statements_if.alogic">fiddle here</a>):

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

Some legal examples are (<a href="http://afiddle.argondesign.com/?example=statements_if2.alogic">fiddle here</a>):

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

  // Different branches can contain different numbers of control units
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
    fence;
  } else {
    c = p_in_0.read();
    fence;
  }

  // This if statement has an implicit else where nothing happens
  if (a) {
    b = 2;
  }

  // This if statement has an implicit else containing a single fence
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
    fence;
  }
```

Some invalid examples are (<a href="http://afiddle.argondesign.com/?example=statements_invalid.alogic">fiddle here</a>):

```
  // Invalid because 'if' is control and 'else' is combinatorial
  if (a) {
    b = 2;
    fence;
  } else {
    c = 2;
  }

  // Invalid because control block must end in a control statement
  if (a) {
    b = p_in_0.read();
    fence;
    b += p_in_0.read();
  }
```

#### `case` statement (combinatorial or control)

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

The selectors can be:
- single selectors or comma-separated lists
- values, constant expressions or variable-expressions (unlike the C `switch`)
- `default`

The selectors are considered in a top-to-bottom order, and if the condition
expression is equal to the selector, the statement is executed and no further
selectors are checked. This means overlapping selectors are legal. For example 
(<a href="http://afiddle.argondesign.com/?example=statements_case.alogic">fiddle here</a>):

```
  // Assume foo is u3
  case (foo) {
    3'd0, 3'd1, 3'd2: a = 0;
    bar + 3'd1: a = 1;
    default: a = 2; // could be placed anywhere in the list
  }
```

Case clauses can contain arbitrarily complex statements using a `{}` block:

```
  case (foo) {
    bar: {
      // code to execute if foo == bar
    }
    baz: {
      // code to execute if foo == baz
    }
    default: {
      // code to execute otherwise
    }
  }
```

The case statement can either have an explicit or implicit `default` case to
catch remaining cases. If used explicitly, it can be placed anywhere in the
selector list and is always evaluated last (this is the same as verilog). If
used implicitly, it can be omitted and the compiler will insert a default empty
combinatorial block or `fence;` control block, as appropriate. For example 
(<a href="http://afiddle.argondesign.com/?example=statements_case_fence.alogic">fiddle here</a>):

```
  case (foo) {
    bar: {
      a++;
      fence;
    }
    baz: {
      b++;
      fence;
    }
  }
```

is compiled as:

```
  case (foo) {
    bar: {
      a++;
      fence;
    }
    baz: {
      b++;
      fence;
    }
    default: fence;
  }
```

### Function calls

Functions are used to encapsulate repetitive portions of FSM behaviour. All
statements relating to function call handling are control statements.

#### The call statement (control)

To end the current control unit, and transfer control to a function on the next
clock cycle, simply call it in statement position 
(<a href="http://afiddle.argondesign.com/?example=statements_call.alogic">fiddle here</a>):

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

#### `return` statement (control)

The `return` statement can be used to end the control unit and transfer control
back to the call site for the next clock cycle. As mentioned in the description
of [FSMs](fsms.md), functions will return automatically when they reach the
end of the function body (<a href="http://afiddle.argondesign.com/?example=statements_return.alogic">fiddle here</a>).

```
  void foo() {
    bar();  // Call 'bar',  when it returns, return to the caller of 'foo'.
  }

  void bar() {
    return; // Return to call site
  }
```

#### `goto` statement (control)

The `goto` statement can be used to perform a tail call to a function. This
statement ends the current control unit, transfers control to the target
function, but does not push a return stack entry, and hence the callee will
return to the site of the preceding function call. One use of `goto` is to
eliminate wasted cycles where there is no work to be done other than returning
to an outer function (<a href="http://afiddle.argondesign.com/?example=statements_goto.alogic">fiddle here</a>):

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
reduced to a single cycle by using `goto`, causing _c_ to return directly to the
call site of _b_ inside _a_:

```
  void b() {
    goto c;
  }
```

### Looping statements

All statements in this section are control statements. The bodies of all loops
must be `{}` blocks, even if they contain only a single statement.

#### The fundamental `loop` statement (control) - (<a href="http://afiddle.argondesign.com/?example=statements_loop.alogic">fiddle here</a>)

The fundamental looping construct is the infinite loop, introduced with the
`loop` keyword. The body of a `loop` must end in a control statement. To exit
the infinite loop, use the `break`, `return`, or `goto` statements:

```
u8 acc = 0;

loop {
  acc ^= p_in.read();
  if (acc == 0)
    break; // an implicit 'else fence;' is inserted by the compiler
}
```

The `loop` keyword ends the current control unit and introduces the loop body,
so the above code would take 1 clock cycle to perform the initialization of
_acc_ and enter the loop, and from then on the loop body would execute once
every cycle (assuming no flow control stalls on p_in), until _acc_ becomes 0.

#### Structured loops

Structured `do`, `while`, and `for` loops are syntactic sugar and are rewritten
by the compiler in terms of the primitive `loop` statement. When determining the
cycle behaviour of these structured loops, consider their rewriting. Note in
particular that an implicit `fence` is always inserted at the end of the loop
body and so does not need to be written.

#### `do` loop (control) - (<a href="http://afiddle.argondesign.com/?example=statements_do.alogic">fiddle here</a>)

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

#### `while` loop (control) - (<a href="http://afiddle.argondesign.com/?example=statements_while.alogic">fiddle here</a>)

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

#### `for` loop (control) - (<a href="http://afiddle.argondesign.com/?example=statements_for.alogic">fiddle here</a>)

For loops follow the common syntax:

```
  for (<init> ; <cond> ; <step>) {
    <body>
  }
```

where _\<init>_ can be a list of zero or more instances of either assignment
statements or simple variable declarations with initializers separated by comma,
_\<cond>_ is an optional expression, _\<step>_ is a list of zero or more comma
separated assignment statements, and _\<body>_ is a list of statements. The
rewriting of a `for` loop in terms of `loop` is:

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

#### `break` statement (control)

The `break` statement can be used to immediately terminate the innermost active
loop and transfer control to the statement following the loop on the next clock
cycle.

#### `continue` statement (control)

The `continue` statement can be used similarly to the C language equivalent to
continue at the end of the loop body. This means that inside a `do` or `while`
loop, the `continue` statement performs the condition check, and on the next
clock cycle transfers control either to the beginning of the loop body, or the
statement after the loop. Inside a `for` loop, the `continue` statement also
executes the _\<step>_ statements before performing the condition check. Inside
a `loop` loop, `continue` unconditionally transfers control to the beginning of
the loop body on the next cycle.

#### `let` headers (control)

The `let` keyword can be used to introduce a list of variable declarations
together with initializers (separated by `,`) to a new scope established by a
following loop statement:

```
  let (<init>) <stmt>
```

The form of _\<init>_ is the same as in the case of the `for` loop. The
_\<stmt>_ following the `let` header must be a `loop`, `do`,`while` or `for`
statement. The `let` statement is syntactic sugar for:

```
  {
    <init>
    <body>
  }
```

The canonical use case is to aid with `do` loops to construct the equivalent of
a rear-testing `for` loop (<a href="http://afiddle.argondesign.com/?example=statements_let.alogic">fiddle here</a>):

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

<p align="center">
<a href="fsms.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="control.md">Next</a>
</p>
