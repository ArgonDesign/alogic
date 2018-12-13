<p align="center">
<a href="statements.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="expr.md">Next</a>
</p>

# Control flow conversion

One of the major features of Alogic is the ability to describe the behaviour of
design entities using code with sequential semantics, which executes over
multiple clock cycles. The compiler converts the sequential code describing
entity behaviour into a state system implemented as a traditional FSM extended
with a return stack to handle [function calls](statements.md#function-calls).

This section provides details of how the Alogic compiler translates sequential
code to the state system.

### Summary of statement types

As described in the documentation of [statements](statements.md), every Alogic
statement can be one of two types: A control statement, or a combinatorial
statement.

Some statements are unambiguously control statements. These are listed here for
reference:

- `fence` statement
- `goto` statement
- `return` statement
- `break` statement
- `continue` statement
- `for` loop statement
- `do` loop statement
- `while` loop statement
- `loop` loop statement

Loops with a `let` clause are control statements in the same way as the loops
themselves.

Some statements are unambiguously combinatorial statements. These are listed
here:

- Assignment statement
- Increment/decrement statement
- Variable declaration statement
- Pipeline `read` statement
- Pipeline `write` statement

The remaining statements can be either of a combinatorial or control type,
depending on circumstance.

Within an entity, a function call to another control function is always a
control statement. Any other expression when used in a statement position is a
combinatorial statement, including calls to built-in functions, port methods and
similar accessors:

<a href="http://afiddle.argondesign.com/?example=control_summary.alogic">Fiddle with this code here.</a>

```
fsm a {
  in sync bool b;

  void main() {
    b.read(); // This is a combinatorial statement
    other();  // This is a control statement
  }

  void other() {
    fence;
  }
}
```

At this point we have discussed all statements, except `{}` block statements
that simply group other statements, and conditional `if` and `case` statement.
We will leave these for later as it makes more sense to consider them in the
context of other statements.

### Which statements execute together in a single cycle

Statements that executes with the same clock cycle are referred to as a control
unit. Working out which statements belong to the same control unit is simple
using only a small number of rules. These are demonstrated through examples
here.

#### Simple straight-line statements

To consider what executes together in a single cycle, follow this **basic
procedure**:

Start at a control function entry point and look down the list of statements in
the function body until you encounter a control statement. If you have
encountered a simple control statement (that is a `fence`, control function
call, `goto`, or `return` statement) then, everything up to and including that
control statement executes in the same cycle. You can then repeat the procedure
starting at the statement following this control statement, respecting the
control transfer performed by control function calls, `goto` and `return`
statements:

<a href="http://afiddle.argondesign.com/?example=control_simple.alogic">Fiddle with this code here.</a>

```
void main() {
  a++;          // | Cycle 1
  b++;          // | Cycle 1
  c.read();     // | Cycle 1
  fence;        // V Cycle 1 ends

  d.write();    // | Cycle 2
  e--;          // | Cycle 2
  goto foo;     // V Cycle 2 ends - Cycle 3 would start at the first
                //                  statement of control function 'foo'
}
```

#### Blocks

If you followed the basic procedure above, and you encountered a naked `{}`
block, simply keep going inside the block. If the block is a combinatorial
block, you will arrive at the bottom within the same cycle:

<a href="http://afiddle.argondesign.com/?example=control_comb_blocks.alogic">Fiddle with this code here.</a>

```
void main() {
  a++;          // | Cycle 1
  b++;          // | Cycle 1
  c.read();     // | Cycle 1
                // |
  {             // |
    f++;        // | Cycle 1
    g++;        // | Cycle 1
  }             // |
  fence;        // V Cycle 1 ends

  d.write();    // | Cycle 2
  e--;          // | Cycle 2
  goto foo;     // V Cycle 2 ends
}
```

If the block is a control block, the current cycle ends and a new cycle begins
within the block:

<a href="http://afiddle.argondesign.com/?example=control_ctrl_blocks.alogic">Fiddle with this code here.</a>

```
void main() {
  a++;          // | Cycle 1
  b++;          // | Cycle 1
  c.read();     // | Cycle 1
                // |
  {             // |
    f++;        // | Cycle 1
    g++;        // | Cycle 1
    fence;      // V Cycle 1 ends

    d.write();  // | Cycle 2
    e--;        // | Cycle 2
    goto foo;   // V Cycle 2 ends
  }
}
```

#### Branching statements

If the basic procedure takes you to a branching `if` or `case` statement,
continue down across all branches in parallel. If the branching statement is a
combinatorial statement, you will arrive at the statement following this
branching statement within the same cycle:

<a href="http://afiddle.argondesign.com/?example=control_comb_branch.alogic">Fiddle with this code here.</a>

```
void main() {
  a++;          //  | Cycle 1
  b++;          //  | Cycle 1
  c.read();     //  | Cycle 1
                //  |
  if (a) {      // / \
    f++;        // | . Cycle 1 if 'a' is true
    g++;        // | . Cycle 1 if 'a' is true
  } else {      // . .
    h++;        // . | Cycle 1 if 'a' is false
    i++;        // . | Cycle 1 if 'a' is false
  }             // \ /
                //  |
  fence;        //  V Cycle 1 ends

  d.write();    // | Cycle 2
  e--;          // | Cycle 2
  goto foo;     // V Cycle 2 ends
}
```

As you would expect, at execution time only the statements across the taken
branch direction will be executed.

If the branching statement is a control statement, the current cycle ends and a
new cycle begins within the taken branch:

<a href="http://afiddle.argondesign.com/?example=control_ctrl_branch.alogic">Fiddle with this code here.</a>

```
void main() {
  a++;          //  | Cycle 1
  b++;          //  | Cycle 1
  c.read();     //  | Cycle 1
                //  |
  if (a) {      // / \
    f++;        // | . Cycle 1 if 'a' is true
    g++;        // | . Cycle 1 if 'a' is true
    fence;      // V . Cycle 1 ends if 'a' is true
  } else {      //   .
    h++;        //   | Cycle 1 if 'a' is false
    i++;        //   | Cycle 1 if 'a' is false
    fence;      //   V Cycle 1 ends if 'a' is false
    j++;        //   | Cycle 2 if 'a' was false
    k++;        //   | Cycle 2 if 'a' was false
    fence;      //   V Cycle 2 ends if 'a' was false
  }             //

  d.write();    // | Cycle 2 if 'a' was true, and Cycle 3 if 'a' was false
  e--;          // | Cycle 2 if 'a' was true, and Cycle 3 if 'a' was false
  goto foo;     // V Cycle 2 ends if 'a' was true, and Cycle 3 ends if 'a' was false
}
```

Remember that for control branches, an implicit `fence` statement is [inserted
by the compiler](statements.md#branching-statements) for an omitted `else`, or
`default` clause.

#### Looping statements

If the basic procedure took you to a looping statement, then the decision
whether or not to enter the loop is performed together with the preceding
combinatorial statements, but the current cycle always ends at the loop
header (<a href="http://afiddle.argondesign.com/?example=control_loop.alogic">fiddle here</a>):

```
void main() {
   a++;        // | Cycle 1
   b++;        // | Cycle 1
   c.read();   // | Cycle 1
               // |
   loop {      // V Cycle 1 - A 'loop' is always entered, but the cycle ends
     f++;      // | Cycle 2
     g++;      // | Cycle 2
     break;    /  V Cycle 2 ends - Iterate only once for this example
   }

   d.write();  // | Cycle 3
   e--;        // | Cycle 3
   goto foo;   // V Cycle 3 ends
}
```

A `break` statement ends the current cycle and transfers execution to the
statement following the loop.

Similarly, here is an example with a front-testing loop (<a href="http://afiddle.argondesign.com/?example=control_front_testing.alogic">fiddle here</a>):

```
void main() {
  a++;         // | Cycle 1
  b++;         // | Cycle 1
  c.read();    // | Cycle 1
               // |
  while (h) {  // V Cycle 1 - Test 'h' to decide loop entry, the cycle ends
    f++;       // | Cycle 2 if 'h' was true
    g++;       // | Cycle 2 if 'h' was true
    h = false; // V Cycle 2 ends if 'h' was true - Iterate once for this example
  }

  d.write();   // | Cycle 3 if 'while' loop was entered, otherwise Cycle 2
  e--;         // | Cycle 3 if 'while' loop was entered, otherwise Cycle 2
  goto foo;    // V Cycle 3 ends if 'while' loop was entered, otherwise Cycle 2 ends
}
```

On loop exit, control is transferred to the statement following the loop on the
subsequent cycle. It might help to understand the execution of loop bodies
better if you consider the [rewritings](statements.md#structured-loops) of `do`,
`while` and `for` loops in terms of the fundamental `loop`.

Assignments in `for` loop initializers and `let` clauses are performed on the
same cycle as the decision to enter the loop. Again, if it helps, consider the
rewritings.

### Loop header optimization

The reason the current cycle always ends upon loop entry is because a new FSM
state must be introduced in order to be used as the target of the loopback at
the end of the loop body. Consider the following `do` loop, which is designed to
executes twice (<a href="http://afiddle.argondesign.com/?example=control_opt_do.alogic">fiddle here</a>):

```
  u2 i = 2'd0;          // | Cycle 1
                        // |
  do {                  // V Cycle 1 ends, 'do' loop is always entered
    a++                 // | Cycle 2 (i == 0)         | Cycle 3 (i == 1)
    i++;                // | Cycle 2 (i becomes 1)    | Cycle 3 (i becomes 2)
  } while (i < 2'd2);   // V Cycle 2 end - loopback   V Cycle 3 ends - loop exit

  b++;                  // | Cycle 4
  fence;                // V Cycle 4
```

For non-front-testing loops (i.e.: for `do` and `loop` loops), an optimization
is possible if the statement immediately preceding the loop is a control
statement. In this case, the loopback can target the state beginning after the
preceding control statement, as there are no combinatorial statements in that
sate at that point. The Alogic compiler performs this optimization, meaning that
the following executes in 3 cycles, rather than 4 (<a href="http://afiddle.argondesign.com/?example=control_opt_for.alogic">fiddle here</a>):

```
void main() {
  a++;      // | Cycle 1
  other();  // V Cycle 1 ends - Cycle 2 begins at control function 'other'

  loop {    // - Due to the loop header optimization, Cycle 3 does not end here
    b++;    // | Cycle 3
    break;  // V Cycle 3 ends here
  }
}

void other() {
  b++;      // | Cycle 2
  return;   // V Cycle 2 ends - Cycle 3 begins at the call site
}
```

Also note that due to this optimization, the `fence` statement in the following
is redundant, and the state machine executes the same with or without it (<a href="http://afiddle.argondesign.com/?example=control_opt_fence.alogic">fiddle here</a>):

```
void main() {
  a++;      // | Cycle 1
  b++       // | Cycle 1
  fence;    // V Cycle 1 ends

  loop {    // - Loop header optimization: No new state required
    c++;    // | Cycle 2
    break;  // V Cycle 2 ends
  }
}
```

This optimization is not possible if there is any combinatorial statement
between the loop statement and the preceding control statement, so the `fence`
in this is executed, and the new cycle then ends on loop entry (<a href="http://afiddle.argondesign.com/?example=control_no_opt.alogic">fiddle here</a>).

```
void main() {
  a++;      // | Cycle 1
  b++       // | Cycle 1
  fence;    // V Cycle 1 ends

  d++;      // | Cycle 2

  loop {    // V Cycle 2 ends - no loop header optimization possible
    c++;    // | Cycle 3
    break;  // V Cycle 3 ends
  }
}
```

For front-testing loops, this optimization is never possible, as they require
combinatorial statements before the loop header in order to perform conditional
entry.

<p align="center">
<a href="statements.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="expr.md">Next</a>
</p>
