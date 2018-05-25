# Finite State Machines

### FSMs as fundamental building blocks

Finite State Machines are the fundamental abstraction Alogic uses to describe
the sequential behaviour of digital logic. Every Alogic entity describing
sequential logic is eventually emitted as some Verilog modules implementing
FSMs.

FSMs are introduced with the `fsm` keyword, followed by the name of the FSM,
followed by the description of the FSM in curly brackets. The behaviour of an
FSM is described using code with sequential semantics, portions of which
executes on every clock cycle.

### Using functions for encapsulation

FSM code is partitioned into functions. After reset, execution starts in the
`main` function. The description of an FSM that reads a byte from an input port
on every clock cycle, increments its value by 2 and writes the result to an
output port would look as follows:

```
fsm add2 {
  in sync u8 p_in;
  out sync u8 p_out;

  void main() {
    u8 x = p_in.read();
    p_out.wire(x + 8'd2);
    fence;
  }
}
```

Note that functions do not return without an explicit `return` statement. If the
execution reaches the end of the body, control is transferred to the beginning
of the function, and conceptually proceeds in an infinite loop. This behaviour
is distinctly different from common programming languages. The body of the
example FSM above takes 1 cycle to execute, and hence repeats on every clock
cycle.

An FSM that, depending on the state of an input port, would apply one of 2
different kinds of processing could be defined using the following pattern:

```
fsm one_or_the_other {
  in bool p_which;
  ...

  void main() {
    if (p_which) {
      do_true();
    } else {
      do_false();
    }
  }

  void do_true() {
    // Do something
    ...
    return;
  }

  void do_false() {
    // Do something else
    ...
    return;
  }
}
```

### Statements

The body of functions is composed of a list of imperative style statements.
Statements execute sequentially, according to their execution semantics, which
are analogous to similar statements in common imperative programming languages.
Statements can be classed either as combinatorial statements, or control
statements. For a comprehensive list of statements, see the
[statements](statements.md) section of the documentation.

### Execution model

The statements in imperative function bodies can be partitioned into control
units. Each control unit corresponds to an FSM state. On every clock cycle, the
FSM executes the control unit corresponding to the current state, and moves on
to a new state, which may be the same as the current state.

In this section, we will not go into the details of where the precise control
unit boundaries are, but we will mention the `fence` statement to aid with the
examples. For now, let it suffice to say that the `fence` statement is used to
delimit control unit boundaries in straight line code, so any combinatorial
statement between 2 `fence` statements executes in one clock cycle. For the
details of where control unit boundaries are, see the section on [control
units](control.md).

Under various circumstances, the FSM can stall. When a stall occurs, no internal
state of the FSM is updated. Internal state includes.

### Function call model

Function calls within the FSM are tracked using a small hardware return stack.
This ensures that any function can be called from an arbitrary number of call
sites, and the compiler will take care of resuming execution of the correct code
when a function returns. The required size of the return stack is determined
automatically by the compiler, unless the FSM is recursive, as described below.
The theoretically minded reader will note that this function call model means
that Alogic FSMs actually are more akin to pushdown automata in their
capabilities than traditional finite state machines.

### Recursive FSMs and variable allocations

Recursive functions in FSMs are permitted. Describing a directly or indirectly
recursive FSM requires that the user attaches a `reclimit` annotation to the
definition of each recursive function. The value of the `reclimit` attribute
defines how many times that function can be entered on the worst case execution
path, and must be provided by the user. The compiler will use this limit to
compute the minimum size of the return stack:

```
fsm rec {
  u8 i;

  void main() {
    i = 8'd0;
    foo();
  }

  (* reclimit = 4 *)
  void foo() {
    ...
    if (i < 8'd3) {
      i++;
      foo();
    }
    ...
    return;
  }
}
```

It the responsibility of the designer to ensure that the actual program logic of
the FSM does not violate the specified recursion limit.

The size of the return stack can be specified explicitly using the `stacklimit`
annotation attached to the FSM itself. This can be used when the worst case
execution path contains a call stack that is smaller than the statically
computed stack size given only the `reclimit` attributes. The value of the
`stacklimit` variable should be set to the worst case number of active function
calls.

```
(* stacklimit = 5 *)
fsm rec {
  ...
}
```

It is very important to note that all variables declared in a function body use
statically allocated storage. This means that there is only 1 instance of every
variable name declared in the function, even if a function is invoked multiple
times in a recursive call stack. For example, on return to the `main` function,
the following will have `b == 3`, and not `b == 0` as it would be using stack
allocated variables:

```
fsm static_storage {
  u8 i;
  u8 b;

  void main() {
    i = 0;
    foo();
    // At this point b == 3
  }

  void foo() {
    u8 a = i;
    // The C equivalent of the declaration above would be a combination of
    // a static declaration and an assignment statement:
    //   static uint8_t a;
    //   a = i;
    if (i < 3) {
      i += 1;
      foo();
    }
    b = a;
    return;
  }
}
```

### The `fence` block

FSMs definitions can contain a single `fence` block. The `fence` block may only
contain combinatorial statements, and is executed at the beginning of every
cycle, before any of the statements of the control unit corresponding to the
current state are considered. For example:

```
fsm fenceblock {
  u3 s_l2 = 1;
  u8 s;

  fence {
    s = 8'd1 << s_l2;
  }

  void main () {
    ...
    s_l2 = 2;
    fence;

    ...
    s_l2 = 3;
    fence;

    ...
    s_l2 = 4;
    fence;
  }
}
```

Is equivalent to:

```
fsm nofencefunc {
  u3 s_l2 = 1;
  u8 s;

  void main () {
    s = 8'd1 << s_l2;
    ...
    s_l2 = 2;
    fence;

    s = 8'd1 << s_l2;
    ...
    s_l2 = 3;
    fence;

    s = 8'd1 << s_l2;
    ...
    s_l2 = 1;
    fence;
  }
}
```

The `fence` block is particularly useful for computing combinatorial signals
dependent on local state. In the above example, as variable `s` is always
assigned at the beginning of every cycle (it is assigned in the `fence` block
and is not under a conditional), it can be implemented as a combinatorial
signal, with no flip-flop allocated for storing it's value.

### Verbatim blocks

Verbatim text written in the target language can be included in the output
generated by the compiler using a `verbatim` block. The `verbatim` block is
followed by the name of the target language (currently only `verilog` is
supported), and the body of the block is inserted verbatim at the end of
the generated output module.

```
fsm v {
  void main() {
    ...
  }

  verbatim verilog {
    always @(posedge clk) {
      $display("tick");
    }
  }
}
```
