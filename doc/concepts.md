
### Building blocks

The Alogic language uses a few key concepts for the description of digital
circuits. We aim to give a quick and incomplete overview here to pique the
interest of the adopter.

### Design entities

The fundamental design unit is called a [design entity](entities.md).
Entities are analogous to Verilog modules. They can be instantiated inside other
entities to build a design hierarchy.

A fundamental entity would be a [Finite State Machine](fsms.md), which is
described using code with locally sequential semantics similar to common
software programming languages using function calls, conditionals and other
common control structures:

```
fsm foo {
  void main() {
    u8 a = ...;
    u8 b = a + 7;
    if (b[2:0] == 3'b0) {
      doit();
    }
  }

  void doit() {
    ...
  }
}
```

The above FSM `foo` would acquire an 8 bit unsigned value `a` from some source,
add 7 to it to get an 8 bit unsigned value `b`, and depending on whether `b` is
divisible by 8 would proceed to do some further action.

### Ports

Communication between entities happens through well defined interfaces called
[ports](ports.md). A port carries some (usually registered) payload signals,
and optionally some flow control signals with well defined semantic.

A simple example of a module that, on every clock cycle, reads an 8 bit value
from an input port using a valid-ready handshake and adds a quasi-static value
to it is as follows:

```
fsm add {
  in sync ready u8 p_in;
  in u8 addend;

  out sync ready u8 p_out;

  void main() {
    u8 sum = p_in.read() + addend;
    p_out.write(sum);
    fence;
  }
}
```
