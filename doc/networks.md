<p align="center">
<a href="expr.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="pipelines.md">Next</a>
</p>

# Networks

### Networks for defining a design hierarchy

In contrast to FSMs, which are the abstraction for describing the behaviour of
sequential logic, networks are used to define design hierarchies by declaring
instances of entities and the interconnections between them.

Networks can be declared similar to other entities, using the `network` keyword,
followed by the name of the network, followed by the description of the network
in curly braces.

```
network a {
  ...
}
```

### Network contents

As the purpose of networks is to define hierarchies of instances and their
interconnections, they can only contain the following contents.

#### Network variable declarations

A network can contain a list of declarations at the beginning of the network
body. Given that there is no state system associated with a network, local
variables or memories cannot be declared in networks. A network can
fundamentally only declare the following:

- input ports
- output ports
- parameters
- constants

Networks can also declare `pipeline` variables, which are used as an abstraction
for describing pipelined data paths. These are described in their on
[chapter](pipelines.md).

#### Instantiations

Instantiating an entity can be done using the `new` keyword, with the following
assignment style syntax:

```
<name-of-instance> = new <name-of-entity>(<paremeter-assignments>);
```

_\<name-of-instance>_ and _\<name-of-entity>_ are identifiers specifying the
name of the instance being created, and the name of the entity being
instantiated.

_\<parameter-assignemts>_ is a comma separated list of `<parameter-name> =
<expression>` concrete parameter specifications, which is used to override the
default parameter values declared in the entity being instantiated. See the the
documentation of [parameters](params.md) as well.

An example instantiation that creates an instance named `bar` of an entity named
`foo` would be simply:

```
bar = new foo();
```

Similarly, to instantiate a parametrized entity called `fifo` with a particular
width an depth, one would write:

```
fifo_i = new fifo(WIDTH=32, DEPTH=512);
```

### Port connections

Ports of instances created as described in the previous section are connected
using the `->` operator:

```
a.p_out -> b.p_in;
```

This creates a combinatorial connection between the left and right hand sides,
of the `->`, with the left hand side port being the driver of the right hand
side port. Alogic takes care of connecting flow control signals in the
appropriate direction.

Ports of the enclosing network are connected using the same syntax:

```
a.p_out -> p_out;
```

In the above `a` is an instance, `p_out` is the output port declared in the
entity of which `a` is an instance of, and `p_out` is a port declared in the
same entity that contains the `->` connection.

Ports connected using `->` must use matching flow control. This means that ports
without flow control can only be connected to ports also not using flow control,
`sync` ports can only be connected to other `sync` ports, and `sync ready` ports
can only be connected to other `sync ready` ports. The compiler will issue an
error if this is violated.

Source ports that use no flow control, or use `sync` flow control, can be
connected to multiple sink ports, using a comma separated list of sinks on the
right hand side of the `->`:

```
settings.width -> b.width, c.width, d.width;
```

Connections of `sync ready` ports must be one to one, otherwise the compiler
will issue an error.

#### Nested FSMs

Networks can also contain nested `fsm` definitions, which can be instantiated in
the enclosing network:

```
network foo {
  fsm bar {
    ...
  }

  bar_0 = new bar();
  bar_1 = new bar();
}
```

This can be useful when defining a circuit composed of multiple smaller, but
tightly interconnected FSMs, and is also used as the abstraction for
[pipelines](pipelines.md).

#### Singleton nested FSMs

It is a common pattern to define a nested FSM in a network and then create a
single instance of it. Alogic provides syntactic sugar for defining just such a
singleton nested entity, by using the `new` keyword before the nested
definition:

```
network foo {
  new fsm bar {
    ...
  }
}
```

This is syntactic sugar for:

```
network foo {
  fsm bar {
    ...
  }
  bar = new bar();
}
```

### Accessing variables in enclosing entity

Nested FSMs can directly access the input and output ports of the enclosing
entity, as well the `param` and `const` values declared in the enclosing entity.
While nested entities will be emitted by the compiler as separate modules,
external ports referenced by the nested entity will be automatically wired
through to the nested entity:

```
network foo {
  in  sync ready bool p_i;
  out sync ready bool p_o;

  new fsm bar {
    void main() {
      p_o.write(p_in.read());
      fence;
    }
  }
}
```

This will be compiled the same way as:

```
network foo {
  in  sync ready bool p_i;
  out sync ready bool p_o;

  new fsm bar {
    in  sync ready bool p_i;
    out sync ready bool p_o;

    void main() {
      p_o.write(p_in.read());
      fence;
    }
  }

  p_i -> bar.p_i;
  bar.p_o -> p_o;
}
```

`param` and `const` values declared in the enclosing entity and referenced in a
nested entity will be copied into the nested entity.

<p align="center">
<a href="expr.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="pipelines.md">Next</a>
</p>
