<p align="center">
<a href="compilation.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="types.md">Next</a>
</p>

# Design entities

### Fundamental units

In Verilog the fundamental design unit is a module. In Alogic the analogous
fundamental unit is an design entity, which corresponds to a Verilog module
with a particular kind of internal structure. A design entity compiles into
one or more Verilog modules that form a hierarchy. There are 3 different types
of design entities in the Alogic language:

- [Finite State Machines](fsms.md) (FSMs)
- [Networks](networks.md)
- [Verbatim entities](interop.md#verbatim-entities)

Of these, FSMs and Networks are fundamental types used to construct a design
hierarchy. If one thinks of a design hierarchy as a tree of instantiated
entities, then FSMs are the leaf nodes and Networks are the internal nodes that
instantiate and connect together other networks and FSMs. FSMs implement actual
digital circuits, while networks usually only contain instantiations of other
entities and wiring connections between them.

Verbatim Verilog entities can be used to wrap arbitrary Verilog code into an
Alogic language design entity in order to be instantiated in other Alogic
Networks as sub-modules. Typically verbatim Verilog entities are used to
implement simple glue logic that is not a good fit to be described in the Alogic
language, or to wrap an instantiation of a whole subsystem written in Verilog
to facilitate integration with other Alogic language modules.

### Entity definitions

Each Alogic source file contains the definition of a design entity. FSM
definitions can be nested inside Networks as described in the documentation on
[networks](networks.md#nested-fsms), but otherwise a single source file must
contain a single entity definition at the root file scope.

The Alogic entity source code must have the following structure:
```
<optional type definitions>

<entity keyword> <entity name> {
  <entity description>
}
```

The entity keyword must be one of `fsm`, `network`, or `verbatim entity`. For
example:

```
fsm foo {
  <fsm description>
}
```

If the entity name is `foo`, the Alogic source file must be called
`foo.alogic`. Within the body, all entities can contain
[port](ports.md), [parameter](params.md#entity-parameters) and
[constant](params.md) declarations as well as any type definitions. The
remainder of the body depends on the type of entity and is discussed in
detail in [FSMs](fsms.md), [Networks](networks.md) and
[Verbatim Entities](interop.md#verbatim-entities). Parameter and port
declarations must precede any other entity contents except for constant
and type definitions, but can otherwise appear in an arbitrary order.

### Output Verilog modules

When the compiler is invoked to compile a design entity, it emits a unique, well
defined Verilog module that corresponds to the Verilog implementation of that
design entity. The Verilog module emitted has the same name as the name of the
design entity, and is output to a `.v` file with the same base name.

For example `spam.alogic`:

<a href="http://afiddle.argondesign.com/?example=entities_network.alogic">Fiddle with this code here.</a>

```
network spam {
  ...
}
```

Would compile into `spam.v`:
```verilog
module spam (
  input wire clk,
  input wire rst_n,
  ...
);
  ...
endmodule
```

If the compiler needs to emit multiple Verilog modules to synthesize a design
entity, then all other modules will be instantiated under this unique module
corresponding to the design entity itself, and will have a module name (and file
name) of the form `<entity name><sep><suffix>`, where _\<sep>_ is the separator
string provided by the `--sep` compiler option, which defaults to `__`. In the
above example, assuming the default separator string is used, additional modules
would be emitted as `spam__<suffix>` into correspondingly named `.v` files.

Note also that Alogic performs [parameter
specialization](params.md#entity-parameters), which will cause additional
suffixes to be added to the name of the output Verilog module corresponding to
the design entity.

<p align="center">
<a href="compilation.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="types.md">Next</a>
</p>
