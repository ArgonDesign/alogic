# Alogic

Alogic is a Medium Level Synthesis language for digital logic that compiles
swiftly into standard Verilog-2005 for implementation in ASIC or FPGA.

## Motivation

Traditionally register transfer level (RTL) digital designs are specified using
the Verilog or VHDL Hardware Description Languages (HDLs). These standard HDLs
allow for almost unlimited flexibility in what digital circuits can be
expressed, and are used throughout the implementation flow as representations of
these digital circuits at various levels of abstraction (e.g.: behavioural
description, gate level netlist, etc.). While this flexibility is necessary for
a language that is used to represent circuits at multiple abstraction levels,
it also forces the designer to be explicit about every detail of their design.
Various implementations of the standard HDL languages also suffer from feature
set mismatches, where one implementation might support certain language features
only with restrictions and these restrictions vary between different
implementations. The requirement to be explicit about every detail, combined
with mutual incompatibilities among various tools results in a design experience
where the designer needs to follow repetitive design patterns and various 'best
practice' rules and code style, in order to produce high quality code.

High Level Synthesis (HLS) tools are trying to approach the digital design
problem from the opposite extreme. A high level (usually a C language)
description of the design is given, and the synthesis tool derives the necessary
digital architecture to implement the design, usually outputting the result in a
standard low level HDL. In our experience, a problem with these tools is that to
achieve high quality output that meets performance requirements, the designer
needs to specify large portions of the digital architecture manually, resulting
in a need to understand the particular synthesis tool behaviour intimately. The
use of the C language together with it's sequential semantics as an input
language does not help when the designer needs to specify architectural detail
for the desired digital circuit with a highly parallel structure. A further
issue with HLS tools is their excessive run-time, which makes design iteration
and debugging costly.

We believe there is a gap between these two approaches to digital design
specification. Alogic is trying to fill this gap by being a Medium Level
language to express common design structures concisely, but explicitly. After
reading the documentation, the designer should be able to tell exactly what kind
of hardware structure their Alogic design is going to compile into. The Alogic
compiler will then generate a Verilog description of the design, which conforms
to the common RTL best practice guidelines. Compilation of Alogic to Verilog
takes a similar amount of time as a similar complexity C program would take to
compile into an executable.

## Further information

* [Installation](https://github.com/ArgonDesign/alogic/blob/master/doc/install.md)
* [Documentation](https://github.com/ArgonDesign/alogic/blob/master/doc/index.md)
* [License](https://github.com/ArgonDesign/alogic/blob/master/LICENSE)
