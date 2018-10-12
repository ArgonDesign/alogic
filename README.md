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
where the designer needs to follow mechanical design patterns and various 'best
practice' rules and code style, in order to produce high quality code.

High Level Synthesis (HLS) tools are trying to approach the digital design
problem from the opposite extreme. A high level (usually C language based)
description of the design is given, and the synthesis tool derives the necessary
digital architecture to implement the design, usually outputting the result in a
standard low level HDL. In our experience, a problem with these tools is that to
achieve high quality output that meets performance and area requirements, the
designer needs to specify large portions of the digital architecture manually,
resulting in a need to understand the particular synthesis tool behaviour
intimately. The use of the C language together with it's sequential semantics as
an input language does not help when the designer needs to specify architectural
detail for the desired digital circuit with a highly parallel structure. A
further issue with HLS tools is their excessive run-time, which makes design
iteration and debugging costly.

We believe there is a gap between these two approaches to digital design
specification. Alogic retains the ability to specify the architecture of digital
designs explicitly, but provides specialized syntax for the description of
commonly used hardware structures, and hence raises the level of abstraction to
a medim level. The goal of Alogic is not to try to derive the necessary
architecture from a high-level description (this is left to the expertise of the
designer), but to allow the explicit and concise expression of the most commonly
used digital structures. After reading the documentation, the designer should be
able to tell exactly what kind of hardware structure their Alogic design is
going to compile into. The Alogic compiler will then generate a Verilog
description of the design, which conforms to the common RTL best practice
guidelines. Compilation of Alogic to Verilog takes a similar amount of time as a
similar complexity C program would take to compile into an executable.

Having implemented major ASIC designs in Alogic, including modern video codecs,
we have experienced a code density improvement of over 5x, while reducing design
effort by a substantial factor compared to a pure Verilog design. The concise
syntax and well defined compilation of Alogic into Verilog also allowed us to
push the architectural complexity of the design further than we would otherwise
have been able to given the design and verification schedules. This in turn
resulted in a significant Silicon area reduction compared to similar solutions
designed using direct Verilog description.

At the start of the Alogic project, the level of abstraction and overall structure
were inspired by Cx. Both languages are based around entities which communicate
with each other through ports, and compile into standalone Verilog modules. Code
that is written sequentially is compiled into discrete control blocks. All of
these structural decisions, as well as a significant amount of syntax, were
motivated by Cx and form the foundations of the Alogic language.

There are some features of Cx which are not currently supported by Alogic – namely,
a development environment, the ability to compile into VHDL, clock and reset
properties, and in-lined functions.

As the Alogic project has developed, the following features have been introduced.
* Support for structures
* More explicit control statements to give the user full control over clock
cycles and states
* Output storage slices on ports to allow more complex designs
* Simplified network syntax for connecting ports together
* Automated connection of pipeline modules

This is an ongoing project and we expect to introduce further features over time.

## Further information

* [Documentation](doc/index.md)
* [License](LICENSE)
* [Installation](doc/install.md)
