
# Translation of Alogic ports to Verilog ports

TBD

# Verilog interfaces

The Verilog interface of an Alogic module:
```
fsm foo() {
	<some port> p0;
	<some port> p1;
	...
	<some port> pn;

	...
}
```
would be:
```verilog
module foo (
	input wire clk,
	input wire rst_n,

	<signals for port p0>
	<signals for port p1>
	...
	<signals for port pn>
);
  ...
endmodule
```

# Verbatim verilog

TBD

# Verilog entities

TBD
