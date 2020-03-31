<p align="center">
<a href="gen.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="interop.md">Next</a>
</p>

# Foreign function interface

Alogic supports a mechanism which allows Alogic code to call functions
implemented in a different programming language.

### Importing foreign functions in Alogic

A foreign function can be imported in entity scope using the `import` keyword,
followed by the function signature, e.g.:

```
import bool foreign(u8 i, u16 j);
```

The above will import a function called `foreign`, which takes 2 arguments of
the specified types, and returns a `bool` value. Foreign function call
expressions can be used inside statements only:

```
fsm a {
  in u8    i_a;
  in u16   i_b;
  out bool o;

  import bool foreign(u8 i, u16 j);

  void main() {
    o = foreign(i_a, i_b); // Foreign function call in statement
    fence;
  }

  bool local = foreign(1, 2); // Error: Foreign function call outside statement
}
```

Foreign function return values and arguments must have a packed type. In
addition a foreign function can be imported with a `void` return type.
All foreign functions are combinational functions, which means calls evaluate
immediately. Like any combinational function, a foreign function can be called
multiple times in a cycle:

```
import void log(u8 i);

void main() {
  u8 a = ...;
  log(a); // First call to 'log' in cycle
  a = ...
  log(a); // Second call to 'log' in same cycle
  fence;
}
```

### Namespace of foreign functions

The namespace of all foreign function `import` declarations is global, which
means that multiple `import` declarations using the same function name will
refer to the same foreign function. It is valid to import the same foreign
function from multiple scopes, but all `import` declarations must use the
same signature. The compiler will issue an error if this is violated.

### Implementation of foreign functions in SystemVerilog

The implementation of foreign functions uses the SystemVerilog DPI. Each
foreign function `import` declarations is translated into a corresponding
DPI import declaration in the output module. All emitted DPI import
declarations are of functions with `void` return type. Any return value is
handled via an output argument that is added as the first formal argument to
the DPI import statement. Furthermore, all return values and arguments are
declared as packed `bit` arrays in the DPI import statement, even if they
have 1-bit wide types. All DPI import declarations are of the `"DPI-C"` and
`context` variant. This means the following Alogic `import` declarations:

```
import void f();
import void g(u8 a1);
import void h(u8 a1, bool a2);
import u32  i();
import u32  j(u8 a1);
import u32  k(u8 a1, bool a2);
```

Are translated to the following SystemVerilog DPI import declarations:

```
import "DPI-C" context function void f();
import "DPI-C" context function void g(input bit [7:0] a1);
import "DPI-C" context function void h(input bit [7:0] a1, input bit [0:0] a2);
import "DPI-C" context function void i(output bit [31:0] r);
import "DPI-C" context function void j(output bit [31:0] r, input bit [7:0] a1);
import "DPI-C" context function void k(output bit [31:0] r, input bit [7:0] a1, input bit [0:0] a2);
```

The SystemVerilog Standard (IEEE 1800) specifies the corresponding C language
function declarations. These are repeated here as reference. All packed `bit`
arrays are passed via pointers to arrays of type `svBitVecVal`. The
`svBitVecVal` type is defined via a `typedef` in the `svdpi.h` standardized
header to be equivalent to `uint32_t`. Input argument pointers are `const`
qualified. The C language signatures for the above functions would be:

```
void f();
void g(const svBitVecVal *a1);
void h(const svBitVecVal *a1, const svBitVecVal *a2);
void i(svBitVecVal *r);
void j(svBitVecVal *r, const svBitVecVal *a1);
void k(svBitVecVal *r, const svBitVecVal *a1, const svBitVecVal *a2);
```

Remember that when compiling the above declarations as C++, the `extern "C"`
qualifier is required to retain C linkage semantics (i.e.: to avoid C++ name
mangling).

If an argument contains more than 32-bits, the passed arrays are packed in a
little-endian fashion, i.e. `u48 x` would be declared in the DPI import
declaration as `bit [47:0] x`, which in turn would be passed as
`svBitVecVal *x`, with bits 31 to 0 stored at `x[0]` and bits 47 to 32 stored
in the lower half of `x[1]`.

<p align="center">
<a href="gen.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="interop.md">Next</a>
</p>
