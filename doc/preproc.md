<p align="center">
<a href="interop.md">Previous</a> |
<a href="index.md">Index</a> |
Next
</p>

# Preprocessor

Alogic implements a preprocessor which uses the syntax of the C preprocessor,
but has a significantly smaller feature set compared to the C preprocessor. The
main use of the preprocessor is to allow sharing type definitions through
include files.

This section describes the available preprocessor directives

### Include files

The `#include` directive can be used ot include the contents of a source file at
the point where the directive is used. The relative path of the included file
must be specified in `"` quotes:

```
#include "interfaces.h"

fsm a {
  in  sync a_t a;
  out sync b_t b;

  ...
}
```

### Text replacement macros

The `#define` directive can be used to define a macro that will undergo text
replacement by the preprocessor. Only simple macros are supported, not
function-like macros:

```
#define MAGIC 4'd4;

fsm magical {
    u4 a = MAGIC;
}
```

The use of `#define` macros is fairly limited, and their main purpose is to
share named constants between multiple entities. Within a single entity, the us
of `const` declarations is more appropriate.

### Conditional compilation

The preprocessor supports the `#if`, `#ifdef`, `#else` and `#endif` directives
to support a limited amount of conditional compilation mainly aimed at assisting
with debugging by making debug statements conditional.

The use of `#if` and `#ifdef` is different in the Alogic preprocessor compared
to the C preprocessor in that both `#if` and `#ifdef` must be followed by the
name of a macro (i.e.: this is not valid `#if 0`).

`#ifdef` is true if the macro has been previously defined using `#define`, or if
it has been pre-defined on the compiler command line.

The macro name following `#if` must be defined, and evaluate to a simple
integer. `#if` is true if the subject macro evaluates to a non-zero value.

<p align="center">
<a href="interop.md">Previous</a> |
<a href="index.md">Index</a> |
Next
</p>
