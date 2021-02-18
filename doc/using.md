<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="import.md">Next</a>
</p>

# The `using` directive

Sometimes code can be clarified by introducing a simpler local alias for a
complex hierarchical reference. This can be achieved with the `using`
directive, which comes in two variants described below. A `using` directive can
appear anywhere where an arbitrary definition is valid.

### Simple `using` directive

The simple `using` directive introduces an alias for a single reference:

```
struct some_parametrized_struct {
  param uint P;
  static bool some_static_method(...) {
    ...
  }
}

using some_parametrized_struct(P=2).some_static_method as g;

bool x = g(...);
```

In the example above, the identifier 'g' is equivalent to the longer
hierarchical reference to the static method inside the parametrized structure.

If the subject of the directive is a hierarchical reference ending in a `.`
member selector, then the `as` clause is optional, and the local name is set to
the name of the selector:

```
using a.b;
// Same as:
using a.b as b;
```

### Wildcard `using` directive

It is possible to introduce an alias for all names accessible via a `.`
selector using the wildcard `using` directive:

```
struct some_parametrized_struct {
  param uint P;
  static bool a(...) { ... }
  static bool b(...) { ... }
  static bool c(...) { ... }
}

using some_parametrized_struct(P=2).*;

bool x = a(...) | b(...) | c(...);
```

The wildcard `using ref.*;` directive is equivalent to writing a simple
`using ref.<selector>;` directive for all valid `.` selector of 'ref'.

<p align="center">
<a href="builtins.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="import.md">Next</a>
</p>
