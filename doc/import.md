<p align="center">
<a href="using.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="gen.md">Next</a>
</p>

# The import mechanism

Alogic code in one file gains access to Alogic code in another file via the
import mechanism.

### Importing files with the `import` directive

The fundamental way to gain access to definitions written in another file is to
use an `import` directive. The syntax of the import directive is `import
"path/to/file" as <identifier>;`. The string defining the location of the file
to import must be a Unix style path, using `/` to separate directories. The path
by default is considered absolute and is resolved against the list of import
search paths passed to the compiler. Names defined inside the referenced file
can then be accessed via hierarchical references through the
given `<identifier>`.

Assume the following directory layout:

```
|- sub
|   └── constants.alogic
├── decoder.alogic
└── top.alogic
```

The content of 'constants.alogic' is:

```
const u32  BASE_ADDRESS = 0x4000_0000;
const uint PAGE_SIZE    = 4096;
```

The content of 'decoder.alogic' is:

```
fsm decoder {
  param u32  BASE;
  param uint SIZE;
  in  u32 addr;
  out bool enable;

  void main() {
    enable = BASE <= addr && addr < BASE + SIZE;
    fence;
  }
}
```

We can then pull these definitions together in 'top.alogic':

```
import "sub/constants.alogic" as constants;
import "decoder.alogic" as decoder;

network top {
  in u32 addr;
  out bool enable;

  d = new decoder.decoder(
    BASE=constants.BASE_ADDRESS,
    SIZE=constants.PAGE_SIZE
  );

  addr -> d.addr;
  d.enable -> enable;
}
```

The ".alogic" suffix is optional and is assumed if the suffix is omitted, so the
above import directives could have been written as:

```
import "sub/constants" as constants;
import "decoder" as decoder;
```

An import directive can appear anywhere where an arbitrary definition is valid.

### Relative imports

If the path given in an `import` directive starts with a `./` or `../`
directory component, then the path is considered relative, and instead of
resolving it against the compiler import search paths, it is resolved against
the directory containing the file where the `import` directive appears. For
example, if `foo.alogic` contains:

```
import "./bar.alogic" as bar;
import "../baz/quuz.alogic as quux;
```

Then the compiler will expect the following input files:

```
|- sub
|   ├── foo.alogic
|   └── bar.alogic
└── baz
    └── quux.alogic
```

Relative import resolution ignores the `#line` directive.

### Importing names with the `from` directive

It is possible to import directly some or all names defined in an Alogic source
file using the `from` directive. The syntax of the simple `from` directive is
`from "path/to/file" import <name> [as <identifer>];`. This imports the name
`<name>` defined in the specified file. The optional `as` clause can be used to
give an alternative local name to the imported definition. In fact the
`from` directive is just syntactic sugar for `import` followed by `using`:

```
from "foo.alogic" import bar as baz;

// The above is equivalent to the following, except 'tmpname' is not visible:
import "foo.alogic" as tmpname;
using tmpname.bar az baz;
```

The `from` directive also has a wildcard variant, which imports all names
defined in the referenced file:

```
from "bar.alogic" import *;
```

This is also just syntactic sugar for:

```
import "bar.alogic" as tmpname;
using tmpname.*;
```

<p align="center">
<a href="using.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="gen.md">Next</a>
</p>
