# Installation

Only Linux platforms are supported at the moment.

Currently only building from source is supported. A bootstrap script is
available to make this a simple exercise.

Before you begin, install the required dependencies:
* Java 8+
* Git
* SBT 1.0+ (https://www.scala-sbt.org/1.x/docs/Setup.html)

To get the bootstrap script:
```
curl https://raw.githubusercontent.com/ArgonDesign/alogic/3.0.x/bootstrap/alogic > alogic && chmod +x alogic
```

To install the latest version, which at the moment might be a remote branch:

```
./alogic update
```

This will by default install the compiler in your home directory under
`.alogic`. You can edit the `INSTALL_DIR` variable in the script to put
it somewhere else.

To install a specific version, just provide a git refspec to the previous
command. For example, to update to the `v3.0.0-M2` release tag:

```
./alogic update v3.0.0-M2
```

The bootstrap script can then be used as the compiler executable. To verify
the installation, try:

```
./alogic --help
```
