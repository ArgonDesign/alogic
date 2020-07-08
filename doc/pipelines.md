<p align="center">
<a href="networks.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="memories.md">Next</a>
</p>

# Pipelines

### Networks for describing pipelines

Networks combined with nested FSMs are also used as the abstraction to describe
pipelines. This is achieved using `pipeline` variable and port declarations.

#### Pipeline variable and pipeline port declarations

Networks can contain variable declarations qualified with the `pipeline`
keyword:

```
pipeline u16 a;
```

`pipeline` variables used in nested FSMs will be passed along the referencing
FSMs through pipeline ports. Pipeline ports can be declared by using the
`pipeline` keyword instead of a type specifier in the port declaration. It
is conventional to declare pipeline ports as the cardinal ports of a pipeline
stage:

```
in sync ready pipeline;
out sync ready pipeline;
```

There can be at most one pipeline input and one pipeline output per entity.

#### Nested FSMs as pipeline stages

The following points explain how to create a pipeline in Alogic. The example
afterwards will also help to clarify.

- To create a pipeline entity, the designer will need to create a `network`
entity with nested FSMs which will act as the pipeline stages. The nested FSMs
must be singletons (have only a single instance), and must be defined in
pipeline order in the enclosing network.

- In the enclosing network, any variables that need to be passed between stages
should be declared as `pipeline` variables. These can then be used inside the
nested FSMs.

- The pipeline ports of the stages must be connected in the desired order.
Using cardinal pipeline ports makes this convenient:
`<stage-n> -> <stage-n+1>`.

- The `.read()` method of an input pipeline port can be used to consume the
incoming pipeline variables from the previous stage.

- The `.write()` method of an output pipeline port can be used to emit the
outgoing pipeline variables to the next stage.

<a href="http://afiddle.argondesign.com/?example=pipelines_commands.alogic">Fiddle with these commands here.</a>

Here is an example of a 3 stage pipeline that computes the dot product of two
4-element vectors, and has a 1-dot-product per cycle throughput. The example
deliberately handles each vector element as a separate scalar variable to
demonstrate how pipeline variables are handled. The pipeline uses 4 multipliers
in parallel in the 1st stage, and performs a radix-2 sum of the element-wise
products over the subsequent 2 stages, using pipeline variables to pass
intermediate results:

<a href="http://afiddle.argondesign.com/?example=pipelines_example1.alogic">Fiddle with this example here.</a>

```
network dotprod {
  // 1st input vector
  in sync ready u16 a0;
  in sync ready u16 a1;
  in sync ready u16 a2;
  in sync ready u16 a3;

  // 2nd input vector
  in sync ready u16 b0;
  in sync ready u16 b1;
  in sync ready u16 b2;
  in sync ready u16 b3;

  // Output dot product
  out sync ready u34 p_prod;

  // Element-wise products
  pipeline u32 mul0;
  pipeline u32 mul1;
  pipeline u32 mul2;
  pipeline u32 mul3;

  // The reduced sums
  pipeline u33 sum10;
  pipeline u33 sum32;

  new fsm stage0 {
    out sync ready pipeline;
    void main() {
      // Read operands, zero-extend them to 32 bits and perform multiplications
      mul0 = @zx(32, a0.read()) * @zx(32, b0.read());
      mul1 = @zx(32, a1.read()) * @zx(32, b1.read());
      mul2 = @zx(32, a2.read()) * @zx(32, b2.read());
      mul3 = @zx(32, a3.read()) * @zx(32, b3.read());

      // Write pipeline variables to the pipeline output port
      out.write();

      fence;
    }
  }

  stage0 -> stage1; // Connect cardinal pipeline ports

  new fsm stage1 {
    in sync ready pipeline;
    out sync ready pipeline;
    void main() {
      // Read pipeline variables from the pipeline input port
      in.read();

      // Zero-extend to 33 bits and perform 1st level of the reduction
      sum10 = @zx(33, mul0) + @zx(33, mul1);
      sum32 = @zx(33, mul2) + @zx(33, mul3);

      // Write pipeline variables to the pipeline output port
      out.write();

      fence;
    }
  }

  stage1 -> stage2; // Connect cardinal pipeline ports

  new fsm stage2 {
    in sync ready pipeline;
    void main() {
      // Read pipeline variables from the pipeline input port
      in.read();

      // Zero-extend to 34 bits and perform the final sum
      u34 prod = @zx(34, sum32) + @zx(34, sum10);

      // Write final result to output port
      p_prod.write(prod);

      fence;
    }
  }
}
```

To implement a pipeline, the compiler will automatically determine which
pipeline variable needs to flow from stage to stage, and will replace the
pipeline ports with regular ports holding a packed structure with all the
transiting variables. The definition of the `dotprod` pipeline above is
equivalent to:

<a href="http://afiddle.argondesign.com/?example=pipelines_example2.alogic">Fiddle with this example here.</a>

```
network dotprod {
  // 1st input vector
  in sync ready u16 a0;
  in sync ready u16 a1;
  in sync ready u16 a2;
  in sync ready u16 a3;

  // 2nd input vector
  in sync ready u16 b0;
  in sync ready u16 b1;
  in sync ready u16 b2;
  in sync ready u16 b3;

  // Output dot product
  out sync ready u34 p_prod;

  // The pipeline variables are pushed into the referencing stages

  new fsm stage0 {
    // The compiler derives this structure as the pipeline output port type
    struct pipeline_out_t {
        u32 mul0;
        u32 mul1;
        u32 mul2;
        u32 mul3;
    };

    // The pipeline port is converted to an ordinery port
    out sync ready pipeline_out_t;

    // The declarations of the referenced pipeline variables are
    // pushed into the stage as local varaible declarations
    u32 mul0;
    u32 mul1;
    u32 mul2;
    u32 mul3;

    void main() {
      // Read operands, zero-extend them to 32 bits and perform multiplications
      mul0 = @zx(32, a0.read()) * @zx(32, b0.read());
      mul1 = @zx(32, a1.read()) * @zx(32, b1.read());
      mul2 = @zx(32, a2.read()) * @zx(32, b2.read());
      mul3 = @zx(32, a3.read()) * @zx(32, b3.read());

      // The 'out.write()' call is replaced with a write call passing the
      // outgoing pipeline variables
      out.write({mul0, mul1, mul2, mul3});

      fence;
    }
  }

  stage0 -> stage1; // Connect cardinal ports (no longer pipeline ports)

  new fsm stage1 {
    // The compiler derives this structure as the pipeline input port type
    struct pipeline_in_t {
        u32 mul0;
        u32 mul1;
        u32 mul2;
        u32 mul3;
    };

    // The compiler derives this structure as the pipeline output port type
    struct pipeline_out_t {
        u33 sum10;
        u33 sum32;
    };

    // The pipeline port are converted to ordinery ports
    in sync ready pipeline_in_t;
    out sync ready pipeline_out_t;

    // Declarations of the referenced pipeline variables
    u32 mul0;
    u32 mul1;
    u32 mul2;
    u32 mul3;
    u33 sum10;
    u33 sum32;

    void main() {
      // The 'in.read();' statement is turned into a '.read()' call on the
      // pipeline input port, carrying the pipeline variables referenced in
      // this or any subsequent stage. The result of the `.read()' is assigned
      // to the now local pipeline variables
      {mul0, mul1, mul2, mul3} = in.read();

      // Zero-extend to 33 bits and perform 1st level of the reduction
      sum10 = @zx(33, mul0) + @zx(33, mul1);
      sum32 = @zx(33, mul2) + @zx(33, mul3);

      // Write pipeline variables referenced by later stages to the output port
      out.write({sum10, sum32});

      fence;
    }
  }

  stage1 -> stage2;  // Connect cardinal ports (no longer pipeline ports)

  new fsm stage2 {
    // The compiler derives this structure as the pipeline input port type
    struct pipeline_in_t {
        u33 sum10;
        u33 sum32;
    };

    // The pipeline port is converted to an ordinery port
    in sync ready pipeline_out_t;

    // Declarations of the referenced pipeline variables
    u33 sum10;
    u33 sum32;

    void main() {
      // The converted 'in.read();' statement
      {sum10, sum32} = in.read();

      // Zero-extend to 34 bits and perform the final sum
      u34 prod = {1'd0, sum32} + {1'd0, sum10};

      // Write final result to output port
      p_prod.write(prod);

      fence;
    }
  }
}
```

#### Complexity of stages

While it is common to build pipelines that sustain a single operation every
cycle, note that the pipeline port `.read();` and `.write();` statements simply
turn into common `.read()` and `.write()` calls on the converted input and
output ports, and as such can be used at any place in the pipeline stage where
a combinational statement is permitted.

Note further that the pipeline stages really are fully capable FSMs and can have
multiple states, perform function calls, or declare local storage and further
explicit ports, the same way as any other FSM, making more complex pipelines
possible.

<p align="center">
<a href="networks.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="memories.md">Next</a>
</p>
