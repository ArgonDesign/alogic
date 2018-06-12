<p align="center">
<a href="networks.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="memories.md">Next</a>
</p>

# Pipelines

### Networks for describing pipelines

Networks combined with nested FSMs are also used as the abstraction to describe
pipelines. This is achieved using `pipeline` variable declarations and the `read;` and
`write;` statements available in nested FSMs.

#### Pipeline variable declarations

Networks can contain variable declarations qualified with the `pipeline`
keyword:

```
pipeline u16 a;
```

`pipeline` variables used in nested FSMs will be passed along the referencing
FSMs through automatically inserted pipeline ports.

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

- All pipeline stages must be connected instance to instance using
`<stage-0-name> -> <stage-1-name>`. Ports should not be used as the compiler
will automatically connect the required pipeline ports.

- All pipeline stages (except for the first) can use the `read;` statement to
read in the pipeline variables from the previous pipeline stage. The `read;`
statement can appear anywhere where a control statement is valid.

- All pipeline stages (except for the last) can use the `write;` statement to
write the pipeline variables to the subsequent pipeline stage. The `write;`
statement can appear anywhere where a control statement is valid.

- The `read;` and `write;` statements operate on the pipeline variables that
have been automatically connected by the compiler

Here is an example of a 3 stage pipeline that computes the dot product of two
4-element vectors, and has a 1-dot-product per cycle throughput. The example
deliberately handles each vector element as a separate scalar variable to
demonstrate how pipeline variables are handled. The pipeline uses 4 multipliers
in parallel in the 1st stage, and performs a radix-2 sum of the element-wise
products over the subsequent 2 stages, using pipeline variables to pass
intermediate results:

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
    void main() {
      // Read operands, zero-extend them to 32 bits and perform multiplications
      mul0 = @zx(32, a0.read()) * @zx(32, b0.read());
      mul1 = @zx(32, a1.read()) * @zx(32, b1.read());
      mul2 = @zx(32, a2.read()) * @zx(32, b2.read());
      mul3 = @zx(32, a3.read()) * @zx(32, b3.read());

      // Write pipeline variables to next stage
      write;

      fence;
    }
  }

  stage0 -> stage1; // Connect pipeline ports

  new fsm stage1 {
    void main() {
      // Read pipeline variables from previous stage
      read;

      // Zero-extend to 33 bits and perform 1st level of the reduction
      sum10 = @zx(33, mul0) + @zx(33, mul1);
      sum32 = @zx(33, mul2) + @zx(33, mul3);

      // Write pipeline variables to next stage
      write;

      fence;
    }
  }

  stage1 -> stage2; // Connect pipeline ports

  new fsm stage2 {
    void main() {
      // Read pipeline variables from previous stage
      read;

      // Zero-extend to 34 bits and perform the final sum
      u34 prod = @zx(34, sum32) + @zx(34, sum10);

      // Write final result to output port
      p_prod.write(prod);

      fence;
    }
  }
}
```

To implement a pipeline, the compiler will automatically insert pipeline input
and output ports into the stages, and pass all required pipeline variables where
they are needed. Pipeline ports always use `sync ready` flow control and an
`fslice` as storage. The compiler turns the above definition of the `dotprod`
pipeline into the following, before compilation proceeds as for other Alogic
networks:

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

  // Notice how pipeline variable declarations
  // are pushed into the referencing stages

  new fsm stage0 {
    // The automatically inserted pipeline output port
    out sync ready pipeline_o_t pipeline_o;

    // Note that the first stage does not contain a pipeline input port

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

      // The 'write;' statement is turned into a '.write()' call on the
      // automatically inserted pipeline output port, carrying the pipeline
      // variables referenced by later stages
      pipeline_o.write({mul0, mul1, mul2, mul3});

      fence;
    }
  }

  // stage to stage connectsions are turned into pipeline
  // output port to pipeline input port connections
  stage0.pipeline_o -> stage1.pipeline_i;

  new fsm stage1 {
    // The automatically inserted pipeline input port
    in sync ready pipeline_i_t pipeline_i;

    // The automatically inserted pipeline output port
    out sync ready pipeline_o_t pipeline_o;

    // Declarations of the referenced pipeline variables
    u32 mul0;
    u32 mul1;
    u32 mul2;
    u32 mul3;
    u33 sum10;
    u33 sum32;

    void main() {
      // The 'read;' statement is turned into a '.read()' call on the
      // automatically inserted pipeline input port, carrying the pipeline
      // variables referenced be this or any subsequent stage. The result
      // of the `.read()' is assigned to the now local pipeline variables
      {mul0, mul1, mul2, mul3} = pipeline_i.read();

      // Zero-extend to 33 bits and perform 1st level of the reduction
      sum10 = @zx(33, mul0) + @zx(33, mul1);
      sum32 = @zx(33, mul2) + @zx(33, mul3);

      // Write pipeline variables referenced by later stages to the
      // pipeline output port
      pipeline_o.write({sum10, sum32});

      fence;
    }
  }

  stage1.pipeline_o -> stage2.pipeline_i;

  new fsm stage2 {
    // The automatically inserted pipeline input port
    in sync ready pipeline_i_t pipeline_i;

    // Note that the last stage does not contain a pipeline output port

    // Declarations of the referenced pipeline variables
    u33 sum10;
    u33 sum32;

    void main() {
      // Write referenced pipeline variables from previous stage
      {sum10, sum32} = pipeline_i.read();

      // Zero-extend to 34 bits and perform the final sum
      u34 prod = {1'd0, sum32} + {1'd0, sum10};;

      // Write final result to output port
      p_prod.write(prod);

      fence;
    }
  }
}
```

#### Complexity of stages

While it is common to build pipelines that sustain a single operation every
cycle, note that the `read;` and `write;` statements simply turn into common
`.read()` and `.write()` calls on the inserted pipeline ports, and as such can
be used at any place in the pipeline stage where a combinatorial statement is
permitted.

Note further that the pipeline stages really are fully capable FSMs and can have
multiple states, perform function calls, or declare local storage and further
explicit ports, the same way as any other FSM. The `read;` and `write;`
statements can be invoked in multiple states, making more complex pipelines
possible.

<p align="center">
<a href="networks.md">Previous</a> |
<a href="index.md">Index</a> |
<a href="memories.md">Next</a>
</p>
