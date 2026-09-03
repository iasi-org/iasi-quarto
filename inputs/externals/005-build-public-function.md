# Public build function

Source: working conversations with Javier G. Grandez.

`iasi.quarto::build()` is the public build entry point.

Its responsibility is deliberately small:

1. Identify what kind of project/artifact must be built.
2. Call the corresponding private builder.
3. Move the returned build results to their final IASI outputs location.

The public function orchestrates; it does not contain the implementation of the individual builders.
