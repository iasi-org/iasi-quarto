# Move build results

Source: working conversations with Javier G. Grandez.

The common move phase performs three responsibilities:

1. Find the IASI `outputs` directory by walking upward from the supplied build `path` until an existing `outputs/` directory is found.
2. Determine the correct destination category from the type of artifact that was built and the individual result.
3. Move each returned build result to its destination.

Builders do not decide the final IASI destination.
