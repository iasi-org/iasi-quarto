# Quarto builder

Source: working conversations with Javier G. Grandez.

The existing Quarto build implementation becomes a private function in `engine-build`, conceptually `.build_quarto()`.

It should preserve the existing Quarto build behaviour as far as possible.

Quarto must build into the output location defined by Quarto itself through its YAML configuration. IASI must not impose a local default output directory on Quarto.
