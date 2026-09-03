# Builder result contract

Source: working conversations with Javier G. Grandez.

Private builders return a list of build outputs.

Each returned output must provide enough information for the public `build()` function to identify what was produced and where its natural build output currently exists.

A Quarto build may return several outputs. An R source-only or binary-only build normally returns one output; `format = "r"` normally returns two.
