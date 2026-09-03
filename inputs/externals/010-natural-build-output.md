# Natural build output

Source: working conversations with Javier G. Grandez.

Each builder produces into its natural/tool-defined output location.

For Quarto, this is whatever its YAML configuration declares.

Build metadata and fingerprints belong to that natural output. The later move step moves the complete result, including its metadata, to the final IASI output location.
