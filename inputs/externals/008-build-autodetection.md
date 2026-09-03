# Build autodetection

Source: working conversations with Javier G. Grandez.

`build()` already receives `format` and `path`.

When `format` is absent or is `all`, `build()` inspects `path` and determines what kind of project is present there.

If `format` is explicit, the explicit request takes precedence over autodetection.
