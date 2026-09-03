# External inputs are immutable

Source: IASI working principles established during project development.

Content placed in `inputs/externals/` is treated as received source material.

IASI and its internal normalisation processes must not correct, rewrite, normalise or complete these files in place. Interpretation and canonicalisation produce new artifacts outside `inputs/externals/`, for example under `definitions/`.
