# Worker-local JSON Schema dependency

`vendor/jsonschema` is the published MIT-licensed `jsonschema` 0.55.1 crate
(registry archive SHA-256
`b68339c3d874e48151d74ffe256d93a58cffa240983cb0967d3cbaea083a44fe`).
Its source, fixtures and license are retained. It is used only by this native
worker, not linked into the Haskell process.

The sole source patch is in `src/regex.rs`: a regex execution error terminates
the worker with exit status 2. Upstream boolean validation treats these errors
as a negative match, which can become an incorrect positive under `not` and
other applicators. This private copy must not be used as a general-purpose
in-process library. The Haskell owner translates failed worker execution into
`ValidatorProcessFailure` and reaps the child.

The worker disables the regex engine's fixed backtracking counter in favor of
the existing owned-process wall-clock deadline. Remaining engine resource
failures still terminate the worker; they are not instance-validation results.
Its panic hook exits without printing payloads, including when the dependency
would otherwise catch a panic and return a negative match. No new regex or
JSON Schema implementation, schema walker or fallback is introduced.

Regression fixtures cover a valid backreference/alternative expression that
exceeds the former counter, its negation, and cancellation of actual regex
execution. The locked dependency notices are in `THIRD_PARTY_LICENSES.txt` and
are embedded in `factory-droid-validator --licenses`.
