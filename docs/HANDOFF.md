# Droid SDK Haskell: paused delivery handoff

<!-- handoff-id: droid-sdk-haskell-2026-09-08; schema-version: 1 -->

Updated: 2026-09-08. Implementation checkpoint: `d6f86ce` on `main`.

## State and authority

**Feature development is paused at the user's request.** Goal `mtnsrj8c-uuc099` has automatic continuation disabled; five of twelve milestones are complete. `mcp-hosted` remains the selected, unfinished milestone. A working hosted-tool core is not full hosted parity. The earlier local-MVP goal does not establish completion of the combined SDK port.

The stopping request authorizes the current commit/push operation, not future publication, live Factory calls, OAuth approvals or model expenditure. No further live test is authorized. Resume substantive work only after explicit user instruction; where the existing goal is available, use `/goal-resume`.

The complete remaining-scope report is outside the repository, as requested: `~/dl/droid-sdk-haskell-remaining-scope-2026-09-08.md`. On this machine `~/dl` resolves to `~/Downloads`. This handoff, the issue tracker and selected evidence are committed so recovery does not depend on that report or `/tmp`.

**Push remains unresolved:** no Git remote was configured and no destination URL was supplied. Do not guess a repository owner, create a repository or select its visibility. Obtain the destination before pushing. Local commits and a local bundle are not an off-machine backup.

## What is implemented

| Milestone | Delivered scope | Status |
| --- | --- | --- |
| `haskell-foundation` | Cabal/GHC2024, Nix shell, CI configuration, licenses, immutable schemas, reference audit and codec foundation | Complete foundation; other-platform execution remains separate |
| `sessions-streams` | Local new/resumed sessions, one-shot/follow-up turns, complete/partial events, outcomes, interruption, model/context queries, fork/compact/rewind retirement and rollback, subscriptions and ordered settings observations | Complete local core |
| `inputs-controls` | Image/text/PDF attachments, raw/typed structured output, safe permission/question handling, settings, initialization-only prompts, tool/skill/command controls and hook events | Complete local core |
| `daemon-session` | Native plain/TLS WebSockets, explicit authentication, create/load, shared turns/events/inputs/output, pending interactions, interruption and owned cleanup | Complete offline vertical slice |
| `mcp-external` | Local/daemon startup configuration, reports/registry, add/remove/toggle/auth operations, early events, configuration replay and sessionless global daemon get/update | Complete offline scope |
| `mcp-hosted` | Argument-only raw/typed/structured Haskell tools, authenticated loopback HTTP, rich results, limits and manual/scoped ownership; native peers invoke handlers during init/load, replacement and rollback | Core implemented; validation parity remains open |

The library runs natively: Python is used only for development audits, and Node.js is not required. Existing CLI/daemon processes remain the agent engine. The main facade is `Factory.Droid`; private `Internal.Session` owns shared lifecycle behavior, `Internal.Stream` owns reduction, and `Protocol`/`Protocol.Dispatch` own correlation and server-request admission. Do not introduce a competing session or callback runtime.

## Immediate continuation

1. Read this file, `PLAN.org`, the functional matrix in `docs/parity.md`, and `docs/development.md`. Earlier codec-section counts and progress notes are historical. Proposed module destinations are not implemented APIs.
2. Review `hsdk-zpf` and `hsdk-4po` before changing hosted validation. The former covers required schema conformance; the latter covers conflicting dependency license declarations. Neither is waived by safe rejection of unsupported inputs.
3. Preserve the implemented server and tool ownership model. Extend or replace the validator only after checking the required baselined input/output contracts and an appropriate dependency's source, behavior and license.
4. Add an actual failure-before regression for each corrected behavior. Use focused checks while implementing; run broad checks at coherent milestones. Do not repeat stress, Haddock or source-distribution cycles after every wrapper.
5. If validation or licensing requires an external decision, record the precise blocker. Independent REST/resource work can proceed after resumption, but do not close `mcp-hosted` or the overall goal prematurely.
6. **At the end of every subtask, run the `fess` skill before marking it complete or committing.** Invoke `/fess`, or load `command-fess/SKILL.md` and execute its full rubric. The current installation is `/Users/johnw/.agents/skills/command-fess/SKILL.md`. Record findings, verification gaps, scope drift and the next fix; do not replace the audit with an assertion that tests pass. If the skill is unavailable, obtain it before closing the subtask. Delegated independence requires the skill's explicit no-history attestation and sentinel probe.

## Remaining roadmap

This is functional scope, not an exhaustive schema-generation queue.

1. **Hosted conformance and provenance:** required JSON Schema behavior, advertised versus enforced constraints, argument/output/error semantics, compiler bounds, and `jsonschema` license clarification. Keep HTTP authentication, bounded envelopes, argument-only handlers and cancellation ownership intact.
2. **REST and advanced daemon resources:** native compute/template/session REST helpers; session collections, search, archival, queues and low-level load coordination; workspace trust/files/defaults; terminals and restoration; uploads/downloads and workspace-targeted transfers; proxy tokens; custom models, SSH keys, relay/update controls; plugins, marketplaces and automations; Git, worktrees, semantic diffs and feedback; crons, missions and Software Factory resources; exported session/mission stores, managers and controllers. Verify every required operation, event/result variant and error, rather than relying on the generic RPC escape hatch.
3. **Advanced transports:** required ACP/process modes, relay/binary tunnels, host-supplied message channels and exposed low-level facilities. Reuse existing framing/correlation/lifecycle primitives; test authentication, cancellation, malformed frames, disconnect and ownership with native peers.
4. **Discovery and remaining configuration:** saved-session filesystem discovery (`hsdk-f20`); remaining initialization, managed-model/tool/skill/hook, queue/loop and session controls; daemon settings observation and fork/compact/rewind; legacy query/stream/conversion/feed capabilities and message-selection/reconstruction helpers. Daemon replacement returns successor identifiers without retiring the source, unlike local replacement.
5. **Attribution, observability and compatibility:** honest Haskell identity; trace/log/metric propagation and privacy; explicit reconciliation of SDK/CLI/schema conflicts; required runtime refinements, diagnostic text and timestamp semantics.
6. **Conformance and platform execution:** complete capability matrix, native/reference-derived failure coverage, actual macOS/Linux results on the declared compiler matrix, and freshly authorized bounded CLI/daemon interoperability tests. CI configuration is not execution evidence.
7. **Distribution sign-off:** documentation/migration/examples, Haddock links, source-repository metadata, licensing, independent source distribution and final scope/audit reconciliation. No required omission, stub or unapproved reduction may remain.

### Open tracker anchors

- `hsdk-mcp-y1i`: external MCP complete; hosted core in progress.
- `hsdk-zpf`: hosted schema validation restrictions.
- `hsdk-4po`: `jsonschema` MPL-2.0 package license versus `LicenseRef-AllRightsReserved` module headers.
- `hsdk-daemon-resources-observability-mxt`: remaining resource/transport/observability umbrella.
- `hsdk-f20`: saved-session discovery; investigation only.
- `hsdk-reference-baseline-h1v`, `hsdk-types-codecs-2rb`: functional inventory and required bindings, not exhaustive schemas.
- `hsdk-haskell-attribution-80m`, `hsdk-protocol-baselines-wvf`, `hsdk-runtime-refinements-tfo`: material contract decisions.
- `hsdk-diagnostic-text-limits-81n`, `hsdk-w8y`: diagnostic/timestamp conformance where required by operations.
- `hsdk-conformance-integration-m3p`, `hsdk-docs-package-signoff-ip3`, `hsdk-documentation-diagnostics-jvc`: final verification/documentation.
- `hsdk-org-export-whitespace-x4c`: Obr's trailing blank-line exporter defect.
- `hsdk-vtk`: deferred optional upstream WebSocket reporting, not missing local safeguards.

## Invariants and known limits

- Preserve asynchronous exception identity and masked ownership transitions. Uncertain mutations invalidate before releasing their lease. Atomic retirement, not return-value receipt, commits a local replacement.
- Intake prefix barriers are not sleeps, snapshot epochs or worker joins. Typed known events fail on malformed payloads; unknown events remain explicit raw data. Delivered text prefixes and authoritative final buffers are distinct.
- Permissions/questions default to cancel/reject. Only an explicit permission handler changes permission policy. Daemon permission routing admits owned/associated execution; questions admit only owned execution. This policy is not a sandbox.
- Daemon scopes own connections, not the external daemon or saved-session storage. Normal exit does not kill the daemon, log out or delete/close saved sessions.
- TLS verification is the default. Plaintext/private roots are explicit. Authentication is protocol-level; no inferred endpoint suffix or browser/OAuth exchange is provided.
- MCP acknowledgements are not authentication completion. Early MCP observers are connection-scoped, before init/load, and remain through replacement/rollback. Configuration preserves omission/empty/false and header-array versus map distinctions.
- Hosted servers bind IPv4 loopback with a random bearer. Defaults: 4 MiB request, 10 MiB response, thirty-second tool deadline. Manual starts pin ownership; leases borrow/share it; restart rotates the token. Cleanup joins handlers and therefore requires cooperative cancellation. Client disconnect does not undo side effects or promise cross-POST cancellation.
- Hosted schemas reject unsupported patterns, unevaluated/dynamic/external references and ambiguous `~01` escapes. `$ref` siblings are normalized conjunctively. Format/content fields are annotations. These restrictions remain a parity gap, not an approved subset.
- Attachment limits are 5 MiB for image/text and 3 MiB for PDF; signature checks are not full document parsers. Turn-output adaptation is not full JSON Schema validation. Redacted `Show` does not sanitize explicit fields or encoded JSON.
- Saved-session discovery hit macOS `getExtendedFileStatus: unsupported operation (Operation is not supported)`. No discovery module or birth-time binding was delivered. Do not silently substitute a different timestamp contract.

## Verification and preserved evidence

Selected logs, reviews, reference information and an archived goal snapshot reside in `docs/evidence/2026-09-08/`; its README distinguishes current checks from historical evidence. `source-sha256.txt` binds the implementation to the recorded checks. Original `/tmp` paths in older notes are optional investigation aids, not required recovery inputs.

Fresh stopping-point checks established:

- Local snapshot: warning-free build and 3,005 passing tests.
- Daemon/external-MCP snapshot: recorded fingerprints match, warning-free build and 3,092 passing tests.
- Hosted implementation: warning-free build and 3,110 passing tests; Ormolu, HLint and actionlint pass.
- Hosted README example matches its compiled source and compiles without execution. Five reference-audit tests pass.
- Haddock completes with link warnings; Cabal check retains `[no-repository]`. Initial offline Haddock lacked source archives; the download attempt exposed a missing matching `zlib.h`. Installing the corresponding Nix development output and setting the include path resolved that build failure.
- The unpacked hosted distribution builds and passes 3,110 tests. Its attachment probe exposed the Base64 package-name collision; commit `d6f86ce` switches the probe to the library's Cabal REPL. Root probes pass all six behavioral markers. See the evidence README for the final distribution recheck.

One earlier authorized local run streamed `HELLO`, matched the final result and reaped its child; it predates later changes. Two prompt attempts were consumed, the first failing before optional local-session routing was corrected. No current live daemon, two-turn, rich-event, hosted-MCP or OAuth interoperability claim is made.

Only Apple Silicon macOS/GHC 9.10.3 execution is established. GHC 9.12.4/9.14.1 and Linux CI entries are proposed execution targets, not a completed support matrix. The current validator's base bound excludes the proposed GHC 9.14 line. Resolve this explicitly; do not suppress the job or call it verified.

## Resume on this machine

```sh
cd /Users/johnw/work/positron/droid-sdk-haskell
git status --short
git log -7 --oneline
obr ready
obr show hsdk-mcp-y1i
obr show hsdk-zpf
obr show hsdk-4po
```

The preferred environment is `nix develop path:.`. If diagnosing the previously observed local loader/environment problem, the exact toolchain used for these checks was:

```sh
export PATH="/nix/store/y3giz0m11fsc55jl2xbk2jpzyinm48ar-ghc-9.10.3-with-packages/bin:/nix/store/ccjihgs38m5qgvksik031vjz1cn9qksy-cabal-install-3.16.1.0/bin:/nix/store/8d4a6sf2k7bdjjaw21h9h0ggvvnhv8cm-hlint-3.10/bin:$PATH"
export NIX_LDFLAGS_arm64_apple_darwin="-L/nix/store/008518i9kpvzsvs6d9rgcwgicjp5l4zb-zlib-1.3.2/lib"
export NIX_CFLAGS_COMPILE_arm64_apple_darwin="-I/nix/store/k2sq5ihmlgs9vv5j74jr3bqywvj2jsxr-zlib-1.3.2-dev/include"
cabal build all --offline --ghc-options=-Werror
cabal test all --offline --ghc-options=-Werror --test-show-details=direct \
  --test-option=--hide-successes --test-option=-j1 \
  --test-option=+RTS --test-option=-N2 --test-option=-RTS
python3 -B scripts/check_attachment_files.py
python3 -B -m unittest discover -s scripts -p 'test_*.py' -v
python3 -B scripts/reference_schemas.py
find src test examples -name '*.hs' -exec ormolu --mode check {} +
hlint src test examples +RTS -N1 -RTS
actionlint .github/workflows/ci.yml
```

These store paths are machine-specific, not an installation contract. Missing public dependency archives require an ordinary Cabal download rather than fabricated success from `--offline`. Run only one Cabal writer at a time. Do not run `droid-example`, README model examples or the archived live driver without new authorization.

After the baseline is understood, resume the goal explicitly. Maintain the existing task tree; do not reopen completed local cores as an excuse to postpone missing capabilities. The downstream `fess` gate applies to each subtask.

## Cold-machine recovery

1. Obtain the pushed repository, or transfer the local bundle `~/dl/droid-sdk-haskell-2026-09-08.bundle` to the new machine and clone it. Until transfer or push is confirmed, no off-machine backup exists.
2. Read this handoff and `docs/evidence/2026-09-08/goal-snapshot.md`. The latter is an archive, not a file to execute or an instruction to resume automatically. If Pi goal state is absent, have the user explicitly recreate the objective and restore the twelve-task plan from that snapshot with confirmation. Preserve the five completed scopes and seven remaining ones.
3. Rebuild Obr's local cache from tracked `PLAN.org`: `obr init && obr sync --import-only --rebuild`. Never copy or commit `.obr/` as the authoritative state.
4. Install Nix or the documented native GHC/Cabal prerequisites; enter the checked-in shell or reproduce the declared dependency matrix. Re-run the baseline on the new platform and retain the result. Do not copy ARM64 Nix paths onto Linux.
5. Recover reference sources as needed: Python `https://github.com/Factory-AI/droid-sdk-python` at `6d06b4e613aab3990cf2ced469b5c4e80053e52e`; TypeScript guides at `https://github.com/Factory-AI/droid-sdk-typescript`, commit `87b4c4c4e4fc093e10a94f1e767ed57b7cbf597e`. Published package URLs/integrities are in the evidence README and parity document. Verify revisions and hashes; do not follow current upstream heads.
6. The four supplied schema files and their fingerprints are already committed. The old Desktop/Downloads input directory is not required. Reacquire the selected CLI only for explicitly authorized live work; prior static offsets apply solely to its recorded binary hash.

A suitable fresh-session instruction is: “Read `docs/HANDOFF.md`, the archived goal and `PLAN.org`. Preserve the scope and safety constraints. After my explicit goal resume, continue with hosted validation/provenance, then the remaining functional roadmap. Run `fess` after every subtask. No live calls or publication without fresh authorization.”
