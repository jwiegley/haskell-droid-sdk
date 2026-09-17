# SDK parity and reference baseline

This document records existing implementation and the functional SDK parity backlog. The active goal covers the combined Python/TypeScript SDK capabilities, including daemon/REST functionality. Exhaustive schema-definition and codec coverage are not independent completion requirements; types and validation needed by required operations remain in scope.

Codec-section counts and delivery narratives are historical checkpoints. The functional matrix governs documented coverage; operational recovery state is kept in the repository-only `docs/HANDOFF.md`. A proposed module destination does not establish an implemented capability.

The 1.205.0 schemas and baselined SDK implementations remain contract references. Their inventories do not require implementing unused definitions merely to reach a coverage total. Existing codecs and tests are retained.

The [deferred exhaustive codec report](exhaustive-codec-backlog.md) records all missing named representations, runtime constraints, partial-contract boundaries and a restart plan. It is an opt-in handoff, not an active functional completion gate.

## Remaining capability tracking — 2026-09-09

The completed closure audit (`hsdk-to0`) remains historical evidence. The user-confirmed issue hierarchy now tracks the remaining work; the audit must not be repeated. Full functional scope is unchanged, and the original twelve capability milestones retain their histories. The dashboard's additional tracking/control rows are not new SDK capabilities. Unused schema/type exports, Windows and browser rendering remain outside the agreed scope; native state/selection/host-channel behavior remains inside it.

### Evidence and present state

The audit began at **17:32 UTC**. It used the existing Python and TypeScript public inventories, current capability matrix, selected configuration/transport declarations, issue evidence and one current-tree verification pass. It did not rebuild the schema inventory or modify SDK/test code. The inventory receipts are `python-public-inventory.md` and `typescript-public-inventory.md` under `/Users/johnw/.pi/agent-deck/10a07ddd-1788549079/subagent-artifacts/outputs/9325d133-0368-40b4-97f6-42c65f90cd4f/`; their declaration locations remain reference navigation, not runtime certification.

- **Retained implementation/evidence:** local and daemon session/stream/input/control paths, external MCP, REST and most daemon resource bindings; mission events/models/getters. The completed milestone claims retain their scoped historical evidence. They do not certify the current tree or final platform/live parity.
- **Historical audit failure:** the all-component build failed on an unused import at `test/MissionRegistrySpec.hs:15`; its tests did not run. Two formatting differences and four HLint lambda hints were also recorded. These defects were reproduced and fixed after the explicit 21:36 UTC SDK resumption.
- **Current mission-association verification:** `hsdk-j47` has a warning-free all-component build, touched-file Ormolu/HLint, all 3,315 tests passing (8.51s), five audit regressions/reference integrity and the expanded compiled mission example. Its three children cover build restoration, provisional merge/usage and subscription/replacement lifetimes. Receipts: `/tmp/droid-mission-resume.aqQ5Iz/parent-*.log`; [scope and self-review](development.md#mission-association-and-provisional-observation). This is not independent review or final SDK certification.
- **Hosted MCP verified offline:** the native worker closes precise validation, cancellation and redistribution requirements. Raw/typed/structured tools execute through local/daemon startup/load/replacement, with authentication, errors, shared/manual ownership and worker/handler teardown checked. The 3,919-test integration boundary is not platform/live/final-distribution certification; see [hosted acceptance](development.md#hosted-session-integration-acceptance).
- **Certification scope:** GHC 9.12.4/base 4.21 is verified on the observed macOS/GNU/Linux ARM64 targets. After the [test-observer repair](development.md#september-14-saved-session-observer-correction), all 3,942 tests pass on each platform; production scanner behavior and its assertions are unchanged. Local live verification passed multi-turn identity, interruption and cleanup in 34.89 seconds; that follow-up's patch version was not recorded. The native daemon passed all seven live markers against pinned CLI 0.217.0 in 32.32 seconds: authenticated HELLO/recall stream-result-session equality, cancellation, closed-handle rejection, borrowed-connection reuse and reconnect, with observed owned-process cleanup. No false SDK attribution or authentication bypass was needed. Earlier failures and the separately scoped distribution evidence remain recorded. See [new-key live verification](development.md#september-14-new-key-live-verification).

Historical audit receipt: `/var/folders/1q/mmxrlqw15hvd4gvfc86kjwhh0000gn/T/droid-hosted-resume.97zto82w/closure-audit-checks.log` (`BUILD_EXIT=1`, tests skipped, `FORMAT_EXIT=100`, `HLINT_EXIT=1`). HEAD remains `6172dc8`; later work is unstaged/uncommitted and is not protected by the old stopping-point bundle.

### Frozen capability-gap ledger

These are capability families, not time estimates. Preserve existing implementation and evidence. Configuration/helper mappings must identify actual missing behavior or cite native equivalents; broad reference groupings do not prove every listed option/helper is absent.

| ID / milestone | Remaining outcome and classification | Acceptance boundary |
| --- | --- | --- |
| F0 — daemon resources | Mission association `hsdk-j47` — verified offline | Build/lint, provisional observations, parent/worker merging, usage/alias isolation, cancellation/rollback, subscriptions and closure pass at the fresh full-suite boundary. |
| F1 — daemon resources | Coordinated session/state/controller workflows — verified offline | All nine session-state children are implemented: independent attachments, load/readiness, optimistic submission, conversation/todo/progress/hook/selection, queues, child hydration, terminals, cwd and pending/deferred interactions. Combined ownership/subscription/load/response checks pass in the 3,545-test suite. |
| F2 — daemon resources | Cron family, explicit logout and Software Factory operations — verified offline | Six cron methods/events, logout, and all twelve Software Factory operations pass native wire/projection/variant/error/cancellation/ownership checks. Logout controller clearing is verified under F1; advanced transport orchestration remains F3. No live service execution is certified. |
| F3 — advanced transports | Native process/host/relay orchestration — verified offline | ACP/launch configuration, owned and supplied IPC, in-process channels, injected sessions and all five relay/connection children use the existing owners. Native owned-IPC/error/cancellation/descriptor checks now execute on both macOS and GNU/Linux ARM64; broader release matrix and live gates remain separate. |
| F4 — discovery/configuration | Saved-session discovery and create/resume configuration — verified offline | Scanning/selection and native macOS/Linux birth-time paths pass. Linux C statx equality, modification-time independence, EBADF and Haskell/independent-unix comparison close the prior platform gap. All four configuration children remain verified; launch mechanics belong to F3. |
| F5 — discovery/configuration | Standalone streams and functional helpers — verified offline | All seven children cover shared stream/feed/idle completion, message-chain helpers, raw/typed subagent inspection, queue review and utilities without duplicate runtime ownership. |
| F6 — attribution/observability | Approved identity/compatibility policy and observability — verified offline | Native sessions omit unsupported optional SDK attribution without impersonation. Terminal dates retain strict wire strings and the documented leap-position boundary. The required non-temporal wire decisions and all five observability children are verified; live certification remains separate. |
| F7 — hosted MCP | Native validation, licensing and hosted integration — verified offline | All tool styles run through authenticated local/daemon paths with precise values, errors, concurrency, deadlines and cleanup. Native worker/source/license deployment is verified on macOS; platform/live/release gates remain separate. |
| F8 — conformance/integration | Actual supported matrix and authorized live interoperability — gated | Execute required macOS/Linux/compiler checks and explicitly authorized bounded CLI/daemon smoke scenarios. Do not substitute CI configuration or old receipts for execution. |
| F9 — package sign-off | Complete distributable release — pending | Close required capability/issue gates; finish docs/examples, final build/test/lint/format/Haddock/package checks, independent unpacked distribution and evidence/provenance reconciliation. No unauthorized publication. |

F1/F3/F5 are functional groupings of the already-inventoried public stores/controllers/helpers, not a new request to clone every TypeScript class or exported type. Their frozen source anchors are the TypeScript inventory sections 1, 3, 6 and 7; F4 also uses Python `_high_level/config.py:158–269`. Native message selection, state subscriptions and terminal restoration cannot be discarded as browser UI. Conversely, incidental object models and schema-only exports do not become separate deliverables.

### Execution discipline

**The 50–95-hour estimate and derived calendar window are withdrawn.** They were uncalibrated planning judgments, not measured test runtime or a valid approval gate. Test command and execution durations are recorded separately in each delivery receipt; do not infer delivery time from test, codec or issue counts.

SDK implementation and offline verification resumed explicitly on 2026-09-09 at 21:36 UTC. The tracker-only amendment is complete and is not a new pause boundary. `hsdk-j47` has now passed its required behavioral checks; take ready issues through their children and acceptance evidence while correctness-independent work continues alongside licensing/platform/live gates. No repeated closure audit, unsupported forecast gate or new schema inventory.

### Goal-to-Obr crosswalk

The approved hierarchy is materialized: **86 new issues and 16 reused issues**, with bindings, parent links and prerequisite edges verified. These 102 linked issues cover **39 dashboard rows and 63 additional detailed children**; the other six dashboard rows preserve completed capability/audit history. These are tracking units and roll-ups, not 102 newly asserted missing capabilities.

The 45-row dashboard is flat because of its task/nesting limits. Full hierarchy, acceptance criteria and stable `pi-goal:mtnsrj8c-uuc099/<key>` bindings live in `PLAN.org`/Obr. Use `obr show <id>` for scope and `obr dep list <id> --direction up --type parent-child` for direct children. Required descendants and integration evidence must pass before a dashboard parent closes.

| Goal key | Obr issue |
| --- | --- |
| issue-tracking | hsdk-reference-baseline-h1v |
| docs-package-signoff | hsdk-docs-package-signoff-ip3 |
| mcp-hosted | hsdk-mcp-y1i |
| hosted-validator | hsdk-zpf |
| validator-license | hsdk-4po |
| daemon-resources | hsdk-daemon-resources-observability-mxt |
| daemon-crons | hsdk-daemon-resources-observability-mxt.1 |
| software-factory | hsdk-daemon-resources-observability-mxt.2 |
| daemon-logout | hsdk-daemon-resources-observability-mxt.3 |
| session-state | hsdk-daemon-resources-observability-mxt.4 |
| mission-association | hsdk-j47 |
| advanced-transports | hsdk-docs-package-signoff-ip3.1 |
| native-host-transports | hsdk-docs-package-signoff-ip3.1.1 |
| relay-orchestration | hsdk-docs-package-signoff-ip3.1.2 |
| discovery-configuration | hsdk-docs-package-signoff-ip3.2 |
| saved-session-discovery | hsdk-f20 |
| session-configuration | hsdk-docs-package-signoff-ip3.2.1 |
| configuration-refinements | hsdk-runtime-refinements-tfo |
| stream-functional-helpers | hsdk-docs-package-signoff-ip3.2.2 |
| attribution-observability | hsdk-docs-package-signoff-ip3.3 |
| haskell-attribution | hsdk-haskell-attribution-80m |
| observability-hooks | hsdk-docs-package-signoff-ip3.3.1 |
| protocol-baseline-decisions | hsdk-protocol-baselines-wvf |
| terminal-timestamp-contract | hsdk-9j2 |
| required-leap-second-contract | hsdk-w8y |
| conformance-integration | hsdk-conformance-integration-m3p |
| compiler-dependency-compatibility | hsdk-conformance-integration-m3p.1 |
| macos-matrix | hsdk-conformance-integration-m3p.2 |
| linux-matrix | hsdk-conformance-integration-m3p.3 |
| offline-conformance | hsdk-conformance-integration-m3p.4 |
| live-verification-authorization | hsdk-conformance-integration-m3p.5 |
| live-local-smoke | hsdk-conformance-integration-m3p.6 |
| live-daemon-smoke | hsdk-conformance-integration-m3p.7 |
| required-operation-coverage | hsdk-types-codecs-2rb |
| documentation-diagnostics | hsdk-documentation-diagnostics-jvc |
| documentation-examples | hsdk-docs-package-signoff-ip3.4 |
| final-package-checks | hsdk-docs-package-signoff-ip3.5 |
| independent-source-distribution | hsdk-docs-package-signoff-ip3.6 |
| final-parity-signoff | hsdk-docs-package-signoff-ip3.7 |

### External gates and concrete actions

| Gate | Required action / owner | Effect |
| --- | --- | --- |
| Validator rights and viability | The old Haskell dependency is removed. The installed native worker uses MIT-licensed jsonschema 0.55.1 with a documented worker-local patch; 65 resolved dependency notices are bundled. Prior jsoncons and direct-FFI failures remain historical evidence. | Replacement route and rights are established offline; platform/live certification remains separate. |
| Compiler compatibility | User selected GHC9.12 on 2026-09-13. Default shell and SDK CI pin9.12.4; Cabal restricts base to4.21 without relaxing dependencies. GHC9.14 is removed from the required matrix by explicit decision. | Strict builds and3,941 tests pass on the observed macOS/GNU/Linux ARM64 targets; compiler gate accepted. |
| Haskell attribution | User approved omission of unsupported optional SDK metadata on 2026-09-13. Native local/daemon peers verify omission, sanitized ordinary environment and the truthful free-form daemon caller. | Policy resolved; this does not assert upstream support for a Haskell enum value or new live interoperability. |
| Platform execution | Current macOS26.6.2 ARM64 and GNU/Linux ARM64 checks pass with GHC9.12.4: -Werror builds,3,941 tests each, native IPC/timestamps and all six attachment FD/race markers. Linux uses the existing cached, network-disabled route. | Observed ARM64 targets accepted; no unexecuted x86_64/musl or hosted-CI claim. |
| Live verification | September 14 standing approval covers bounded owned-loopback checks. Local multi-turn/interruption evidence is retained; the new-key native daemon scenario passes all seven markers against pinned CLI 0.217.0 in 32.32 seconds. | Authorization, local-live and daemon-live evidence established. Daemon session closure, borrowed-connection reuse, authenticated reconnect and observed owned-process cleanup passed. No account/trust change, shared-endpoint authorization, remote TLS certification or exhaustive process-census claim. |
| Documentation limitations | SDK-owned URL/file/private-type destinations are repaired. Under delegated noncritical judgment, reproduced upstream instance/Generic links and absent dependency documentation are accepted as documented limitations. | Keep generation and URL/file checks enabled; do not claim warning-free Haddock or exhaustive anchors. Warning-free SDK builds and all required checks remain mandatory. |
| Publication/durability | No remote or destination is configured. Obtain an explicit destination/commit/push authorization if publication is wanted. | Does not prevent producing a local package; it prevents claiming publication or an off-machine backup. |

Functional scope remains unchanged. The September 14 standing approval supersedes the earlier per-attempt authorization wait for routine work. Critical decisions, credential creation/disclosure, shared-service mutations and publication remain outside these bounded live scenarios. No verification result is inferred from approval alone.

## Reference provenance

`schema/manifest.json` records the immutable source revisions and schema fingerprints. The four schema documents were supplied at `/Users/johnw/Desktop/droid-json-schema`; the checked-in copies are byte-for-byte identical. The earlier Downloads path is superseded.

| Reference | Baseline | Advertised Factory protocol |
| --- | --- | --- |
| Python SDK | `0.4.0`, commit `6d06b4e613aab3990cf2ced469b5c4e80053e52e` | `1.192.0` |
| TypeScript reference checkout | commit `87b4c4c4e4fc093e10a94f1e767ed57b7cbf597e` | Not an implementation |
| Published `@factory/droid-sdk` | `0.7.0` | `1.151.0` |
| Supplied JSON Schema | Draft-07, four documents | `1.205.0` |
| CLI static-inspection baseline | `0.212.1` | Embedded constant `1.201.1`; later live checks use installed CLI 0.217.0 without replacing this baseline |

The TypeScript checkout contains documentation and examples, with a dependency on the published SDK. Its implementation and declarations were retrieved from the exact public npm archive, without installing or executing the package. Registry integrity was independently checked:

```text
https://registry.npmjs.org/@factory/droid-sdk/-/droid-sdk-0.7.0.tgz
sha512-v3kYE754zYDUyPgEvtC1GNYdj80GVoiTP9FPDkWNDcFhCrRVvQ0B6juZlnitGfwNLM/qIsz9PZaN79ZCnzFm2Q==
```

The extracted archive was inspected at `/tmp/droid-sdk-0.7.0-inspect/package`; that temporary directory is no longer present and is not a build dependency. The pinned archive and integrity above remain the reproducible source reference. Its bundled implementation and declarations were inspected; its source maps do not provide recoverable source content. The protocol constants occur in Python `src/droid_sdk/schemas/constants.py` and the npm archive's `dist/chunk-5UXINOXG.mjs`.

These protocol versions are not interchangeable. The older SDKs and newer schema remain separate conformance references until their differences have been reconciled and tested. A version mismatch warning is not evidence of compatibility.

Static inspection of the installed CLI found readable bundled JavaScript in `/nix/store/w4v4bvjjh3h0bwln5wrbss0p7d6pyhqr-droid-0.212.1/bin/.droid-wrapped` (266,959,280 bytes; SHA-256 `885c25635fc61f475a046ef26bd583b996524cfe091dcd598dbb2fa8e94a5d25`). Its zero-based byte offset 204365608 defines `R0="1.201.1"`; request and response builders use that constant in `factoryProtocolVersion` near offsets 204487570 and 204489130. The executable was not run during this inspection. It supplies historical evidence, not authoritative 1.205.0 runtime contracts.

## Public export inventory

Static source inspection establishes the following namespace counts. Python lists were evaluated from their syntax trees, including the low-level schema reexport; TypeScript export clauses and short-name aliases were resolved against the published declarations. These counts describe exported spellings, not distinct capabilities or implemented parity.

| Namespace | Names | Authoritative declaration |
| --- | ---: | --- |
| Python `droid_sdk` | 157 | `src/droid_sdk/__init__.py:184–344` |
| Python `droid_sdk.schemas` | 359 | `src/droid_sdk/schemas/__init__.py:392–764` |
| Python `droid_sdk.low_level` | 371 | `src/droid_sdk/low_level/__init__.py:29–43`; includes all 359 schema exports |
| TypeScript package root | 868 | `dist/index.d.ts:1` |
| TypeScript Node entry point | 936 | `dist/node.d.ts:2,736` |

The TypeScript root is a subset of the Node surface: 68 names are Node-only and none are root-only. All 936 export aliases resolve lexically to declarations; 282 exported names end in `Schema`. The latter is not a complete schema count, because inferred types, aliases, schema lists and schema-construction functions are separate exports. Compiler-verified dependency closure and cross-version field reconciliation remain open.

Native parity includes capabilities reachable through exported return types, not merely top-level names. In particular, TypeScript `SessionStore`, `SessionStateManager` and `MissionStore` expose behavior through public managers/controllers despite lacking independent entry-point exports (`dist/index-D_SzTnFR.d.ts:112167–114111`). Host-supplied IPC channels, binary relay tunnels, terminal restoration and message-selection helpers have native equivalents and are not excluded as browser-only facilities. Windows job-object retry helpers remain outside the platform scope.

## Observability

The five required children and parent `hsdk-docs-package-signoff-ip3.3.1` are closed/verified offline; dashboard **18/45**. Native observability uses no mandatory backend or global mutable logger:

| Required capability | Native path and boundary |
| --- | --- |
| Structured logging | `DroidLogger`, `DroidLogEvent`, explicit serialized errors and `emitDroidLog`; debug/info/warn/error, scalar attributes, SDK content-key byte lengths and isolated ordinary sink failures. |
| Metrics | `DroidMetricSink` / `recordDroidMetric`; exact counter/histogram values and count/millisecond units, preserving zero/fractions and the shared attribute/failure rules. |
| Tracing | Fresh `DroidTraceContextProvider`, immutable carrier merge and RPC `_meta` injection; omission preserves fields, empty values remain explicit, and failed providers leave the carrier unchanged. |
| Actual instrumentation | Explicit local/borrowed-session and daemon configuration reaches shared RPC exchanges, connection scopes, owned process startup/scopes and optional transport in/out/out_failed records. Trace waits cannot bypass the final generation/admission check. |
| Privacy and cleanup | Default events omit payloads/credentials/argv/stderr/trace values; explicit caller data is not claimed sanitized. Synchronous callback failures are isolated, standard async exceptions propagate, and an existing operation failure wins over later telemetry failure. Existing owners retain cleanup. |

The historical 22-case increment and **3,904-test** integration verified these paths on macOS/GHC 9.10.3; see [usage and constraints](../README.md#observability) and [batch evidence/fess](development.md#observability-integration). These cases remain in the current GHC 9.12.4 macOS/GNU/Linux ARM64 suites. The subsequent attribution decision, bounded live checks and release verification are recorded separately; the historical compiler version is not a current build requirement.

## Saved-session file scanning

`hsdk-f20.1`, `.2`, `.3` and the saved-session parent are closed/verified offline: **3/3** required children are complete. Scanning and selection retain actual observations, explicit birth availability and the compatibility projection; the native timestamp branches now execute on macOS and GNU/Linux ARM64. `DroidSessionFile` preserves original header/settings objects and timestamp provenance rather than rebuilding metadata. Broader release certification remains separate.

| Contract | Reconciled native behavior |
| --- | --- |
| Locations and identity | The default root is current home plus `.factory/sessions`; callers select an explicit directory or file. Only direct `.jsonl` entries are scanned, in filename order, and IDs/settings paths derive from filenames rather than header IDs. TypeScript's empty ID for `.jsonl` is retained; selection of project/legacy layouts belongs to `.2`. |
| Header titles | Require `type: session_start` and a string title. A nonblank legacy `sessionTitle` takes precedence without trimming, following TypeScript's normalization; Python ignores this legacy field. Both original fields remain in raw JSON. |
| Optional fields | Malformed owner/cwd fields use Python's empty-owner/absent-cwd behavior rather than TypeScript's whole-header rejection. Raw cwd is not resolved by the file layer, and invalid optional mission values are absent from typed projections but retained in JSON. |
| Counts | Count physical lines after the first, ignoring only CR/space/tab within LF-delimited lines, including an unterminated final line. Later records need not be valid JSON. VT/FF lines count as in TypeScript; Python's byte stripping ignores them. |
| Settings and missions | Preserve raw object settings. Only valid TypeScript-shaped archivedAt/tags fields supply first-tag mission/role metadata through existing `SessionTag` and `DecompSessionType` codecs. A valid header role wins; the first mission-tag ID, including empty text, outranks the header ID. No merging from later matching tags. |
| Archive and favorites | Honor Python's truthy archive marker even when unrelated settings validation would fail, rather than expose the archived session as the older TypeScript reader does. Missing/malformed optional settings/favorites are tolerated; favorites retain only exact strings, including empty IDs. |
| Ownership and failures | Open regular files nonblocking/CLOEXEC, compare device/inode after acquisition, and read at most the observed size in 64 KiB chunks. Early EOF rejects the observation; later appends are ignored. Missing/permission-denied entries are absent; other I/O errors and cancellation propagate. Stable symlinks are followed, not sandboxed; neither contents nor multiple files form an immutable snapshot. |

File-layer checkpoint evidence: **24 new native cases**, **58 scanner/attachment checks / 0.17s**, five additional 24-case runs, warning-free all-component build, changed-file tooling, seven extracted TypeScript schema/runtime groups and five original Python parser/dataclass groups. The [compiled example](../README.md#saved-session-file-observations) and [full fess](development.md#saved-session-file-scanning-delivery) record that checkpoint before later selection/timestamp acceptance. Sources: pinned Python `_high_level/discovery.py` and TypeScript `node.mjs:3936–4153`, `chunk-5UXINOXG.mjs:8468–8510`; receipts `/tmp/droid-saved-session-scan.nwXvhGDX/`.

## Saved-session selection

`listDroidSessions` is available through `Factory.Droid` and `Factory.Droid.Discovery`. `ListDroidSessionsOptions` supplies optional cwd/root, outside-cwd selection and an optional nonnegative limit. Defaults are one captured process cwd, the current home directory's `.factory/sessions`, current-workspace selection and no limit. Explicit empty paths refer to the captured cwd; a leading tilde is literal, not expanded by the list APIs. Negative limits and NUL options fail before filesystem work; zero validates options and returns without looking up the root.

`DroidSavedSession` composes the complete `DroidSessionFile` with a normalized cwd view and root-level favorite membership. It does not reconstruct messages, lose extensions/timestamp provenance or start an engine. Existing direct-directory/regular-file readers and descriptor ownership are reused. Stable symlinks remain supported, not sandboxed; no stored metadata chooses a file to read.

| Selection contract | Native resolution |
| --- | --- |
| Project/legacy layouts | Scan root `.jsonl` files and the selected project-directory keys; outside-cwd mode scans root files plus one level of `-`-prefixed directories. Do not descend into ordinary/nested directories. |
| Workspace path conventions | Retain canonical filesystem and lexical resolution used by the baselines. Python's backslash-escaped/partially canonical key and TypeScript's literal POSIX/strictly canonical-or-lexical key both remain discoverable. Parent segments across symlinks and missing suffixes are covered by actual source/native fixtures. |
| Legacy filter | When outside-cwd mode is false, require a match under either reference cwd meaning. Missing/NUL stored cwd cannot match; selected project-directory entries retain descriptive cwd independently of their membership. Raw cwd remains in the file observation. |
| Duplicate IDs and order | Filter first, then choose the greatest exact modification time per ID. Ties use code-point ID/path ordering, not filesystem enumeration or locale. All candidate metadata remains intact in the selected wrapper. |
| Limits and source defect | Limit the final sorted unique list. TypeScript's `limit + 20` parse cutoff can lose a unique candidate behind duplicates; the native implementation follows the complete-selection behavior rather than reproducing that shortfall. Nonzero queries can read all eligible directory candidates. |
| Errors and ownership | A root that is itself a file yields no sessions instead of probing `.favorites` beneath it. Existing missing/permission handling and unexpected-I/O propagation remain. Listing cancellation preserves its exception and closes the active descriptor. |

Selection checkpoint evidence: **24 new selection cases**, default/relative/empty/tilde behavior in isolated native self-peers, the reproduced/fixed non-directory-root failure, and a reviewer-requested zero-I/O regression with a positive control. Ten TypeScript and ten Python public-list groups execute original code against shared fixtures; the native API passes those same ten scenarios under the documented combined semantics. That integration passed **3,882 tests / 12.74s**, all-component build/full tooling/audits and an exact compiled facade example. [Full fess](development.md#saved-session-selection-delivery); receipts `/tmp/droid-saved-session-selection.C68hZhyW/`. Subsequent timestamp and discovery-parent acceptance are recorded below; broader release certification remains separate.

### Birth-time checkpoint

`hsdk-f20.3` is **closed/verified offline**. The installed `unix-2.8.7.0` API reports `haveStatx=False` on macOS and its `getExtendedFileStatus` failure was reproduced. Native `fstat` exposes `st_birthtimespec`. The C bridge reads this field on Darwin and uses Linux descriptor-only `statx` with `AT_EMPTY_PATH`, requesting `STATX_BTIME` and checking the returned mask. Missing OS/filesystem support is absence; zero remains a reported value, and other failures retain errno. The existing file owner captures the observation on its open descriptor; settings/favorites do not request it.

`sessionFileBirthTime :: Maybe UTCTime` preserves availability. `sessionFileCreatedAt` uses it when present, otherwise the separately retained status-change time as an explicit SDK-compatible fallback. Python 3.10 can fall back to ctime where its stat object lacks birth time; the native implementation retains a reported birth value when available, as the TypeScript/libuv path can. All underlying observations remain accessible, and no metadata observation is an immutable filesystem snapshot.

Darwin C and linked Haskell results match Node bigint birth nanoseconds exactly. Actual C and Haskell bad-FD probes retain EBADF. Four new native cases cover reported availability, birth stability across metadata changes, zero/negative/fractional projection values and inode-bound observations across path replacement. **3,858 full tests / 12.88s**, warning-free Haskell/C builds, full-tree tooling and five audits pass. Cabal source listing includes the bridge and internal module; this is not an independently built sdist.

**Linux compilation and execution are now established on ARM64.** Declared public Nix tools run in the existing cached image without an image pull, network access or app launch. The C bridge matches direct statx nanoseconds, retains birth time across mtime changes and reports EBADF. The Haskell binding equals the independent `unix` statx result exactly; four scanner/timestamp cases and the full 3,935-test suites pass. The timestamp child and discovery parent are accepted, without claiming other architectures/libcs. [Current Linux evidence/fess](development.md#linux-execution-checkpoint); the [original checkpoint](development.md#saved-session-timestamp-checkpoint) retains the earlier compiler-availability blocker. The [Linux statx contract](https://www.man7.org/linux/man-pages/man2/statx.2.html) requires checking `stx_mask`, not assuming requested fields were returned.

## Stream and functional helper mapping

The September 11 mapping (`hsdk-docs-package-signoff-ip3.2.2.1`) compares required behavior with existing native facilities, not exported spellings. The six implementation children below record delivered and remaining scope. Private code is reuse evidence, not proof of a public standalone capability; a typed record selector or pattern match can, however, supply the same operation without another wrapper.

Source locators below refer to the pinned TypeScript runtime `dist/chunk-5UXINOXG.mjs` and Python `src/droid_sdk/`. The cached runtime SHA-256 is `c7d07f5089088711616ccca3c2b593bbd85ea5c65252b64b86351bd577c4bde3`. Reference declarations guide navigation; the bodies establish the distinctions.

| Existing child under `.2.2` | Native reuse and demonstrated remaining behavior | Source |
| --- | --- | --- |
| `.2` Standalone stream feed | Implemented through `Factory.Droid.Stream`: scoped feeding, one caller-thread consumer, complete/partial frames, exact terminal identity, result/failure inspection, monotonic/reported duration, enrichment, deadlines/cancellation and logical close. Normal local/daemon turns use that same queue/collector. Pure `StreamState`/`stepStream`/`summarizeStream` supply manual completion with absent/latest/explicit usage and no invented peer receipt; both result paths share output decoding. **31 new native tests**, **3,779-test acceptance** integration, eight extracted tracker checks and two compiled README examples; retained in the later legacy boundary. | TS 8576–8931; Python `_high_level/streaming.py:763–1006`; native `Internal.Stream`, `Internal.Output`, `Internal.Session` |
| `.3` Legacy completion | Implemented by `withDroidLegacyStream` in the same typed feed. Initial Idle and explicit completions are ignored; any non-idle→Idle observation returns a distinct `DroidIdleCompletion` with latest optional usage. The low-level event families, tool-name correlation, caller-bracketed subscription cleanup and common deadlines/cancellation are retained. Normal turns remain explicitly correlated; no success/reason/ACK or remote interruption is invented. **16 new native cases; 3,795-test integration**, six extracted iterator-control groups with a converted-event fixture boundary, one structural converter check and an exact compiled example. Native malformed-known-payload failure and full precise usage differ deliberately from Python's silent skipping/four-counter projection. | Python `client.py:1649–1752`, `stream.py:140,187–280`; native `Internal.Stream` completion policy |
| `.4` Message-chain helpers | Verified through existing public `SessionState.repairMessageParents`, `orderMessagesByParentChain`, `filterDisplayMessages` and newly exported `persistedHook`. Runtime bodies are unchanged. Hook detection distinguishes metadata presence from visibility, including whitespace names and empty command lists. Four new pure cases join **92 focused message-state/codec checks**, eight extracted reference groups, an all-component build and an exact compiled example. Deterministic native UTF-16 ties and the typed malformed-message boundary remain explicit differences; no history manager was added. | TS 9293–9416; native `SessionState.hs`; [delivery/fess](development.md#message-chain-helper-delivery) |
| `.5` Subagent linkage helpers | `Schema.Session.findSubagentSessionTag` preserves the first typed tag; `inspectSubagentSessionTag` preserves the first raw object, including arbitrary calling metadata. Standard Maybe/record/KeyMap operations supply presence and IDs without later-tag fallback or merging. The raw projection corrects the earlier typed-only acceptance overclaim; strict codecs and state admission remain unchanged. Four original plus two raw regression cases are included in the **3,830-test** boundary; typed/raw exact examples compile. | TS 9459–9470; native `Schema.Session`; [correction/fess](development.md#raw-subagent-metadata-correction) |
| `.6` Queue review helpers | Verified `SessionState` helpers supply a distinct `QueueDisplayGroup`, optional `queueReviewPriority` and reviewability derived from presence, preserving priority zero. All five kinds retain the mapped steering/queued and reviewability values; existing daemon/placement classification and queue transitions are unchanged. Two new cases cover the full table and stable pure review selection with exact payloads. **22 focused tests**, six extracted reference groups, all-component build and an exact compiled example pass. No dequeue or replay is introduced. | TS 14006–14061; native `SessionState` queue APIs; [delivery/fess](development.md#queue-review-helper-delivery) |
| `.7` Usage/content/tool/inspection helpers | Public projections and shared arithmetic/predicates are implemented in their existing owners; the contracts below distinguish raw inspection from wire admission. Native constructors/patterns/exceptions provide the remaining equivalents. **23 new utility cases**, **3,830-test integration**, nine extracted source groups plus one structural equality check, full tooling and an exact compiled example pass. | TS 9261–9291, 9418–9524; [delivery/fess](development.md#utility-helper-delivery) |

### Utility contracts and native equivalents

| Capability | Public native equivalent and preserved boundary |
| --- | --- |
| Usage aggregation | `Schema.Usage.sumTokenUsage` sums known `Scientific` counters and credits exactly. Empty input returns zero counters/credits, absent credits contribute zero, and unknown extensions are not summed. `Mission.sumUsage` reuses the arithmetic while retaining absence for empty mission usage. |
| Tool-use summary | `SessionState.summarizeSessionToolUsage` counts assistant tool-use blocks and returns optional positive last-minus-first duration in supplied order, using nonzero updated time or created time. `refreshInvocationSummary` reuses it without changing empty/invalid-history guards or prior-duration preservation. No fake linked session is needed. |
| Streaming-update comparison | `Schema.Notifications.equalToolStreamingUpdates` uses standard Object key deletion and equality, ignoring only top-level `timestamp`; nested/extension differences remain. It does not apply events or change existing deduplication/deadline policy. Native tests execute this behavior; the original dependency-based comparison was structurally checked, not executed through an invented substitute. |
| Content construction | `Schema.Content.buildUserMessageContent` preserves images→documents→optional text, explicit optional ECMAScript trimming and absent/empty distinctions. Defaults keep text verbatim and explicitly empty text. Supplied sources are retained; normal Input file/base64/limit validation remains separate and unchanged. |
| Tool-result selection/identity/pending | `ContentToolResult` patterns and `toolResultToolUseId` supply typed operations. Ordinary raw Object filtering and `inspectToolResultId` retain arbitrary values, preferring canonical `toolUseId` unless nullish, then `tool_use_id`. Canonical wire decoding remains camel-case-only. Hydration and public callers share `isPendingToolResult`, which matches only exact scalar pending text, not block arrays or absence. |
| Permission display input | `Schema.Interaction.permissionToolInputForDisplay` retains nonempty input strings, including whitespace. Only matching exit-spec/propose-mission details fill missing, empty or non-string plan/proposal/title values with nonempty detail strings. It creates a display object, not a permission decision or stored-input mutation. |
| Decoupled settings presence | `Schema.Settings.hasDecoupledInteractionSettings` tests whether either interactionMode or autonomyLevel key exists, including null or invalid values. Parsed settings and reset/update validation are unchanged. |
| Partial envelope inspection | `Schema.RPC.inspectJsonRpcEnvelope` takes an explicit expected version, accepts partial objects including `{}`, validates present envelope/type/method/nullable-ID/metadata fields, and returns the original object with an optional differing-version observation. Native known metadata uses the existing supplied 1.205.0 codec; native nested extensions are preserved where the older source strips them. Strict wire routing remains authoritative; no negotiation or attribution is invented. |
| Machine kind | `Schema.Host.machineConnectionType` maps computer→computer, local→tui, ephemeral→workspace, with Nothing for unknown names. Labels are descriptive, not evidence of locality, authentication or ownership. |
| Error conversion | Native `Exception`/`SomeException`, `toException`, `throwIO` and existing typed SDK failures cover the language-level error capability. JavaScript's coercion of arbitrary values into printable `Error` objects does not require a duplicate exception hierarchy or payload-dumping fallback. Original async exceptions and the explicit sensitive-cause boundary remain intact. |

The standalone leaf now supplies output adaptation and tool-result/progress correlation, rather than retaining raw IDs alone. Source queue leaks, swallowed malformed notifications, post-close feeding quirks and detached cleanup are not compatibility requirements. Native scopes retain their established ownership and error guarantees.

Acceptance evidence is recorded in [mapping](development.md#streamhelper-mapping-delivery), [standalone streams](development.md#standalone-stream-delivery), [legacy streams](development.md#legacy-stream-delivery), [message helpers](development.md#message-chain-helper-delivery), [tag correction](development.md#raw-subagent-metadata-correction), [queue review](development.md#queue-review-helper-delivery) and [utilities](development.md#utility-helper-delivery). All **7/7** helper children and parent `.2.2` are closed/verified after [parent acceptance](development.md#streamhelper-parent-acceptance). At that checkpoint the dashboard was **17/45**, and the shared **3,830-test** boundary included every helper increment; ten exact README examples compiled and 51 selected extracted-runtime/control groups plus four explicitly structural checks passed. Discovery and the required conformance gates were subsequently accepted; distribution evidence remains separately scoped.

Explicit-turn stream results are immutable observations of a matching peer completion, distinct from successful callback consumption; a later callback failure or scope timeout does not erase an earlier receipt. Python clears its result on iterator timeout. The native action still fails, closes feeding and drops undelivered frames; no remote rollback or successful action is inferred. Legacy results are separately typed idle observations with optional usage, not peer completion receipts. Pure manual summaries carry explicit caller duration/reason and optional usage. Neither relaxes wire completion codecs or fabricates peer events.

## Schema inventory

| Document | Definitions |
| --- | ---: |
| `shared.schema.json` | 98 |
| `droid.schema.json` | 223 |
| `daemon.schema.json` | 418 |
| `protocol.schema.json` | 739 references to the preceding definitions |

The corpus contains 739 distinct owned definitions, 2,209 resolvable references, and 188 method-bearing definitions representing 183 distinct method names. The index covers each owned definition exactly once. Eighty-four annotations identify behavior not fully expressed by the JSON Schema: 11 custom refinements, 11 normalizations, and 62 invalid-value fallbacks. These annotations require reference-source inspection and behavioral tests; Aeson round trips alone cannot establish conformance.

Generate the complete definition inventory, including source references, wire methods, response/result associations, and runtime-only constraint locations:

```sh
python3 scripts/reference_schemas.py --inventory
```

Verify both the checked-in snapshot and its source, then run the audit regression checks:

```sh
python3 scripts/reference_schemas.py
python3 scripts/reference_schemas.py /Users/johnw/Desktop/droid-json-schema
python3 -m unittest discover -s scripts -p 'test_*.py' -v
```

Python is a reference-audit development tool, not a dependency of the Haskell SDK runtime. The audit verifies provenance and reference integrity; it does not implement Draft-07 instance validation.

## Implemented shared enum foundation

`src/Factory/Droid/Schema/Enums.hs` implements the following definitions from `shared.schema.json`. `test/Main.hs` verifies every constructor against the corresponding schema literal, rejects invalid JSON and unknown literals, and checks encoding/decoding properties. All 60 checks pass on GHC 9.10.3; this is not full schema or SDK parity.

| Schema definition | Haskell type |
| --- | --- |
| `AutonomyLevelSchema` | `AutonomyLevel` |
| `CustomModelAuthModeSchema` | `CustomModelAuthMode` |
| `DroidInteractionModeSchema` | `DroidInteractionMode` |
| `FileEditToolProfileSchema` | `FileEditToolProfile` |
| `MessageRoleSchema` | `MessageRole` |
| `MessageVisibilitySchema` | `MessageVisibility` |
| `ModelFallbackReasonSchema` | `ModelFallbackReason` |
| `ModelProviderSchema` | `ModelProvider` |
| `ReasoningEffortSchema` | `ReasoningEffort` |
| `SandboxModeSchema` | `SandboxMode` |
| `SessionOriginSchema` | `SessionOrigin` |
| `SettingsLevelSchema` | `SettingsLevel` |
| `SkillLocationSchema` | `SkillLocation` |
| `ToolExecutionModeSchema` | `ToolExecutionMode` |
| `WorktreeLifecycleSchema` | `WorktreeLifecycle` |

## Implemented model-catalog codecs

`src/Factory/Droid/Schema/Models.hs` implements eight definitions from the supplied 1.205.0 schema. `test/ModelsSpec.hs` supplies 43 checks for catalog records, fallback metadata, mission settings, compaction selectors and Bedrock request metadata. Model discovery, compaction and mission operations themselves are not implemented.

| Schema definition or relative path | Haskell type | Verification |
| --- | --- | --- |
| `ModelMetadataSchema` | `ModelMetadata` | Full-field golden fixture, required/null/default checks, precise numeric decoding, extensions |
| `ModelMetadataSchema/properties/kind` | `ModelKind` | Every inline enum literal, decode branches and invalid values |
| `ModelMetadataSchema/properties/tier` | `ModelTier` | Every inline enum literal, decode branches and invalid values |
| `ModelInfoSchema` | `ModelInfo`, `ModelAvailability` | Enabled/disabled branches, required or forbidden reason, reserved-key precedence |
| `ListModelsOptionsSchema` | `ListModelsOptions` | Missing/false/true distinction, null rejection and extensions |
| `ListModelsResultSchema` | `ListModelsResult` | Catalog round trip, required models and extensions |
| `ModelFallbackSchema` | `ModelFallback` | Required fields, enum reasons and extensions |
| `MissionModelSettingsSchema` | `MissionModelSettings` | Full/minimal fields, enum reasoning efforts, flags and extensions |
| `CompactionModelSchema` | `CompactionModel` | Current-model literal, all 97 built-in literals, custom-pattern and invalid-input checks |
| `CustomModelBedrockRequestMetadataSchema` | `BedrockRequestMetadata` | Exact string-valued object domain, including empty keys/values |

All paths above belong to `shared.schema.json#/definitions/`. The metadata fields are mapped individually:

| Wire field | Haskell selector |
| --- | --- |
| `id` | `metadataId` |
| `displayName` | `metadataDisplayName` |
| `shortDisplayName` | `metadataShortDisplayName` |
| `modelProvider` | `metadataProvider` |
| `supportedReasoningEfforts` | `metadataSupportedReasoningEfforts` |
| `defaultReasoningEffort` | `metadataDefaultReasoningEffort` |
| `isCustom` | `metadataIsCustom` |
| `noImageSupport` | `metadataNoImageSupport` |
| `supportsImageGeneration` | `metadataSupportsImageGeneration` |
| `tier` | `metadataTier` |
| `tokenMultiplier` | `metadataTokenMultiplier` |
| `promoLabel` | `metadataPromoLabel` |
| `kind` | `metadataKind` |
| `variantBadge` | `metadataVariantBadge` |

`ModelInfo` retains these fields through `modelMetadata`. Its `disabled` and `disabledReason` fields are represented jointly by `modelAvailability`: `ModelDisabled reason` requires a reason; `ModelEnabled` forbids one, including explicit null. Missing `disabled` decodes as enabled and encodes as false. The discovery option `includeDisabled` maps to `modelsIncludeDisabled`; the result field `models` maps to `catalogModels`. Additional properties reside in the corresponding additional-fields record selector and cannot override typed fields, including absent options.

Optional non-nullable fields decode to `Maybe` but reject explicit JSON null. Absent `isCustom` defaults to false and is emitted explicitly on encoding. `tokenMultiplier` uses `Scientific`, retaining JSON decimal precision without a binary floating-point conversion. Unknown `kind` or `tier` values fail decoding rather than becoming arbitrary strings.

These guarantees follow the supplied wire schema, not all permissive input behavior of Python `schemas/models.py`. Python admits null for several optional fields, arbitrary kind/tier strings, and a null reason on an enabled model; it forbids extensions on discovery options. The 1.205.0 schema differs on each point. Older-peer compatibility remains subject to protocol-baseline reconciliation; these codecs alone do not establish it.

`ModelFallback` maps `requestedModel` to `fallbackRequestedModel` and `reason` to `fallbackReason`, with open fields in `fallbackAdditionalFields`. `MissionModelSettings` maps `workerModel`, `workerReasoningEffort`, `validationWorkerModel`, `validationWorkerReasoningEffort`, `skipScrutiny` and `skipUserTesting` to the corresponding `mission*` selectors; extensions are stored in `missionSettingsAdditionalFields`. All mission fields are optional and non-nullable. These settings do not start missions or skip SDK verification. The TypeScript mission settings have the same declared fields (`dist/chunk-5UXINOXG.mjs:2790–2797`).

`CompactionModel` preserves its selector text behind a validated constructor. `currentCompactionModel` represents `current-model`; `builtinCompactionModels` enumerates the 97 supplied literals. `mkCompactionModel` also accepts the exact `^custom:.+$` pattern: a nonempty suffix with no ECMAScript line terminators, without trimming or case folding. Edge cases were compared with the literal JavaScript regex from the published SDK (`dist/chunk-5UXINOXG.mjs:2652–2656`). This does not assert model availability or apply a parent field's fallback policy.

`BedrockRequestMetadata` is Aeson's string-valued `KeyMap`, matching the explicitly dynamic map schema. The complete managed-custom-model configuration remains unimplemented pending version-matched runtime refinement evidence.

## Implemented token-usage codecs

`src/Factory/Droid/Schema/Usage.hs` implements `shared.schema.json#/definitions/TokenUsageSchema` and the inline `lastCallTokenUsage` object shared by `LoadSessionResultSchema` and `SessionTokenUsageChangedNotificationSchema` in `droid.schema.json`. The load-session result remains unimplemented; the token-usage notification body is covered below. `test/UsageSpec.hs` supplies 23 checks, including numeric properties and an equality check between the two inline schema definitions.

| Type | Wire field | Haskell selector |
| --- | --- | --- |
| `TokenUsage` | `inputTokens` | `usageInputTokens` |
| `TokenUsage` | `outputTokens` | `usageOutputTokens` |
| `TokenUsage` | `cacheCreationTokens` | `usageCacheCreationTokens` |
| `TokenUsage` | `cacheReadTokens` | `usageCacheReadTokens` |
| `TokenUsage` | `thinkingTokens` | `usageThinkingTokens` |
| `TokenUsage` | `factoryCredits` | `usageFactoryCredits` |
| `LastCallTokenUsage` | `inputTokens` | `lastCallInputTokens` |
| `LastCallTokenUsage` | `cacheReadTokens` | `lastCallCacheReadTokens` |
| `LastCallTokenUsage` | `outputTokens` | `lastCallOutputTokens` |

All numeric fields use `Scientific`: the schema specifies numbers without integer, nonnegative or machine-width constraints. `factoryCredits` and last-call `outputTokens` are optional but non-nullable. Additional properties are retained in `usageAdditionalFields` or `lastCallAdditionalFields`, without permitting reserved-key overrides.

The supplied numeric shapes agree with the published TypeScript `TokenUsageSchema` and its last-call projection (`dist/chunk-5UXINOXG.mjs:2594–2601,2938–2944,3824–3830`). Python `schemas/session.py:43–63` instead types token counts as integers and admits null for optional numbers. Python preserves extensions while this TypeScript schema strips them. The Haskell wire records preserve the supplied numeric domain and the Python extension capability; older input coercions and null acceptance are not silently reproduced.

## Implemented content codecs

`src/Factory/Droid/Schema/Content.hs` implements 14 owned definitions from `shared.schema.json` and the supporting inline types. `test/ContentSpec.hs` supplies 131 checks: full/minimal golden objects, schema field and literal coverage, required/null behavior, extension preservation and precedence, complete union membership, duration properties and cache-label composition. Message records and generic RPC envelopes are implemented below; operation-specific envelopes remain separately tracked.

| Definition | Haskell type | Field mapping, excluding common identity/extensions |
| --- | --- | --- |
| `BaseContentBlockSchema` | `BaseContentBlock` | `id` → `blockId`; extensions → `blockAdditionalFields` |
| `TextBlockSchema` | `TextBlock` | `type` = `text`; `text` → `textBlockText` |
| `Base64ImageSourceSchema` | `Base64ImageSource` | `type` = `base64`; `data` → `imageSourceData`; `mediaType` → `imageSourceMediaType` (`ImageMediaType`) |
| `ImageBlockSchema` | `ImageBlock` | `type` = `image`; `source` → `imageBlockSource`; `generated` → `imageBlockGenerated` |
| `ThinkingBlockSchema` | `ThinkingBlock` | `type` = `thinking`; `signature` → `thinkingBlockSignature`; `signatureProvider` → `thinkingBlockSignatureProvider`; `thinking` → `thinkingBlockThinking`; `durationMs` → `thinkingBlockDuration` |
| `RedactedThinkingBlockSchema` | `RedactedThinkingBlock` | `type` = `redacted_thinking`; `data` → `redactedThinkingData` |
| `ToolUseSchema` | `ToolUseBlock` | `type` = `tool_use`; `id` → `toolUseId`; `input` → `toolUseInput`; `name` → `toolUseName`; `namespace` → `toolUseNamespace`; `scriptExecution` → `toolUseScriptExecution`; `thoughtSignature` → `toolUseThoughtSignature` |
| `Base64PDFSourceSchema` | `Base64PDFSource` | `type` = `base64`; `mediaType` = `application/pdf`; `data` → `pdfSourceData`; `parsedData` → `pdfSourceParsedData`; `name` → `pdfSourceName`; `path` → `pdfSourcePath` |
| `PlainTextSourceSchema` | `PlainTextSource` | `type` = `text`; `mediaType` = `text/plain`; `data` → `plainTextSourceData`; `name` → `plainTextSourceName`; `mime` → `plainTextSourceMime` |
| `DocumentSourceSchema` | `DocumentSource` | `PDFDocument` or `PlainTextDocument` |
| `DocumentBlockSchema` | `DocumentBlock` | `type` = `document`; `source` → `documentBlockSource` |
| `ToolResultSchema` | `ToolResultBlock` | `type` = `tool_result`; `toolUseId` → `toolResultToolUseId`; `content` → `toolResultContent`; `isError` → `toolResultIsError` |
| `ContentBlockSchema` | `ContentBlock` | `ContentText`, `ContentImage`, `ContentThinking`, `ContentRedactedThinking`, `ContentToolUse`, `ContentToolResult`, `ContentDocument` |
| `CacheLabelSchema` | `CacheLabel` | `cache_control` → `labelCacheControl`; extensions → `labelAdditionalFields` |

Optional block identity and extensions are composed through `textBlockBase`, `imageBlockBase`, `thinkingBlockBase`, `redactedThinkingBase`, `documentBlockBase` or `toolResultBase`. Tool-use identity is required and therefore remains a separate `Text` field. Source objects and tool-use/script records have their own additional-fields selectors. Extensions never override typed fields or literal discriminants.

Inline `ScriptExecution` maps `runId` to `scriptRunId` and `outerToolUseId` to `scriptOuterToolUseId`. `SignatureProvider` distinguishes `SignedBy ModelProvider` from the explicit `UnknownSignatureProvider` literal; other names fail decoding. The opaque `ThinkingDuration` type admits nonnegative fractional milliseconds through `mkThinkingDuration`, and its accessor returns the exact `Scientific` value.

`ToolResultContent` distinguishes a `ResultText` string from `ResultBlocks`. `ToolResultItem` admits only text, image and document blocks. Absent content, an empty string and an empty array are distinct; null is invalid. Source data remains a JSON string: these codecs do not establish valid base64, file contents, attachment limits or signature authenticity.

The seven variants and source shapes agree with the older TypeScript definitions (`dist/chunk-5UXINOXG.mjs:2170–2254`). Its tool-use shape lacks `namespace` and `scriptExecution`, which the supplied schema adds. Python `schemas/messages.py` retains extensions but admits null in several optional fields and arbitrary signature-provider strings. The Haskell codecs use the supplied constraints and retain extensions; they do not silently reproduce older permissive decoding.

`CacheControl` represents the literal `ephemeral` type, with `ttl` mapped to `cacheTTL` and open fields to `cacheAdditionalFields`. `CacheTTL` distinguishes `5m` from `1h` without adding a default. `CachedContentBlock` combines `cachedBlockContent` with `cachedBlockControl`; typed cache fields replace conflicting content extensions, including removal when absent. The shared content object encoder is used for ordinary, concrete and cached blocks. Nested tool-result items retain their ordinary schema rather than acquire recursive cache validation. The older TypeScript cache-control definition has no typed TTL field (`dist/chunk-5UXINOXG.mjs:2255–2259`).

## Implemented shared session metadata

`src/Factory/Droid/Schema/Session.hs` implements three further shared definitions. `test/SessionSpec.hs` retains 12 checks for this shared surface and adds local session/reference checks described below. These are metadata codecs, not session operations or an attribution policy.

| Definition | Haskell type | Field mapping |
| --- | --- | --- |
| `SessionIdParamsSchema` | `SessionIdParams` | `sessionId` → `sessionParamsId`; extensions → `sessionParamsAdditionalFields` |
| `SessionTagSchema` | `SessionTag` | `name` → `sessionTagName`; `metadata` → `sessionTagMetadata`; extensions → `sessionTagAdditionalFields` |
| `SessionWorktreeMetadataSchema` | `SessionWorktreeMetadata` | `repoRoot` → `worktreeRepoRoot`; `branch` → `worktreeBranch`; `lifecycle` → `worktreeLifecycle`; `parentWorktreePath` → `worktreeParentPath`; `path` → `worktreePath`; `removedAt` → `worktreeRemovedAt`; `setupProfileId` → `worktreeSetupProfileId`; extensions → `worktreeAdditionalFields` |

`SessionTagName` has a private constructor and a validating `mkSessionTagName`; only the empty string is rejected. Tag metadata is an optional string-valued map, not nullable. Whitespace and Unicode names, empty metadata and extension fields are preserved. Python `schemas/session.py:34–40` instead permits empty names and null metadata while forbidding extensions; TypeScript `SessionTagSchema` requires nonempty names and strips extensions (`dist/chunk-5UXINOXG.mjs:2605–2608`). The supplied schema governs the wire codecs.

Session IDs are strings without an added UUID constraint. Worktree metadata requires only `repoRoot`; its paths and `removedAt` remain strings without filesystem or date validation, as specified by the supplied definitions.

## Implemented message records

`src/Factory/Droid/Schema/Messages.hs` implements `FactoryDroidMessageSchema` as `Message ContentBlock` and `FactoryDroidMessageWithCachingSchema` as `Message CachedContentBlock`. The public aliases are `FactoryDroidMessage` and `FactoryDroidMessageWithCaching`. The schemas differ only in the content-item type; the record and its codecs are shared. `test/MessagesSpec.hs` supplies 19 checks.

| Wire field | Haskell selector |
| --- | --- |
| `id` | `messageId` |
| `role` | `messageRole` |
| `content` | `messageContent` |
| `createdAt` | `messageCreatedAt` |
| `updatedAt` | `messageUpdatedAt` |
| `parentId` | `messageParentId` |
| `visibility` | `messageVisibility` |
| `openaiMessageId` | `messageOpenAIMessageId` |
| `openaiPhase` | `messageOpenAIPhase` |
| `openaiEncryptedContent` | `messageOpenAIEncryptedContent` |
| `openaiReasoningId` | `messageOpenAIReasoningId` |
| `openaiReasoningSummary` | `messageOpenAIReasoningSummary` |
| `geminiThoughtSignature` | `messageGeminiThoughtSignature` |
| `chatCompletionReasoningField` | `messageChatCompletionReasoningField` |
| `chatCompletionReasoningContent` | `messageChatCompletionReasoningContent` |
| `isUserVisible` | `messageIsUserVisible` |
| `isError` | `messageIsError` |
| `userMessageSource` | `messageUserSource` |
| `interactionMode` | `messageInteractionMode` |
| `modelId` | `messageModelId` |
| `routerId` | `messageRouterId` |
| `reasoningEffort` | `messageReasoningEffort` |
| `apiProvider` | `messageApiProvider` |
| `hookEventName` | `messageHookEventName` |
| `hookMatcher` | `messageHookMatcher` |
| `hookCommands` | `messageHookCommands` |
| `hookStatus` | `messageHookStatus` |
| `hookResults` | `messageHookResults` |
| `hookToolCallId` | `messageHookToolCallId` |
| `hookParentId` | `messageHookParentId` |
| `hookOrder` | `messageHookOrder` |
| `hookPreventedAction` | `messageHookPreventedAction` |
| `hiddenFromUserViews` | `messageHiddenFromUserViews` |
| `hookStartTime` | `messageHookStartTime` |
| `hookEndTime` | `messageHookEndTime` |
| `isParallelExecution` | `messageIsParallelExecution` |
| `parallelGroupId` | `messageParallelGroupId` |

Extensions reside in `messageAdditionalFields`. `messageOpenAIPhase` uses `Maybe (Maybe OpenAIPhase)`: absent, explicit null and a phase literal remain distinct. Other optional fields are non-nullable. `ApiProvider`, `OpenAIPhase`, `ChatCompletionReasoningField` and `HookStatus` implement the four inline enumerations; API routes are not conflated with model providers.

Inline `PersistedHookCommand` maps `command` to `hookCommandText` and `timeout` to `hookCommandTimeout`. `PersistedHookResult` maps `exitCode`, `stdout`, `stderr` and `suppressOutput` to the corresponding `hookResult*` selectors. Both retain extensions. All numeric fields use `Scientific` without inventing integer, range or timestamp-format constraints. Commands are not executed, and provider signatures and hook output are not authenticated by decoding.

The older TypeScript message definitions already preserve nullable phases and distinguish ordinary from cached content (`dist/chunk-5UXINOXG.mjs:2260–2320`), but omit the newer `apiProvider`, `hookParentId`, `hookOrder` and `hookPreventedAction` fields. Python `schemas/messages.py` permits null for many more fields, models several constrained enums as strings, and treats hook records as dictionaries. These older input permissiveness differences do not replace the supplied wire constraints.

## Implemented base RPC records

The following ten owned shared definitions and their inline enums are implemented in `src/Factory/Droid/Schema/RPC.hs`. `test/RPCSpec.hs` supplies 57 checks for these bare shapes and result bodies. Complete envelopes are covered in the next section; SDK-level exception mapping remains separate work.

| Definition | Haskell type | Field mapping |
| --- | --- | --- |
| `JsonRpcErrorSchema` | `JsonRpcError` | `code` → `rpcErrorCode`; `message` → `rpcErrorMessage`; `data` → `rpcErrorData`; extensions → `rpcErrorAdditionalFields` |
| `JsonRpcProtocolVersionMismatchErrorDataSchema` | `ProtocolVersionMismatch` | `localFactoryProtocolVersion` → `mismatchLocalVersion`; `peerFactoryProtocolVersion` → `mismatchPeerVersion`; `messageType` → `mismatchMessageType`; `method` → `mismatchMethod`; `requestId` → `mismatchRequestId`; extensions → `mismatchAdditionalFields` |
| `BaseRequestSchema` | `BaseRequest` | `type` = `request`; `id` → `baseRequestId`; `method` → `baseRequestMethod`; `params` → `baseRequestParams`; extensions → `baseRequestAdditionalFields` |
| `BaseNotificationSchema` | `BaseNotification` | `type` = `notification`; `method` → `baseNotificationMethod`; `params` → `baseNotificationParams`; extensions → `baseNotificationAdditionalFields` |
| `BaseResponseSuccessSchema` | `BaseResponseSuccess` | `type` = `response`; `id` → `successResponseId`; `result` → `successResponseResult`; `error` → `successResponseError`; extensions → `successResponseAdditionalFields` |
| `BaseResponseFailureSchema` | `BaseResponseFailure` | `type` = `response`; `id` → `failureResponseId`; `result` → `failureResponseResult`; `error` → `failureResponseError`; extensions → `failureResponseAdditionalFields` |
| `CommandAckSchema` | `CommandAck` | `accepted` = true; extensions → `ackAdditionalFields` |
| `SuccessResultSchema` | `SuccessResult` | `success` → `resultSuccess`; extensions → `resultAdditionalFields` |
| `SuccessOrErrorResultSchema` | `SuccessOrErrorResult` | `success` → `outcomeSuccess`; `error` → `outcomeError`; extensions → `outcomeAdditionalFields` |
| `EmptyObjectSchema` | `EmptyObject` (Aeson `Object`) | No declared fields; arbitrary object members are retained |

`JsonRpcErrorCode` covers the nine supplied numeric codes, including `RpcConflict` (-32006). `JsonRpcMessageType` covers request, response and notification. Error data, base parameters and base results are explicitly unconstrained JSON: omission differs from a present null, scalar, array or object. Specific operation codecs must impose their own stronger parameter/result structures.

Failure IDs are required but nullable, so `failureResponseId = Nothing` emits an explicit null rather than omitting the key. Mismatch diagnostic IDs are both optional and nullable, represented by `Maybe (Maybe Text)`. The schema named `BaseResponseSuccessSchema` permits error and result together or neither; its name is not evidence of success. Base notifications permit open identifier extensions. Wire validation is distinct from operational outcome classification.

The TypeScript base definitions explain these broad shapes (`dist/chunk-5UXINOXG.mjs:2016–2051`). Python `schemas/shared.py:111–185` instead permits only object-or-null parameters, requires an object result on success, forbids request extensions and permits an absent failure ID. Python also lacks the conflict code present in TypeScript and the supplied schema. The Haskell records follow the supplied definitions; no compatibility or attribution policy has been inferred from them.

An acknowledgement requires Boolean true, not a truthy substitute, and does not establish eventual completion. Success flags are actual Booleans: false remains valid, and optional error text is not made conditional on the flag. `EmptyObjectSchema` is open, so it is represented by `Object` rather than unit. TypeScript defines the acknowledgement literal and open common result bodies (`dist/chunk-5UXINOXG.mjs:2071–2073,8201–8202`). Their complete response aliases are implemented below.

## Implemented RPC envelopes and metadata

`Schema/Metadata.hs` implements three further shared definitions; `Schema/RPC.hs` implements eleven envelope/message/response definitions. `test/MetadataSpec.hs` supplies 43 checks and `test/EnvelopeSpec.hs` supplies 81. These are wire codecs, not an attribution or compatibility policy.

| Shared definition | Haskell representation | Verification |
| --- | --- | --- |
| `SdkClientMetadataSchema` | `SdkClientMetadata`, `SdkLanguage`, `SdkVersion` | Both languages, exact ASCII version domain, bounds and extensions |
| `ClientRequestAttributionSchema` | `SdkAttribution` or `NonSdkAttribution` | Required SDK metadata, six non-SDK origins, branch-specific extensions |
| `TraceContextMetaSchema` | `TraceContextMeta` | Optional non-nullable fields, unrestricted strings, nested extensions |
| `JsonRpcEnvelopeSchema` | `WithEnvelope Object` (`JsonRpcEnvelope`) | Required literals, optional version/meta, open fields and collision protection |
| `JsonRpcBaseRequestSchema` | `WithEnvelope BaseRequest` | Exact intersection with existing bare request codec |
| `JsonRpcBaseNotificationSchema` | `WithEnvelope BaseNotification` | Exact intersection, including open identifier extensions |
| `JsonRpcBaseResponseSuccessSchema` | `WithEnvelope BaseResponseSuccess` | String ID, optional raw result/error |
| `JsonRpcBaseResponseFailureSchema` | `WithEnvelope BaseResponseFailure` | Required nullable ID/error, optional raw result |
| `JsonRpcBaseResponseSchema` | `WithEnvelope BaseResponse` | Failure-first full-parser alternatives and overlap preservation |
| `JsonRpcMessageSchema` | `WithEnvelope RpcMessageBody` | Request/generic-response/notification discrimination; generic null ID without error |
| `CommandAckResponseSchema` | `WithEnvelope (RpcResponse CommandAck)` | Required typed result or unrestricted failure; malformed-result fallback |
| `EmptyObjectResponseSchema` | `WithEnvelope (RpcResponse EmptyObject)` | Open-object result or unrestricted failure |
| `SuccessResultResponseSchema` | `WithEnvelope (RpcResponse SuccessResult)` | Boolean result or unrestricted failure |
| `SuccessOrErrorResultResponseSchema` | `WithEnvelope (RpcResponse SuccessOrErrorResult)` | Boolean/optional-error result or unrestricted failure |

`WithEnvelope` maps `factoryProtocolVersion` to `envelopeProtocolVersion`, `_meta` to `envelopeMeta`, and all remaining body fields to `envelopeBody`. It requires and emits `jsonrpc = "2.0"` and the legacy `factoryApiVersion = "1.0.0"`. All four envelope keys are removed before body decoding and reserved when encoding, including omitted options. Extensions therefore have one owner. `RpcObject.toRpcObject` supplies total object-valued serialization, reusing the existing base serializers without inspecting an arbitrary `ToJSON` result. Envelope `Show` output is redacted.

`BaseResponseGeneric` maps `id`, `result`, `error` and extensions to its `genericResponse*` selectors. Its ID is required but nullable, and both payload fields may be absent; this is the wider response branch of `JsonRpcMessageSchema`, not the narrower base-response union. `ResultResponse a` instead requires a string ID and typed result, maps fields to `resultResponse*`, and still permits an accompanying error. The supplied `anyOf` branches overlap: base responses decode failure-first, while typed responses decode result-first and fall back to the complete failure parser. JSON is preserved, but hand-selected overlapping constructors need not retain constructor identity after round-trip.

SDK `language` and `version` map to `sdkLanguage` and `sdkVersion`; extras use `sdkMetadataAdditionalFields`. Versions are one to 64 ASCII letters, digits, dots, pluses or hyphens, without semantic-version normalization. The `client = "sdk"` branch requires `sdk`; other declared clients retain an undeclared `sdk` key as an unrestricted extension. Trace `traceparent`, `tracestate` and `requestAttribution` map to `traceParent`, `traceState` and `traceRequestAttribution`; extensions use `traceAdditionalFields`. Trace strings are not validated as W3C headers. Metadata records redact `Show`, but explicit JSON and fields remain sensitive.

The older TypeScript envelope and body definitions have the same broad structural composition (`dist/chunk-5UXINOXG.mjs:2001–2077`); its trace schema does not declare request attribution. The supplied language enum remains unchanged and no Haskell identity or default omission has been selected.

## Implemented request-correlation channel

`src/Factory/Droid/Protocol.hs` supplies a low-level channel over caller-owned object send/receive actions. `test/ProtocolSpec.hs` covers 26 cases, including a native child that reverses replies while interleaving notifications and a server request. This channel is not the complete SDK client or a permission dispatcher.

| Public operation | Contract and evidence |
| --- | --- |
| `withRpcChannel` | Own one reader; mark terminal state before cancellation/join; preserve callback exception and leave transport ownership with caller |
| `requestReply` | Register string IDs atomically; reject active duplicates; correlate out-of-order replies; retain complete error responses |
| `requestReply` deadline | `Nothing` disables, `Just n` uses microseconds across writer acquisition, sending and waiting; zero sends nothing and negative values fail |
| `sendRpcMessage` | Serialize writes; preserve initiating raw-send exceptions; an interrupted write terminates further channel use |
| `receiveRpcEvent` | Single-consumer delivery of notifications, server requests and null-ID responses; drain pre-terminal events, reject post-terminal delivery |
| `RpcChannelError` | Fixed categories for closed/read/write/malformed/duplicate/timeout failures without embedding frames |
| `requestReplyObserved`, `requestReplyObservedAt` | First accepted replies enqueue ordered STM observations without delaying correlation. The latter supplies a captured local UTC receive time; callback execution does not read a clock in STM. Existing cancellation/duplicate/terminal rules are retained. |
| `decodeRpcResult` | Pure `FromJSON` result validation; remote errors take precedence, including a manually constructed success-shaped response carrying an error |
| `requestResult` | Typed request helper reusing `requestReply`; exchange deadline does not purport to bound arbitrary pure decoder CPU time |
| `RpcResultError` | Explicit `RpcRemoteFailure` details with redacted `Show`; distinct missing-result and invalid-result categories |

Replies lacking both result and error fail the identified request without ending the channel. Error/result coexistence remains schema-valid and is classified as a raw failure. Invalid envelopes and receive failures are terminal; pending and subsequent calls fail rather than waiting indefinitely. Completed replies are not overwritten by EOF. Unknown, late and duplicate non-null-ID responses are ignored. Callers must not reuse settled IDs within a connection. Timeout or cancellation during a write ends the potentially partial stream; cancellation during reply wait only removes that request. The event queue is unbounded and requires a consuming caller; no overflow policy or automatic server-request response is inferred.

The correlation baseline is Python `protocol.py:207–350,430–545` and TypeScript `dist/node.mjs:1645–1772,1916–1947`. Both references permit pending-ID overwrite; the Haskell channel rejects it atomically. Python begins its response timeout after sending; TypeScript starts a timer first but can remain blocked awaiting send. The Haskell deadline covers both phases and terminal receive failure interrupts a blocked send. Source review also found two implementation defects in the new channel—post-poison event delivery and mislabeled terminal errors. Both regressions failed before their fixes; dispatch and terminal-cause selection now share the required state checks. Safe Droid permission defaults, automatic IDs, method-specific exceptions and session attribution remain outstanding.

## Implemented generic callback dispatcher

`src/Factory/Droid/Protocol/Dispatch.hs` consumes the existing channel event queue rather than installing another transport reader. It borrows the channel, owns its dispatcher loop and server-request workers, and accepts explicit response-envelope context. `test/DispatchSpec.hs` supplies 16 checks, including a native duplex exchange. These are generic RPC facilities, not completed Droid-specific interaction bindings.

| Public surface | Observable contract |
| --- | --- |
| `withRpcDispatcher` | Exclusive event intake; seal registrations before concurrently cancelling and joining owned loop/workers; preserve callback failure and leave transport ownership with caller |
| `onRpcEvent` | Raw notifications, server requests and null-ID responses; registration-order snapshot per message |
| `onRpcNotification` | Typed notification-only adapter over the same subscription registry |
| `onRpcError` | Terminal channel-error subscription and immediate replay of an already recorded cause; normal scoped closure is distinct |
| `registerRpcHandler` | Replace a method handler; an old unsubscribe token cannot remove its successor; active requests retain their handler snapshot |
| `RpcRequestHandler` | Complete base request to explicit error or dynamic result JSON; operation adapters remain responsible for stronger parameter/result validation |
| `RpcDispatcherError` | Distinguish scoped closure from terminal channel failure without incorporating payloads |

Unsubscribe actions are idempotent and affect subsequent snapshots, not callbacks or handlers already started. Notification callbacks run serially and should remain brief; they can make outbound requests because response correlation has its own reader. Ordinary callback exceptions are isolated. Request handlers run in separate owned workers; duplicate active IDs are ignored and IDs remain occupied through response sending. Gated publication and token-checked removal prevent fast completion or stale cleanup from corrupting the worker registry. Closing claims each worker once, so simultaneous channel-failure and scope cleanup do not send duplicate cancellation into resource finalizers. Callbacks must support asynchronous cancellation; no absolute completion deadline is promised.

Replies use the original request ID and caller-selected version, metadata and extensions; they do not copy incoming attribution. Reserved response fields take precedence over envelope-context extensions. An unregistered method receives `RpcMethodNotFound`; an ordinary raw-handler exception becomes a fixed `RpcInternalError` without exception text. This generic behavior must not be mistaken for the unfinished Droid permission default. TypeScript's permission/ask-user handler failure emits an error notification and cancellation result, whereas Python sends an internal-error response (`dist/node.mjs:2064–2275`; `protocol.py:590–647`). Dedicated interaction adapters must resolve that difference and validate the current refinements rather than silently inheriting one policy.

The subscription and handler baseline is TypeScript `dist/node.mjs:1645–1712` and Python `protocol.py:358–461`. Scoped handler ownership closes the lifetime gap in the TypeScript rejected-promise wrappers: rejecting a promise is not cancellation of the underlying callback. An idle borrowed channel remains usable after dispatcher scope exit; cancelling an in-progress response write can instead poison it through the channel's existing partial-write rule.

## Implemented typed local operations

The initial `Factory.Droid.Client` checkpoint bound 31 statically defined client-to-Droid operations, with 31 request aliases and fourteen local response aliases (45 local definitions), none transitively runtime-refined. It had 130 operation checks. Settings update and native-tool discovery now extend this to 33 operations and fifteen local response aliases; their field-level fallback policy is documented below. The native fixtures do not launch the real Droid CLI.

`MethodRequest method params` fixes the method at the type level and requires parameters. It maps `id`, `params` and extensions to `methodRequestId`, `methodRequestParams` and `methodRequestAdditionalFields`; `method` is the indexed literal and `type` is `request`. Both type roles are nominal. `eraseMethodRequest` bridges to the existing bare request without reparsing JSON or omitting required parameters. `WithEnvelope` continues to own the outer metadata contract.

`CallOptions` maps the caller's request ID, envelope context and microsecond deadline to `callRequestId`, `callEnvelope` and `callTimeoutMicros`. Context extensions remain at the request root, below reserved typed fields. There is no default identity, automatic ID generation or inferred timeout. The shared executor selects its literal through the named request alias and delegates to `requestResult`; every public operation retains a concrete parameter/result signature.

In the table, each request stem denotes both `<Stem>RequestSchema` in `droid.schema.json` and the corresponding Haskell `<Stem>Request` alias. The wire method is `droid.` followed by the displayed suffix. Parameter and result types refer to the existing codec modules.

| Request stem | Method suffix / client function | Parameters | Result |
| --- | --- | --- | --- |
| `AddUserMessage` | `add_user_message` / `addUserMessage` | `AddUserMessageParams` | `EmptyObject` |
| `AppendMessages` | `append_messages` / `appendMessages` | `AppendMessagesParams` | `EmptyObject` |
| `AuthenticateMcpServer` | `authenticate_mcp_server` / `authenticateMcpServer` | `McpServerNameParams` | `SuccessResult` |
| `CancelMcpAuth` | `cancel_mcp_auth` / `cancelMcpAuth` | `McpServerNameParams` | `SuccessResult` |
| `ChangeWorkingDirectory` | `change_working_directory` / `changeWorkingDirectory` | `ChangeWorkingDirectoryParams` | `ChangeWorkingDirectoryResult` |
| `ClearMcpAuth` | `clear_mcp_auth` / `clearMcpAuth` | `McpServerNameParams` | `SuccessResult` |
| `CloseSession` | `close_session` / `closeSession` | `CloseSessionParams` | `EmptyObject` |
| `CompactSession` | `compact_session` / `compactSession` | `CompactSessionParams` | `CompactSessionResult` |
| `ExecuteRewind` | `execute_rewind` / `executeRewind` | `ExecuteRewindParams` | `ExecuteRewindResult` |
| `ForkSession` | `fork_session` / `forkSession` | `ForkSessionParams` | `ForkSessionResult` |
| `GetContextBreakdown` | `get_context_breakdown` / `getContextBreakdown` | `EmptyObject` | `GetContextBreakdownResult` |
| `GetContextStats` | `get_context_stats` / `getContextStats` | `EmptyObject` | `ContextStats` |
| `GetRewindInfo` | `get_rewind_info` / `getRewindInfo` | `GetRewindInfoParams` | `GetRewindInfoResult` |
| `InterruptSession` | `interrupt_session` / `interruptSession` | `EmptyObject` | `EmptyObject` |
| `KillWorkerSession` | `kill_worker_session` / `killWorkerSession` | `KillWorkerSessionParams` | `EmptyObject` |
| `ListCommands` | `list_commands` / `listCommands` | `EmptyObject` | `ListCommandsResult` |
| `ListMcpRegistry` | `list_mcp_registry` / `listMcpRegistry` | `EmptyObject` | `ListMcpRegistryResult` |
| `ListMcpServers` | `list_mcp_servers` / `listMcpServers` | `EmptyObject` | `ListMcpServersResult` |
| `ListMcpTools` | `list_mcp_tools` / `listMcpTools` | `EmptyObject` | `ListMcpToolsResult` |
| `ListModels` | `list_models` / `listModels` | `ListModelsOptions` | `ListModelsResult` |
| `ListSkills` | `list_skills` / `listSkills` | `EmptyObject` | `ListSkillsResult` |
| `ListTools` | `list_tools` / `listTools` | `ListToolsOptions` | `ListToolsResult` |
| `RemoveMcpServer` | `remove_mcp_server` / `removeMcpServer` | `RemoveMcpServerParams` | `SuccessResult` |
| `RenameSession` | `rename_session` / `renameSession` | `RenameSessionParams` | `SuccessResult` |
| `ResolveQueuedUserMessage` | `resolve_queued_user_message` / `resolveQueuedUserMessage` | `ResolveQueuedMessageParams` | `EmptyObject` |
| `SetSkillDisabled` | `set_skill_disabled` / `setSkillDisabled` | `SetSkillDisabledParams` | `SuccessResult` |
| `SubmitBugReport` | `submit_bug_report` / `submitBugReport` | `SubmitBugReportParams` | `SubmitBugReportResult` |
| `SubmitMcpAuthCode` | `submit_mcp_auth_code` / `submitMcpAuthCode` | `SubmitMcpAuthCodeParams` | `SuccessResult` |
| `SubmitMcpAuthError` | `submit_mcp_auth_error` / `submitMcpAuthError` | `SubmitMcpAuthErrorParams` | `SuccessResult` |
| `ToggleMcpServer` | `toggle_mcp_server` / `toggleMcpServer` | `ToggleMcpServerParams` | `SuccessResult` |
| `ToggleMcpTool` | `toggle_mcp_tool` / `toggleMcpTool` | `ToggleMcpToolParams` | `SuccessResult` |
| `UpdateSessionSettings` | `update_session_settings` / `updateSessionSettings` | `UpdateSessionSettingsParams` | `EmptyObject` |
| `WarmupCache` | `warmup_cache` / `warmupCache` | `EmptyObject` | `EmptyObject` |

The fifteen response aliases use the corresponding request stem followed by `Response` for directory change, compaction, rewind, fork, context breakdown/statistics, rewind information, the seven catalog operations, and bug reports. Each implements the correspondingly named `ResponseSchema` through `WithEnvelope (RpcResponse ResultType)`. Other operations reuse shared `EmptyObjectResponse` or `SuccessResultResponse` contracts. An empty-object response remains open: an `accepted` extension is retained but does not establish turn completion. False Boolean success flags remain unchanged.

These are low-level remote operations, not SDK-side filesystem changes, browser launches, credential exchanges or handle transfers. Reports send only caller-provided content. Minimal local initialization/loading and safe default interaction handlers are implemented in `Factory.Droid`; general configuration and customizable server-to-client interactions remain separate work. Role probes and named-alias verification are recorded in [development verification](development.md).

`Schema.Settings` adds `UpdateSessionSettingsParams` and `ListToolsOptions` for the required request bodies, together with three operation/response aliases in `Schema.Local`. `ToolPolicy` embeds the four tool-ID arrays without normalizing them. Optional-nullable spec fields distinguish omitted/clear/set through nested `Maybe`; compaction retains `Scientific`, and query depth uses nonnegative unbounded `Natural`. Mission settings and session tags reuse their existing validated types. Extensions cannot override declared fields.

| Definition | Haskell type |
| --- | --- |
| `UpdateSessionSettingsRequestParamsSchema` | `UpdateSessionSettingsParams` |
| `ListToolsRequestParamsSchema` | `ListToolsOptions` |
| `UpdateSessionSettingsRequestSchema` | `UpdateSessionSettingsRequest` |
| `ListToolsRequestSchema` | `ListToolsRequest` |
| `ListToolsResponseSchema` | `ListToolsResponse` |
| `SystemPromptConfigSchema` (shared) | `SystemPromptConfig` |
| `SettingsUpdatedNotificationSchema` (local) | `SettingsUpdated` with inline `SettingsChange` |
| `SessionSettingsSchema` (local) | `SessionSettings` |

The selected CLI 1.201.1 declarations (`H9R` near byte 204403610; `XQA` near 204410200) replace invalid `interactionMode` and `autonomyLevel` values with absence. Those two field decoders therefore catch invalid/null payloads; the underlying enum codecs and other optional fields remain strict. `SettingsUpdated` additionally applies whole-field fallback to `availableAutonomyLevels` and retains the actual optional/non-null spec-field contract, unlike mutation patches. These are explicit operational contracts, not exhaustive 1.205.0 runtime refinement claims.

`Schema.SystemPrompt` validates `/\S/` using ECMAScript whitespace rather than Haskell/Python trimming, preserves content and implements the selected CLI's preset normalization. Object tags become `type=preset` and `preset=droid`; unknown keys remain invalid. The canonical output matches the supplied schema. `droidSystemPrompt` is initialization-only because load/settings-update requests do not declare an override; unsupported resume options fail before launch.

`SessionSettings` covers full initialize/load settings, including required model/reasoning, optional policy/prompt/sandbox fields and extensions. `getDroidSettings` folds accepted full replies and applicable notifications in dispatcher receive order. In CLI 0.212.1, the notifier always reflects current spec/mission overrides by presence; absence clears those fields, whereas other fields remain partial. This differs from the older Python explicit-null clearing rule. Invalid enum fallbacks provide no new field observation; malformed non-fallback data invalidates the view until a full load. The [README contract](../README.md#settings-tools-and-skills) records freshness, lifecycle and ambiguous peer-epoch limits. See [verification evidence](development.md#current-local-sdk-delivery).

## Implemented tool records

`src/Factory/Droid/Schema/Tools.hs` implements 19 shared definitions plus five inline enums and the inline diff-coordinate record. `test/ToolsSpec.hs` adds 175 checks. Content and tool record tests share `test/SchemaTest.hs`; its structural checks supplement golden fixtures and do not constitute a complete Draft-07 validator.

| Definition | Haskell type | Wire field → selector |
| --- | --- | --- |
| `ApplyPatchToolInputSchema` | `ApplyPatchToolInput` | `file_path` → `patchInputFilePath`; `patch` → `patchInputPatch` |
| `CreateToolInputSchema` | `CreateToolInput` | `file_path` → `createFilePath`; `content` → `createContent` |
| `EditToolInputSchema` | `EditToolInput` | `file_path` → `editFilePath`; `old_str` → `editOldString`; `new_str` → `editNewString`; `change_all` → `editChangeAll` |
| `ExecuteToolInputSchema` | `ExecuteToolInput` | `command` → `executeCommand`; `summary` → `executeSummary`; `timeout` → `executeTimeout`; `riskLevel` → `executeRiskLevel`; `riskLevelReason` → `executeRiskReason`; `fireAndForget` → `executeFireAndForget` |
| `ReadToolInputSchema` | `ReadToolInput` | `file_path` → `readFilePath`; `offset` → `readOffset`; `limit` → `readLimit`; `image_quality` → `readImageQuality` |
| `GlobToolInputSchema` | `GlobToolInput` | `patterns` → `globPatterns`; `folder` → `globFolder`; `excludePatterns` → `globExcludePatterns` |
| `GrepToolInputSchema` | `GrepToolInput` | `pattern` → `grepPattern`; `path` → `grepPath`; `glob_pattern` → `grepGlobPattern`; `case_insensitive` → `grepCaseInsensitive`; `output_mode` → `grepOutputMode`; `context` → `grepContext`; `context_before` → `grepContextBefore`; `context_after` → `grepContextAfter`; `line_numbers` → `grepLineNumbers`; `head_limit` → `grepHeadLimit` |
| `LSToolInputSchema` | `LSToolInput` | `directory_path` → `listDirectoryPath`; `ignorePatterns` → `listIgnorePatterns` |
| `WebSearchToolInputSchema` | `WebSearchToolInput` | `query` → `searchQuery`; `numResults` → `searchNumResults`; `includeDomains` → `searchIncludeDomains`; `excludeDomains` → `searchExcludeDomains`; `category` → `searchCategory` |
| `FetchUrlToolInputSchema` | `FetchUrlToolInput` | `url` → `fetchUrl` |
| `TaskToolInputSchema` | `TaskToolInput` | `subagent_type` → `taskSubagentType`; `description` → `taskDescription`; `prompt` → `taskPrompt` |
| `TodoWriteToolInputSchema` | `TodoWriteToolInput` | `todos` → `todosText` |
| `ExitSpecModeToolInputSchema` | `ExitSpecModeToolInput` | `plan` → `exitSpecPlan`; `title` → `exitSpecTitle` |
| `SkillToolInputSchema` | `SkillToolInput` | `skill` → `skillName` |
| `ProposeMissionToolInputSchema` | `ProposeMissionToolInput` | `proposal` → `missionProposal`; `title` → `missionTitle` |
| `DiffLineSchema` | `DiffLine` | `type` → `diffLineType`; `content` → `diffLineContent`; `lineNumber` → `diffLineNumbers` |
| `FileOperationResultSchema` | `FileOperationResult` | `success` → `fileResultSuccess`; `diff` → `fileResultDiff`; `diffLines` → `fileResultDiffLines`; `content` → `fileResultContent`; `message` → `fileResultMessage`; `file_path` → `fileResultSnakePath`; `filePath` → `fileResultCamelPath` |
| `ApplyPatchFileChangeSchema` | `ApplyPatchFileChange` | `file_path` → `patchFilePath`; `display_operation` → `patchOperation`; `content` → `patchContent`; `diff` → `patchDiff`; `error` → `patchError`; `moved_to` → `patchMovedTo`; `systemReminder` → `patchSystemReminder` |
| `ApplyPatchToolResultSchema` | `ApplyPatchToolResult` | `success` = true; `files` → `patchResultFiles` |

Every record retains extensions in its corresponding `*AdditionalFields` selector and reserves all declared wire keys. The inline `RiskLevel`, `ImageQuality`, `GrepOutputMode`, `PatchOperation` and `DiffLineType` enums reject undeclared values. `DiffLineNumbers` maps `old` and `new` to `diffOldLine` and `diffNewLine`, with independent optional coordinates and open extensions.

`patchResultFiles` uses `NonEmpty ApplyPatchFileChange`, and the result encoder always emits Boolean true. Empty file lists and truthy substitutes such as 1 or `"true"` are invalid. File-operation outputs preserve both path spellings independently, including when both occur. Empty optional arrays, explicit false and omitted values remain distinct; declared fields are non-nullable. Numeric options use `Scientific` without adding integer or nonnegative restrictions.

These are passive wire records. They do not execute commands, read or modify files, grant permissions, resolve available droids, compile search patterns or contact URLs. Runtime validation and authorization remain separate responsibilities. Descriptive guidance such as the suggested task-description length is not silently converted into a constraint absent from the supplied schema.

The TypeScript tool-input, diff and generic file-result definitions match these structural fields (`dist/chunk-5UXINOXG.mjs:20314–20410`). The two structured apply-patch result definitions come from the newer supplied snapshot. Python exposes related permission-action records rather than these exact tool-schema names; action-level conversion remains part of the interaction-layer work.

## Implemented script results

`src/Factory/Droid/Schema/Script.hs` implements the supplied `ScriptRunResultSchema` and `ScriptRunResultValueSchema`; `test/ScriptSpec.hs` supplies 16 checks. These definitions were not found as named exports in the older SDK baselines. The supplied snapshot governs these codecs, without implying that an older runtime supports script execution.

| Definition | Haskell representation |
| --- | --- |
| `ScriptRunResultSchema` | `ScriptRunResult`, with `ScriptCompletion`, `BackgroundTask` and `InterruptedCall` |
| `ScriptRunResultValueSchema` | Aeson `Value`, the exact recursive JSON domain |

| Status/shape | Constructor | Fields |
| --- | --- | --- |
| `running` | `ScriptRunning` | `runId` |
| `stalled` | `ScriptStalled` | `runId` |
| `completed` with inline data | `ScriptCompleted` + `InlineScriptResult` | `runId`, required `result`, optional `backgroundTasks` |
| `completed` with a file | `ScriptCompleted` + `ScriptResultFile` | `runId`, required `resultPath`, optional `backgroundTasks` |
| `failed` | `ScriptFailed` | `runId`, required `error`, optional `backgroundTasks` |
| `cancelled` | `ScriptCancelled` | `runId`, required `interruptedCalls`, optional `backgroundTasks` |

The run ID is the first constructor argument. `BackgroundTask` maps `taskId` and `description` to `backgroundTaskId` and `backgroundTaskDescription`. `InterruptedCall` maps `toolUseId`, `name`, `input` and `taskId` to the corresponding `interrupted*` selectors. All result, task and call records reject additional fields; only the call's input object and inline result retain arbitrary JSON values.

`NonEmptyText` in `Factory.Droid.Schema.Primitives` enforces the declared minimum string length through a private constructor and `mkNonEmptyText`. Whitespace is preserved. Background-task lists use `Maybe (NonEmpty BackgroundTask)`; absence is valid, but an empty or null list is not. The interrupted-call array is required but may be empty. Completed results contain exactly one of result and resultPath; an inline null result is valid. No result path is read and no script or interrupted call is executed by these codecs.

## Implemented source provenance

`src/Factory/Droid/Schema/Sources.hs` implements session and diagnostic provenance. `test/SourcesSpec.hs` supplies 21 checks, including full/minimal fixtures for all 16 session platforms, required fields, nullable metadata, variant-specific extensions and diagnostic length limits. These codecs describe origins; they do not contact services or assign SDK attribution.

| Definition | Haskell representation |
| --- | --- |
| `SessionSourceSchema` | `SessionSource`, `SessionSourceDetails`, platform-specific payload records and `TeamsConversationType` |
| `BugReportSourceSchema` | `BugReportSource`, `BugReportSurface` and `BugReportRuntime` |

`SessionSource` stores typed details in `sessionSourceDetails` and open fields in `sessionSourceAdditionalFields`. The constructor determines the `platform` literal. Required string fields and nullable metadata map as follows:

| Platform | Constructor | Field mapping |
| --- | --- | --- |
| `slack` | `SourceSlack` | `delegationSessionId` → `slackDelegationSessionId`; `teamId`, `channel`, `threadTs`, `userId`, `automationId` → corresponding `slack*` selectors |
| `web` | `SourceWeb` | Constructor argument is `delegationSessionId` |
| `api` | `SourceApi` | Constructor argument is `delegationSessionId` |
| `sessions_api` | `SourceSessionsApi` | Constructor argument is `delegationSessionId` |
| `jira` | `SourceJira` | `cloudId`, `issueId`, `delegationSessionId`, `issueKey`, `siteId`, `projectId`, `commentId`, `userId`, `taskId` → corresponding `jira*` selectors |
| `linear` | `SourceLinear` | `agentSessionId`, `delegationSessionId`, `issueId`, `issueUrl`, `issueIdentifier`, `organizationId`, `userId` → corresponding `linear*` selectors |
| `microsoft-teams` | `SourceTeams` | `tenantId`, `conversationId`, `serviceUrl`, `delegationSessionId`, `conversationType`, `rootMessageId`, `teamId`, `channelId`, `userId`, `aadObjectId` → corresponding `teams*` selectors |
| `readiness-remediation` | `SourceReadinessRemediation` | Arguments are `reportId`, `repoUrl`, `criterionId` |
| `readiness-evaluation` | `SourceReadinessEvaluation` | Argument is `repoUrl` |
| `automation` | `SourceAutomation` | Arguments are `automationId`, `computerId` |
| `wiki-generation` | `SourceWikiGeneration` | Argument is `repoUrl` |
| `wiki-ci-setup` | `SourceWikiCISetup` | Argument is `repoUrl` |
| `tui` | `SourceTui` | No other declared fields |
| `desktop` | `SourceDesktop` | No other declared fields |
| `acp` | `SourceAcp` | No other declared fields |
| `unknown` | `SourceUnknown` | Explicit literal, not an unknown-platform fallback |

Nullable fields use `Maybe (Maybe a)`, preserving absence and explicit null separately. The Teams conversation enum covers `personal`, `groupChat` and `channel`. Only the selected variant's keys are reserved: for example, an unrelated `cloudId` member on a TUI source is a valid extension and is retained. Conversation identifiers and service URLs are not inferred or normalized by the wire codec.

The 16 source variants and their required fields agree with Python `schemas/client.py:339–471` and the published TypeScript source definitions (`dist/chunk-5UXINOXG.mjs:3320–3489`); the supplied snapshot and Python additionally type Slack `automationId`. Browser-named origins are still wire data within the native target, not browser-runtime implementations.

`BugReportSource` maps `surface`, `runtime`, `version`, `cliVersion`, `platform`, `arch` and `osVersion` to the corresponding `bugReport*` selectors; extensions reside in `bugReportAdditionalFields`. Its optional text fields use `BoundedText 100` or `BoundedText 32`. The type has a private constructor and a nominal limit parameter, so a larger bound cannot be substituted for a smaller one through `coerce`. Empty strings are valid, over-limit strings are rejected, and content is never truncated.

Diagnostic codecs retain JSON Schema's Unicode-character domain ([Draft-07 validation, section 6.3.1](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.3.1)). The pinned CLI and TypeScript Zod validators count UTF-16 units instead ([Zod issue 3355](https://github.com/colinhacks/zod/issues/3355)). `validateBugReportSource` now provides this distinct submission preflight, used by both named local/daemon report methods: 100/32 units are enforced without truncating or changing the canonical codec. Failure is a value-free `BugReportSourceTooLong field limit`. Reference-derived Unicode vectors and a failure-before native regression resolve `hsdk-diagnostic-text-limits-81n`; this does not establish live report interoperability.

## Implemented host records

`src/Factory/Droid/Schema/Host.hs` implements three owned shared definitions and the inline computer registration record. `test/HostSpec.hs` supplies 31 checks. These codecs do not read configuration files, perform migrations, generate identifiers or register computers.

| Definition | Haskell representation | Field mapping |
| --- | --- | --- |
| `HostIdSchema` | `HostId` (`UUIDText`) | Validated hyphenated UUID text |
| `HostConfigSchema` | `HostConfig` | `schemaVersion` = numeric 1; `hostId` → `hostConfigId`; `createdAt` → `hostConfigCreatedAt`; `computerRegistration` → `hostConfigRegistration`; extensions → `hostConfigAdditionalFields` |
| `LegacyComputerConfigSchema` | `LegacyComputerConfig` | `computerId` → `legacyComputerId`; `registeredAt` → `legacyRegisteredAt`; extensions → `legacyComputerAdditionalFields` |

Inline `ComputerRegistration` maps `computerId`, `firestoreOrgId`, `userId` and `registeredAt` to `registrationComputerId`, `registrationFirestoreOrgId`, `registrationUserId` and `registrationTimestamp`; extensions are stored in `registrationAdditionalFields`. Organization and user IDs require nonempty text, not UUID syntax. Timestamps remain exact `Scientific` numbers.

`UUIDText` in `Schema.Primitives` uses the established `uuid-types` parser while retaining the original spelling, including case. Its equality and ordering are textual. Tests cover nil, mixed/uppercase hexadecimal, missing/misplaced hyphens, invalid characters, prefixes, braces and whitespace. The installed parser source and API were inspected at version 1.0.6.1; this is now a direct library dependency already present through Aeson in the development environment. The record shapes agree with the published TypeScript host schemas (`dist/chunk-5UXINOXG.mjs:3300–3318`).

## Implemented core local notifications

`src/Factory/Droid/Schema/Notifications.hs` implements the 18 core definitions listed here from `droid.schema.json`: 16 notification payloads and two named enums. The tool/hook definitions in the next section extend the same module. `test/NotificationsSpec.hs` supplies 135 checks against that document and the already-implemented shared message/content/usage codecs. This is not the complete session-notification union or a transport dispatcher.

| Definition | Haskell type | Field mapping, excluding the literal `type` and extensions |
| --- | --- | --- |
| `AgentTurnCompletionReasonSchema` | `AgentTurnCompletionReason` | All 18 supplied reason literals |
| `DroidWorkingStateSchema` | `DroidWorkingState` | All six supplied working-state literals |
| `AssistantTextDeltaNotificationSchema` | `AssistantTextDelta` | `messageId` → `assistantDeltaMessageId`; `blockIndex` → `assistantDeltaBlockIndex`; `textDelta` → `assistantDeltaText` |
| `AssistantTextCompleteNotificationSchema` | `AssistantTextComplete` | `messageId` → `assistantCompleteMessageId`; `blockIndex` → `assistantCompleteBlockIndex` |
| `ThinkingTextDeltaNotificationSchema` | `ThinkingTextDelta` | `messageId` → `thinkingDeltaMessageId`; `blockIndex` → `thinkingDeltaBlockIndex`; `textDelta` → `thinkingDeltaText` |
| `ThinkingTextCompleteNotificationSchema` | `ThinkingTextComplete` | `messageId` → `thinkingCompleteMessageId`; `blockIndex` → `thinkingCompleteBlockIndex`; `durationMs` → `thinkingCompleteDuration` |
| `CreateMessageNotificationSchema` | `CreateMessage` | `message` → `createdMessage`; `parentId` → `createdParentId`; `requestId` → `createdRequestId` |
| `AgentTurnCompletedNotificationSchema` | `AgentTurnCompleted` | `reason` → `turnCompletionReason`; `tokenUsage` → `turnTokenUsage`; `turnId` → `completedTurnId`; `cumulativeTokenUsage` → `turnCumulativeTokenUsage`; `childTokenUsage` → `turnChildTokenUsage`; `cumulativeChildTokenUsage` → `turnCumulativeChildTokenUsage`; `durationMs` → `turnDurationMs` |
| `SessionTokenUsageChangedNotificationSchema` | `SessionTokenUsageChanged` | `sessionId` → `sessionUsageId`; `tokenUsage` → `sessionUsageTokens`; `inclusiveTokenUsage` → `sessionUsageInclusive`; `lastCallTokenUsage` → `sessionUsageLastCall` |
| `DroidWorkingStateChangedNotificationSchema` | `DroidWorkingStateChanged` | `newState` → `workingStateNewState` |
| `AssistantMessageRetractedNotificationSchema` | `AssistantMessageRetracted` | `messageId` → `retractedMessageId` |
| `SessionTitleUpdatedNotificationSchema` | `SessionTitleUpdated` | `title` → `updatedSessionTitle`; `requestId` → `titleRequestId`; `updateType` → `titleUpdateType` (`TitleUpdateType`) |
| `SessionWorkingDirectoryChangedNotificationSchema` | `SessionWorkingDirectoryChanged` | `cwd` → `updatedWorkingDirectory` |
| `QueuedMessagesDiscardedNotificationSchema` | `QueuedMessagesDiscarded` | `text` → `discardedMessageText`; `requestId` → `discardedRequestId` |
| `StructuredOutputNotificationSchema` | `StructuredOutput` | `messageId` → `structuredMessageId`; `structuredOutput` → `structuredOutputValue` |
| `SessionCompactedNotificationSchema` | `SessionCompacted` | `summaryId` → `compactedSummaryId`; `removedCount` → `compactedRemovedCount`; `visibleBoundaryMessageId` → `compactedVisibleBoundaryId` |
| `ErrorNotificationSchema` | `ErrorNotification` | `message` → `errorNotificationMessage`; `errorType` → `errorNotificationType`; `timestamp` → `errorNotificationTimestamp`; `error` → `errorNotificationError`; `exitCode` → `errorNotificationExitCode` |
| `ChildSessionAvailableNotificationSchema` | `ChildSessionAvailable` | `childSessionId` → `availableChildSessionId`; `timestamp` → `availableChildTimestamp`; `toolUseId` → `availableChildToolUseId`; `subagentType` → `availableChildSubagentType`; `description` → `availableChildDescription` |

Each payload retains extensions in its `*AdditionalFields` selector and reserves every declared field, including omitted options. Inline `NotificationError` maps `name` and `message` to `notificationErrorName` and `notificationErrorMessage`; its extensions remain separate from the enclosing event. Notification error categories are typed data, not SDK IO exceptions.

Structured output and the visible compaction boundary are required but nullable: `Nothing` emits null rather than omitting the key. Structured output accepts only an object or null, without claiming caller-schema validation. Block indices retain the schema's unconstrained number domain; error exit codes are unbounded integers. Nonnegative turn duration and removed-count fields use the validated `NonNegativeNumber` primitive, while thinking completion reuses `ThinkingDuration`.

`AgentTurnCompleted` is separate from `DroidWorkingStateChanged`; decoding an idle state does not imply completion. Turn usage fields and child/cumulative counters remain independent, and wall-clock duration is not summed with child clocks. No callback, session mutation, filesystem change or child-session attachment occurs during decoding.

The published TypeScript payloads establish the corresponding text, usage, working-state, compaction and error shapes (`dist/chunk-5UXINOXG.mjs:2825–2968`), but its shown title and error records lack the newer `updateType` and `exitCode` fields. Python's completion-reason enum lacks `model_rate_limited`, which the supplied snapshot includes. These differences remain explicit rather than relabeling the older protocol baselines.

## Implemented tool and hook notifications

The notification module implements 14 additional local definitions below. `test/ToolNotificationsSpec.hs` adds 114 checks covering wire fields, enum literals, optional values, nested validation and shared serializer composition. None of these data codecs executes tools, hooks or retry actions, or applies a permission decision.

| Definition | Haskell representation | Field mapping |
| --- | --- | --- |
| `ToolConfirmationOutcomeSchema` | `ToolConfirmationOutcome` | All 16 declared selection literals, including cancel |
| `ToolResultNotificationSchema` | `ToolResultNotification` | `messageId` → `resultNotificationMessageId`; existing result fields → `resultNotificationBlock` |
| `ToolCallNotificationSchema` | `ToolCallNotification` | `toolUse` → `calledToolUse`; extensions → `toolCallAdditionalFields` |
| `ToolExecutionHeartbeatNotificationSchema` | `ToolExecutionHeartbeat` | `toolUseId` → `heartbeatToolUseId`; `toolName` → `heartbeatToolName`; extensions → `heartbeatAdditionalFields` |
| `ToolExecutionPhaseChangedNotificationSchema` | `ToolExecutionPhaseChanged` | `toolUseId` → `phaseToolUseId`; `toolName` → `phaseToolName`; `phase` → `changedToolPhase`; extensions → `phaseAdditionalFields` |
| `ToolProgressUpdateSchema` | `ToolProgressUpdate` | `type` → `progressKind`; `toolName`, `status`, `details`, `text`, `error`, `timestamp`, `parameters`, `valueSnippet`, `terminalId`, `fullOutput`, `subagentSessionId` → corresponding `progress*` selectors; extensions → `progressAdditionalFields` |
| `ToolProgressUpdateNotificationSchema` | `ToolProgressUpdateNotification` | `toolUseId` → `progressNotificationToolUseId`; `toolName` → `progressNotificationToolName`; `update` → `progressNotificationUpdate`; extensions → `progressNotificationAdditionalFields` |
| `LlmRetryNotificationSchema` | `LlmRetry` | `attempt` → `retryAttempt`; `reason` → `retryReason`; extensions → `retryAdditionalFields` |
| `PermissionResolvedNotificationSchema` | `PermissionResolved` | `requestId` → `resolvedRequestId`; `toolUseIds` → `resolvedToolUseIds`; `selectedOption` → `resolvedSelection`; extensions → `resolvedAdditionalFields` |
| `DroidHookEventSchema` | `DroidHookEvent` | All nine case-sensitive hook event literals |
| `HookCommandSchema` | `HookCommand` (`PersistedHookCommand`) | Reuses the existing command/timeout codec and extensions |
| `HookResultSchema` | `HookResult` | Existing output fields → `hookResultOutput`; `command` → `hookResultCommand`; `timeout` → `hookResultTimeout` |
| `HookExecutionStartedNotificationSchema` | `HookExecutionStarted` | `hookId` → `startedHookId`; `hookEventName` → `startedHookEventName`; `hookCommands` → `startedHookCommands`; `hookMatcher` → `startedHookMatcher`; `hookToolCallId` → `startedHookToolCallId`; `hiddenFromUserViews` → `startedHookHidden`; `isParallelExecution` → `startedHookParallel`; `parallelGroupId` → `startedHookParallelGroupId`; `hookParentId` → `startedHookParentId`; `hookOrder` → `startedHookOrder`; extensions → `startedHookAdditionalFields` |
| `HookExecutionCompletedNotificationSchema` | `HookExecutionCompleted` | `hookId` → `completedHookId`; `hookStatus` → `completedHookStatus`; `hookEventName` → `completedHookEventName`; `hookMatcher` → `completedHookMatcher`; `hookResults` → `completedHookResults`; `hookToolCallId` → `completedHookToolCallId`; `hiddenFromUserViews` → `completedHookHidden`; `hookParentId` → `completedHookParentId`; `hookOrder` → `completedHookOrder`; `hookPreventedAction` → `completedHookPreventedAction`; extensions → `completedHookAdditionalFields` |

`contentBlockObject` and `persistedHookResultObject` expose the same object encodings used by their existing `ToJSON` instances. The result wrappers add only their own typed fields and reserve those fields against conflicting extensions; they do not duplicate or weaken nested content/output validation. Tool-result arrays retain the text/image/document restriction. Hook-result extensions reside in the persisted output record.

`ToolExecutionPhase`, `ToolProgressKind`, `LlmRetryReason` and `HookCompletionStatus` implement the inline enums. Progress status remains free-form text, while its kind is constrained; parameters remain an arbitrary JSON object. Permission tool IDs preserve order, duplicates and empty arrays. Retry attempts and hook ordering retain the schema's number domain. Unknown retry/settled values are explicit literals, not arbitrary-value fallbacks.

Started hooks require a free-form event name and command array. Completed hooks allow an optional constrained event name and only completed/error statuses. Missing results differ from an empty results array. The source contracts are documented in the published TypeScript definitions (`dist/chunk-5UXINOXG.mjs:2800–2823,2855–2861,3029–3080`) and Python `schemas/cli.py`; Python permits null and broader tool-result content in places where the supplied wire schemas are stricter. Newer hook parent/order/prevented-action fields are retained explicitly.

## Implemented loop snapshots

`src/Factory/Droid/Schema/Loop.hs` implements two local definitions and their inline status/stop enums. `test/LoopSpec.hs` supplies 17 checks. The runtime-refined loop-tool input and scheduling behavior remain separate work.

| Definition | Haskell representation | Field mapping |
| --- | --- | --- |
| `LoopStateSchema` | `LoopState` | `loopId` → `loopStateId`; `status` → `loopStateStatus`; `intervalMs` → `loopStateInterval`; `iteration` → `loopStateIteration`; `startedAt` → `loopStateStartedAt`; `updatedAt` → `loopStateUpdatedAt`; `nextRunAt` → `loopStateNextRunAt`; `isDue` → `loopStateIsDue`; `lastRunStartedAt` → `loopStateLastRunStartedAt`; `lastRunCompletedAt` → `loopStateLastRunCompletedAt`; `stopReason` → `loopStateStopReason`; extensions → `loopStateAdditionalFields` |
| `LoopStateChangedNotificationSchema` | `LoopStateChanged` | `type` = `loop_state_changed`; `loopState` → `changedLoopState`; extensions → `changedLoopAdditionalFields` |

`LoopInterval` has a private constructor and inclusive integer bounds of 5,000–86,400,000 milliseconds. Counters and timestamps use unbounded `Natural` values; negative and fractional JSON values fail decoding. `nextRunAt` is required but nullable. The codec does not infer consistency rules between status, timestamps and `isDue`, or operate a scheduler. The TypeScript state shape agrees (`dist/chunk-5UXINOXG.mjs:2564–2581`).

## Implemented session-control bodies

`src/Factory/Droid/Schema/Control.hs` implements 24 local definitions and their inline types. `test/ControlSpec.hs` supplies 199 checks, including a structural comparison proving that appended messages reuse the shared message schema with only the declared visibility restriction. These are parameter/result bodies, not RPC envelopes or executable operations.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `OutputFormatSchema` | `OutputFormat` | `type` = `json_schema`; `schema` → `outputFormatSchema` |
| `AddUserMessageRequestParamsSchema` | `AddUserMessageParams` | `text`, `messageId`, `content`, `images`, `imagePaths`, `files`, `outputFormat`, `skipAgentLoop`, `queuePlacement`, `role`, `visibility`, `userMessageSource` → corresponding `userMessage*` selectors; `messageId` uses `userMessageId` |
| `AppendMessagesRequestParamsSchema` | `AppendMessagesParams` | `messages` → `appendMessages` |
| `QueuePlacementSchema` | `QueuePlacement` | `end_of_turn`, `end_of_loop` |
| `ResolveQueuedUserMessageRequestParamsSchema` | `ResolveQueuedMessageParams` | `requestId` → `queueRequestId`; `action` and optional `queuePlacement` → `queueResolution` |
| `RewindFileSnapshotSchema` | `RewindFileSnapshot` | `filePath` → `rewindFilePath`; `contentHash` → `rewindContentHash`; `size` → `rewindFileSize` |
| `RewindFileCreationSchema` | `RewindFileCreation` | `filePath` → `rewindCreatedFilePath` |
| `RewindEvictedFileSchema` | `RewindEvictedFile` | `filePath` → `rewindEvictedFilePath`; `reason` → `rewindEvictionReason` |
| `GetRewindInfoRequestParamsSchema` | `GetRewindInfoParams` | `sessionId` → `rewindInfoSessionId`; `messageId` → `rewindInfoMessageId` |
| `GetRewindInfoResultSchema` | `GetRewindInfoResult` | `availableFiles` → `rewindAvailableFiles`; `createdFiles` → `rewindCreatedFiles`; `evictedFiles` → `rewindEvictedFiles` |
| `ExecuteRewindRequestParamsSchema` | `ExecuteRewindParams` | `sessionId` → `executeRewindSessionId`; `messageId` → `executeRewindMessageId`; `filesToRestore` → `executeRewindRestore`; `filesToDelete` → `executeRewindDelete`; `forkTitle` → `executeRewindTitle` |
| `ExecuteRewindResultSchema` | `ExecuteRewindResult` | `newSessionId` → `rewindNewSessionId`; `restoredCount` → `rewindRestoredCount`; `deletedCount` → `rewindDeletedCount`; `failedRestoreCount` → `rewindFailedRestoreCount`; `failedDeleteCount` → `rewindFailedDeleteCount` |
| `CompactSessionRequestParamsSchema` | `CompactSessionParams` | `customInstructions` → `compactionInstructions` |
| `CompactSessionResultSchema` | `CompactSessionResult` | `newSessionId` → `compactionNewSessionId`; `removedCount` → `compactionRemovedCount` |
| `ForkSessionRequestParamsSchema` | `ForkSessionParams` | `title` → `forkSessionTitle`; `tags` → `forkSessionTags` |
| `ForkSessionResultSchema` | `ForkSessionResult` | `newSessionId` → `forkedSessionId` |
| `RenameSessionRequestParamsSchema` | `RenameSessionParams` | `title` → `renameTitle` |
| `ChangeWorkingDirectoryRequestParamsSchema` | `ChangeWorkingDirectoryParams` | `workingDirectory` → `requestedWorkingDirectory` |
| `ChangeWorkingDirectoryResultSchema` | `ChangeWorkingDirectoryResult` | `resolvedPath` → `changedResolvedPath` |
| `ValidateWorkingDirectoryResultSchema` | `ValidateWorkingDirectoryResult` | `isValid` → `directoryIsValid`; `resolvedPath` → `directoryResolvedPath`; `error` → `directoryValidationError` |
| `CloseSessionRequestParamsSchema` | `CloseSessionParams` | `reason` → `closeSessionReason` |
| `KillWorkerSessionRequestParamsSchema` | `KillWorkerSessionParams` | `workerSessionId` → `killedWorkerSessionId` |
| `SubmitBugReportRequestParamsSchema` | `SubmitBugReportParams` | `userComment` → `bugReportUserComment`; `clientLogs` → `bugReportClientLogs`; `source` → `bugReportSource` |
| `SubmitBugReportResultSchema` | `SubmitBugReportResult` | `bugReportId` → `submittedBugReportId` |

Every open body retains extensions in its corresponding additional-fields selector. Queue updates require a placement; deletion reserves only its own fields, so a placement member on that branch remains an extension. `AppendMessagesParams` uses `NonEmpty UserOnlyMessage`; `mkUserOnlyMessage` validates an explicit `VisibilityUserOnly` without rewriting any other field. The wrapper's private constructor prevents other visibility values from entering this payload.

User-message text is required but may be empty. Optional `content` uses `NonEmpty UserMessageContent`, restricted to text/image blocks; image sources and document sources reuse existing codecs. Other attachment arrays may be empty. Simultaneously supplied text, content and attachments remain distinct, ordered data without inferred precedence. The older TypeScript input schema lacks the newer ordered `content` field (`dist/chunk-5UXINOXG.mjs:3677–3689`). Source strings are not base64-decoded, and paths are not accessed.

Fork tags intentionally use a separate `ForkSessionTag` with `forkTagName :: Text`, optional string-valued `forkTagMetadata`, and `forkTagAdditionalFields`: the inline fork schema permits empty names, unlike `SessionTagSchema`. This matches the older TypeScript fork definition (`dist/chunk-5UXINOXG.mjs:4262–4269`); the difference is not silently removed by reusing the stricter shared tag type.

Rewind lists are required but may be empty. Rewind sizes/counts and the compaction result's removed count remain unconstrained JSON numbers; the latter is distinct from the nonnegative count in a compaction notification. Result identifiers do not perform replacement or ownership transfer. Paths are not accessed, worker sessions are not terminated and caller-provided logs are not uploaded by these codecs. The older TypeScript bodies provide the corresponding rewind/compaction/fork contracts (`dist/chunk-5UXINOXG.mjs:4199–4270`); append-message constraints are taken from the supplied snapshot.

## Implemented interaction bodies

`src/Factory/Droid/Schema/Interaction.hs` implements eight local definitions and their inline types. `test/InteractionSpec.hs` supplies 185 checks, including full/minimal fixtures for all eleven detail alternatives and a structural equality check for the inline/named question schemas. Permission-result refinements, handlers and dispatch remain separate work.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `AskUserQuestionSchema` | `AskUserQuestion` | `index` → `questionIndex`; `topic` → `questionTopic`; `question` → `questionText`; `options` → `questionOptions`; `multiSelect` → `questionMultiSelect` |
| `AskUserCollectedAnswerSchema` | `AskUserCollectedAnswer` | `index` → `answerIndex`; `question` → `answerQuestion`; `answer` → `answerText` |
| `AskUserRequestParamsSchema` | `AskUserParams` | `toolCallId` → `askUserToolCallId`; `questions` → `askUserQuestions` |
| `AskUserResultSchema` | `AskUserResult` | `answers` → `askUserAnswers`; `cancelled` → `askUserCancelled` |
| `ToolConfirmationListItemSchema` | `ToolConfirmationListItem` | `label` → `confirmationOptionLabel`; `value` → `confirmationOptionValue`, reusing `ToolConfirmationOutcome` |
| `ToolConfirmationDetailsSchema` | `ToolConfirmationDetails` | `type` and branch fields → `confirmationDetails :: ConfirmationDetails`; extensions → `confirmationAdditionalFields` |
| `ToolConfirmationInfoSchema` | `ToolConfirmationInfo` | `toolUse` → `confirmationInfoToolUse`, reusing `ToolUseBlock`; `confirmationType` → `confirmationInfoType`; `details` → `confirmationInfoDetails` |
| `RequestPermissionRequestParamsSchema` | `RequestPermissionParams` | `toolUses` → `permissionToolUses`; `options` → `permissionOptions`; `associatedSessionIds` → `permissionAssociatedSessionIds` |

The detail union preserves the complete branch-specific structure. Only the selected branch's fields are reserved against extensions.

| Discriminator | Constructor | Fields |
| --- | --- | --- |
| `edit` | `ConfirmationEdit` | `filePath`, `fileName`, optional `oldContent` and `newContent` |
| `exec` | `ConfirmationExec` | `fullCommand`, `command`, optional `extractedCommands`, `impactLevel` and `riskLevelReason` |
| `create` | `ConfirmationCreate` | `filePath`, `fileName`, `content` |
| `ask_user` | `ConfirmationAskUser` | `questionnaire`, optional `parsed :: ParsedQuestionnaire` and `parseError :: QuestionnaireParseError` |
| `exit_spec_mode` | `ConfirmationExitSpecMode` | `plan`, optional `title` |
| `propose_mission` | `ConfirmationProposeMission` | `proposal`, optional `title` |
| `start_mission_run` | `ConfirmationStartMissionRun` | `runningMissionCount`, `runningMissionSessionIds` |
| `apply_patch` | `ConfirmationApplyPatch` | `filePath`, `fileName`, `patchContent`, optional `oldContent`, `newContent` and `files :: [ConfirmationPatchFile]` |
| `mcp_tool` | `ConfirmationMcpTool` | `toolName`, `impactLevel`, optional `serverName` and `actualToolName` |
| `sandbox_violation` | `ConfirmationSandboxViolation` | `violatingToolName`, `target`, typed `operationType` and `violationType`, `reason`, optional typed `violationReason`, `isOrgDeny` |
| `droid_shield_violation` | `ConfirmationDroidShieldViolation` | `command`, `reason` |

Each ordinary record retains its own additional-fields object. `ParsedQuestionnaire` uses `parsedQuestions` and `parsedQuestionnaireAdditionalFields`; `QuestionnaireParseError` maps message/line/extensions to its `questionnaireError*` selectors. `ConfirmationPatchFile` has `confirmationPatchPath`, `confirmationPatchName`, `confirmationPatchOperation` (the existing `PatchOperation`), optional move/old/new content and `confirmationPatchAdditionalFields`. Parsed questionnaires and parse errors may coexist. No questionnaire parser or patch executor is implied.

Question indices, parse-error lines and running-mission counts retain the declared JSON-number domain rather than adopting Python's integer fields. Empty/duplicate lists and cancellation with collected answers remain valid data. The outer `confirmationType` and inner detail discriminator are independently constrained by the schema; codecs do not silently reconcile mismatches or establish permission-policy consistency. Organization-denial flags and violation categories are retained without applying or overriding policy.

The baselined TypeScript definitions (`dist/chunk-5UXINOXG.mjs:3127–3266`) and Python `schemas/cli.py:882–1275` establish the corresponding older surface. Python permits null on optional fields and forbids extras in answer/result records where the supplied snapshot is open and non-nullable. Its `exit_spec_mode.optionNames` is not a declared field in this snapshot and survives only as an extension; its typed compatibility mapping remains open. The older TypeScript patch confirmation lacks the newer typed `files` list. These differences are not evidence of completed SDK interoperability.

## Implemented context snapshots

`src/Factory/Droid/Schema/Context.hs` implements three local definitions and three inline entry shapes. `test/ContextSpec.hs` supplies 57 checks, including structural equality of skill/droid entry schemas except for their location domains.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `ContextStatsSchema` | `ContextStats` | `used`, `remaining`, `limit`, `accuracy`, `updatedAt` → corresponding `context*` selectors; extensions → `contextStatsAdditionalFields` |
| `ContextBreakdownCategorySchema` | `ContextBreakdownCategory` | `name` → `contextCategoryName`; `tokens` → `contextCategoryTokens`; `colorKey` → `contextCategoryColor`; extensions → `contextCategoryAdditionalFields` |
| `GetContextBreakdownResultSchema` | `GetContextBreakdownResult` | `modelId`, `modelDisplayName`, `contextBudget`, `usedTokens`, `freeTokens`, `categories`, `skills`, `mcpServers`, `droids`, `lastCallCompactionTokens` → corresponding `breakdown*` selectors; extensions → `breakdownAdditionalFields` |

`LocatedContextEntry location` shares the name/location/tokens codec between `ContextBreakdownSkillEntry` and `ContextBreakdownDroidEntry`; fields use `contextEntry*` selectors. Skills use the shared `SkillLocation`, whereas `DroidLocation` accepts only project/personal. MCP entries use `contextMcpName`, `contextMcpToolCount`, `contextMcpTokens` and `contextMcpAdditionalFields`. `ContextAccuracy` and `ContextCategoryColorKey` preserve the two accuracy labels and eight case-sensitive color-key labels.

All counts are exact `Scientific` values, including fractional or negative values admitted by the schema. The codec does not infer sums, clamp remaining capacity or parse the timestamp string. Required lists may be empty; an omitted compaction count differs from zero. These are snapshots, not context measurement or compaction operations. The TypeScript shapes agree (`dist/chunk-5UXINOXG.mjs:3508–3514,4148–4180`); Python uses integer counts and broader string-valued color/location fields (`schemas/client.py:2189–2306`).

## Implemented discovery metadata

`src/Factory/Droid/Schema/Discovery.hs` implements ten local definitions, the inline command-list result and skill-disabling records. `test/DiscoverySpec.hs` supplies 114 checks. No filesystem discovery, command resolution, authentication or settings mutation occurs in these codecs.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `CustomCommandInfoSchema` | `CustomCommandInfo` | `name`, `description`, `argumentHint`, `isExecutable` → corresponding `customCommand*` selectors |
| `ExecToolInfoSchema` | `ExecToolInfo` | `id`, `llmId`, `displayName`, `description`, `category`, `defaultAllowed`, `currentlyAllowed` → corresponding `execTool*` selectors |
| `ListToolsResultSchema` | `ListToolsResult` | `tools` → `listedTools` |
| `SkillResourceSchema` | `SkillResource` | `name`, `path`, `type` → corresponding `skillResource*` selectors |
| `SkillInfoSchema` | `SkillInfo` | `name`, `location`, `filePath`, `description`, `enabled`, `userInvocable`, `version`, `content`, `resources`, `disabledBy` → corresponding `skill*` selectors |
| `ListSkillsResultSchema` | `ListSkillsResult` | `skills` → `listedSkills`; `projectAvailable` → `listedSkillsProjectAvailable` |
| `EditableSkillSettingsLevelSchema` | `EditableSkillSettingsLevel` | `user`, `project` |
| `SetSkillDisabledRequestParamsSchema` | `SetSkillDisabledParams` | `skillName` → `changedSkillName`; `disabled` → `changedSkillDisabled`; `settingsLevel` → `changedSkillSettingsLevel` |
| `GetUserInfoResultSchema` | `GetUserInfoResult` | `userId` → `reportedUserId`; `orgId` → `reportedOrgId` |
| `GitRepoInfoSchema` | `GitRepoInfo` | `repoName` → `gitRepoName`; `owner` → `gitRepoOwner` |

Each record retains its corresponding additional-fields object. `ListCommandsResult` models the inline result at `ListCommandsResponseSchema/anyOf/0/allOf/1/properties/result`: `commands` maps to `listedCommands`, and extensions to `listedCommandsAdditionalFields`. `Schema.Local.ListCommandsResponse` and `Client.listCommands` now supply its complete envelope and operation binding.

`SkillDisabledByLedger` carries a source list, while `SkillDisabledByFrontmatter` does not declare one. Their common `skillDisabledAdditionalFields` reserves only the selected branch's keys. Each `SkillDisabledSource` contains `disabledSourceLevel`, optional `disabledSourceFolderPath` and `disabledSourceAdditionalFields`. Source levels use the complete shared `SettingsLevel`; editable levels deliberately use only user/project. Neither a folder source without a path nor simultaneous enabled/disabledBy reports acquire invented cross-field constraints.

The native-tool category and skill-resource type are closed enums, not arbitrary strings. Tool identifiers, reported allow states, names, paths and resource classifications are not derived from one another. Empty catalogs, duplicate entries and explicit false remain distinct from absent fields. TypeScript supplies the corresponding contracts (`dist/chunk-5UXINOXG.mjs:4019–4044,4070–4138`). Python makes several tool-description fields optional, leaves category unconstrained and uses an untyped disabledBy object (`schemas/client.py:818–870,2080–2140`); canonical wire codecs retain the supplied stricter structures. Typed legacy input compatibility remains separate work.

The shared enum fixture helper now lives in `test/SchemaTest.hs`, reused by context, discovery and interaction tests without changing the earlier interaction checks.

## Implemented MCP wire bodies

`src/Factory/Droid/Schema/MCP.hs` implements 24 local definitions and their inline types. `test/MCPSpec.hs` supplies 214 checks, including schema-shape equality between server listings and status notifications, branch/field preservation and redacted `Show` output for every record. Typed local management/discovery request envelopes and operation bindings are covered above. OAuth-refined configuration, add-server binding, local credential exchange and SDK-owned MCP servers remain unimplemented.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `McpServerNameSchema` | `McpServerName` (`Text`) | Unconstrained string; empty/whitespace names remain unchanged |
| `McpServerTypeSchema` | `McpServerType` | `stdio`, `http`, `sse` |
| `McpServerStatusSchema` | `McpServerStatus` | `connecting`, `connected`, `disconnected`, `failed`, `disabled` |
| `McpSettingsLevelSchema` | `McpSettingsLevel` | The sole literal `user` |
| `McpHttpServerConfigFieldsSchema` | `McpHttpServerConfigFields` | Optional `url` → `mcpConfigUrl` |
| `McpStdioServerConfigFieldsSchema` | `McpStdioServerConfigFields` | Optional `command` → `mcpConfigCommand`; `args` → `mcpConfigArgs` |
| `StdioMcpSchema` | `StdioMcp` | `name`, `command`, optional `args` and `env` → corresponding `stdioMcp*` selectors |
| `HttpHeaderSchema` | `HttpHeader` | `name` → `httpHeaderName`; `value` → `httpHeaderValue` |
| `McpRegistryServerSchema` | `McpRegistryServer` | `name`, `description`, `type`, optional `url`, `command`, `args`, `note`, `logoUrl` → corresponding `registryServer*` selectors |
| `ListMcpRegistryResultSchema` | `ListMcpRegistryResult` | `servers` → `mcpRegistryServers` |
| `McpToolInfoSchema` | `McpToolInfo` | `serverName`, `name`, `description`, `inputSchema` → corresponding `mcpTool*` selectors; `isEnabled` → `mcpToolEnabled`; `isReadOnly` → `mcpToolReadOnly` |
| `ListMcpToolsResultSchema` | `ListMcpToolsResult` | `tools` → `listedMcpTools` |
| `McpStatusSummarySchema` | `McpStatusSummary` | `total`, `connected`, `connecting`, `failed`, optional `disabled`, `configError` → corresponding `mcp*` selectors |
| `McpServerStatusInfoSchema` | `McpServerStatusInfo` | `status` → `mcpStatus`; `isManaged` → `mcpStatusManaged`; `name`, `source`, `serverType`, `error`, `toolCount`, `hasAuthTokens`, `requiresAuth`, `pendingAuthUrl`, `pendingAuthMessage`, `pendingAuthState`, `blockedByPolicy` → corresponding `mcpStatus*` selectors |
| `ListMcpServersResultSchema` | `ListMcpServersResult` | `servers` → `listedMcpServers`; `summary` → `listedMcpSummary` |
| `McpStatusChangedNotificationSchema` | `McpStatusChanged` | `type` = `mcp_status_changed`; remaining fields reuse `changedMcpStatus :: ListMcpServersResult` |
| `McpAuthRequiredNotificationSchema` | `McpAuthRequired` | `type` = `mcp_auth_required`; `serverName` → `mcpAuthRequiredServer`; `authUrl` → `mcpAuthUrl`; `message` → `mcpAuthRequiredMessage`; `state` → `mcpAuthState` |
| `McpAuthCompletedNotificationSchema` | `McpAuthCompleted` | `type` = `mcp_auth_completed`; `serverName` → `mcpAuthCompletedServer`; `outcome` → `mcpAuthOutcome`; `message` → `mcpAuthCompletedMessage` |
| `McpServerNameParamsSchema` | `McpServerNameParams` | `serverName` → `requestedMcpServerName` |
| `RemoveMcpServerRequestParamsSchema` | `RemoveMcpServerParams` | `serverName` → `removedMcpServerName`; required `settingsLevel` is intrinsically `user` |
| `ToggleMcpServerRequestParamsSchema` | `ToggleMcpServerParams` | `serverName` → `toggledMcpServerName`; `enabled` → `toggledMcpServerEnabled`; required `settingsLevel` is intrinsically `user` |
| `ToggleMcpToolRequestParamsSchema` | `ToggleMcpToolParams` | `serverName` → `toggledMcpToolServer`; `toolName` → `toggledMcpToolName`; `enabled` → `toggledMcpToolEnabled` |
| `SubmitMcpAuthCodeRequestParamsSchema` | `SubmitMcpAuthCodeParams` | `serverName` → `submittedMcpCodeServer`; `code` → `submittedMcpCode`; `state` → `submittedMcpCodeState` |
| `SubmitMcpAuthErrorRequestParamsSchema` | `SubmitMcpAuthErrorParams` | `serverName` → `submittedMcpErrorServer`; `error` → `submittedMcpError`; `state` → `submittedMcpErrorState`; `errorDescription` → `submittedMcpErrorDescription` |

Every open record retains its additional-fields object. `McpToolInputSchema` implements the inline input-schema subset with optional `mcpInputType`, object-valued `mcpInputProperties`, string-valued `mcpInputRequired` lists and extensions; property values remain arbitrary JSON. It is not a complete JSON Schema validator. `McpConfigError` supplies typed path/message/extensions in summaries. `McpAuthOutcome` has success/cancelled/failed alternatives without retrieving or storing tokens.

Server listings and status notifications share their object encoder; the notification reserves its own type literal without discarding other listing extensions. Counts remain `Scientific` and have no inferred sum constraints. Status source levels use the complete shared `SettingsLevel`, independently of the user-only mutation-body level. Auth flags, policy blocks and connection status remain independent reports.

Standalone stdio records do not declare a type discriminator. Optional args/env preserve absent versus explicitly empty values; their JSON Schema defaults are annotations, not codec normalization. TypeScript materializes these defaults, while Python also trims/rejects blank names/commands and forbids extras (TypeScript `dist/chunk-5UXINOXG.mjs:3515–3520`; Python `schemas/client.py:484–508`). These input-model differences remain separate compatibility work.

Header records are JSON wire data, not ready-to-use HTTP headers. Python rejects invalid names and CR/LF values (`schemas/client.py:510–533`), whereas the supplied schema constrains only their string types. The wire codec performs no HTTP operation; an operational header boundary must validate syntax before use. Registry URLs and authentication URLs likewise remain unparsed strings here. HTTP/SSE session configurations still depend on the unresolved OAuth contract and are not represented by the partial registry HTTP fields.

MCP record `Show` instances emit only the type name and a redaction marker, including arbitrary extensions; explicit fields/JSON remain sensitive and are not a safe logging substitute. Use the explicit observability controls above. No credential helper, server command, callback URL or authentication exchange is executed by these codecs. Older metadata/auth contracts are in TypeScript `dist/chunk-5UXINOXG.mjs:2324–2380,2993–3005,3947–3994` and Python `schemas/mcp.py:91–248`; the supplied newer `blockedByPolicy` field remains explicit.

## Implemented mission features and handoff reports

`src/Factory/Droid/Schema/Mission.hs` implements 22 local definitions and their inline enums. `test/MissionSpec.hs` supplies 164 checks, including all absent/null/present combinations of the two legacy worker identifiers. The module represents reports only: neither their text nor their status labels establish that work was performed or verified.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `DecompSessionTypeSchema` | `DecompSessionType` | `orchestrator`, `worker` |
| `MissionStateEnumSchema` | `MissionPhase` | All seven declared mission phase labels |
| `FeatureSuccessStateSchema` | `FeatureSuccessState` | `success`, `partial`, `failure` |
| `MissionFeatureSchema` | `MissionFeature` | `id`, `description`, `status`, `skillName`, `preconditions`, `expectedBehavior`, `fulfills`, `milestone`, `workerSessionIds`, `currentWorkerSessionId`, `completedWorkerSessionId` → corresponding `missionFeature*` selectors |
| `DiscoveredIssueSchema` | `DiscoveredIssue` | `severity`, `description`, `suggestedFix` → corresponding `discoveredIssue*` selectors |
| `VerificationCommandSchema` | `VerificationCommand` | `command` → `verificationCommand`; `exitCode` → `verificationExitCode`; `observation` → `verificationObservation` |
| `InteractiveCheckSchema` | `InteractiveCheck` | `action` → `interactiveCheckAction`; `observed` → `interactiveCheckObserved` |
| `VerificationSchema` | `Verification` | `commandsRun` → `verificationCommandsRun`; `interactiveChecks` → `verificationInteractiveChecks` |
| `TestCaseSchema` | `HandoffTestCase` | `name` → `handoffTestName`; `verifies` → `handoffTestVerifies` |
| `TestFileSchema` | `HandoffTestFile` | `file` → `handoffTestFile`; `cases` → `handoffTestCases` |
| `TestsSchema` | `HandoffTests` | `added`, `coverage`, `updated` → corresponding `handoffTests*` selectors |
| `SkillDeviationSchema` | `SkillDeviation` | `step` → `skillDeviationStep`; `whatIDidInstead` → `skillDeviationInstead`; `why` → `skillDeviationWhy` |
| `SkillFeedbackSchema` | `SkillFeedback` | `followedProcedure` → `skillFollowedProcedure`; `deviations` → `skillDeviations`; `suggestedChanges` → `skillSuggestedChanges` |
| `HandoffSchema` | `Handoff` | `whatWasImplemented` → `handoffImplemented`; `whatWasLeftUndone` → `handoffLeftUndone`; `verification`, `tests`, `discoveredIssues`, `salientSummary`, `skillFeedback` → corresponding `handoff*` selectors |
| `DismissalRecordSchema` | `DismissalRecord` | `type`, `sourceFeatureId`, `summary`, `justification` → corresponding `dismissal*` selectors |
| `WorkerStateInfoSchema` | `WorkerStateInfo` | `startedAt` → `workerStartedAt`; `completedAt` → `workerCompletedAt`; `exitCode` → `workerExitCode` |
| `SubagentInvocationSummarySchema` | `SubagentInvocationSummary` | `childSessionId`, `status`, `subagentType`, `description`, `toolUseCount`, `durationMs` → corresponding `invocation*` selectors |
| `MissionStateChangedNotificationSchema` | `MissionStateChanged` | `type` = `mission_state_changed`; `state` → `changedMissionPhase`; `updatedAt` → `changedMissionUpdatedAt` |
| `MissionFeaturesChangedNotificationSchema` | `MissionFeaturesChanged` | `type` = `mission_features_changed`; `features` → `changedMissionFeatures` |
| `MissionHeartbeatNotificationSchema` | `MissionHeartbeat` | `type` = `mission_heartbeat`; `timestamp` → `missionHeartbeatTimestamp` |
| `MissionWorkerStartedNotificationSchema` | `MissionWorkerStarted` | `type` = `mission_worker_started`; `workerSessionId` → `startedMissionWorkerId` |
| `MissionWorkerCompletedNotificationSchema` | `MissionWorkerCompleted` | `type` = `mission_worker_completed`; `workerSessionId` → `completedMissionWorkerId`; `exitCode` → `completedMissionWorkerExitCode` |

Each record retains its additional-fields object. Feature lifecycle, success assessment and subagent status use distinct enums; issue severity and dismissal type are constrained independently. Legacy nullable worker identifiers use `Maybe (Maybe Text)`, without collapsing absent and explicit null values. Arrays retain order, duplicates and empty lists. Counts/durations/exit codes retain the schema's number domain, and timestamps remain unparsed strings.

A handoff contains typed verification, test, issue and feedback reports. No command is executed, path accessed, issue dismissed or state transition applied. The older TypeScript contracts are in `dist/chunk-5UXINOXG.mjs:2391–2470,2967–2992,3760–3784`; Python uses integer exit codes and more permissive optional nulls. Python also requires `MissionFeature.verificationSteps` (`schemas/mission.py:54–105`), absent from both the supplied declared fields and the older TypeScript feature schema. It survives as an extension in the canonical wire type; its typed compatibility mapping remains open.

The eleven-branch `ProgressLogEntry` union and `MissionProgressEntry` notification are implemented. Worker completion uses the reference's field-local preprocessing: all-ECMAScript-whitespace `commitId`/`repoPath` strings disappear; nonblank strings retain their spelling; null/nonstrings reject. Newer supplied-schema pause/failure reasons and worker model/substitution fields are retained explicitly. The simpler independent `MissionWorkerCompleted` notification remains distinct. Typed snapshots, immutable transformations and owned read-only views are implemented; multi-session cache/association behavior remains unfinished.

All six mission notification families now enter the shared `DroidEvent` decoder, for local/daemon `AllEvents` and outside-turn session observers. Malformed selected payloads are errors rather than raw extensions; foreign-session payloads are filtered first. Complete-message callbacks/result-event logs continue to exclude this metadata. Tests cover all eleven progress variants, required/null fields, normalization, exact numbers/order, native local/daemon delivery, callback queries/unsubscribe and malformed input. These are protocol observations, not proof of mission execution or handoff correctness.

References: TS 0.7.0 `chunk-5UXINOXG.mjs:1745–1767,2392–2395,2471–2556,2967–2992,14799–14877`; supplied progress/mission schemas. The published mission resource offers readiness operations only (`index-D_SzTnFR.d.ts:106387–106391`); no separate start/stop mission RPC was invented.

`MissionStateSchema` now maps to `MissionSnapshot`, preserving all declared fields and extensions without recomputing reports. `LoadedSessionState.loadedMissionSnapshot` exposes validated daemon load data. `Factory.Droid.Mission` supplies an immutable counterpart to the reference store's transformations: presence-aware setters/merges, ordered worker registration, progress-derived history, explicit-time worker updates, per-session usage replacement/aggregation, load restoration and typed event application. Pristine source fields never erase a populated destination, but explicit null title/empty lists/default state do. Source maps win collisions without deleting unrelated destination entries.

Snapshot restoration follows the controller's sequence and derives aggregate usage from the per-session map; aggregate-only input and other metadata remain available in the receipt. Native maps avoid prototype-property behavior, and derivation uses typed worker variants rather than extension fields. Explicit observation timestamps replace hidden clock reads. This is an application-owned pure model using existing event subscriptions, not an automatic live cache or a new callback runtime.

References: TS 0.7.0 `chunk-5UXINOXG.mjs:3765–3776,9418–9436,16521–16540,17814–17973`. Extracted reference traces and native tests cover defaults/clears, history, collisions, usage and key ordering; the numeric-worker ordering test failed before correction. Native daemon load and observer cases exercise the model. Automatic multi-mission association and controller management remain required.

`getDroidMissionSnapshot` / `Daemon.getMissionSnapshot` now expose the owned view, seeded by successful full replies and updated on the existing ordered intake before callbacks. Getters are atomic with lifecycle and owner-ID checks; absence differs from an invalid view. Routine usage/heartbeats do not create missions. Malformed mission mutations invalidate only mission observation until a new full baseline; foreign-session events are filtered before application.

The mission controller reference (`chunk-5UXINOXG.mjs:14882–14900`) prefers inclusive usage for the owner or known workers. The earlier base-only `applyEventAt` interpretation was wrong and is corrected, with a failure-before regression. Reply-time worker fallbacks use captured UTC receive samples via `requestReplyObservedAt`; notification fallbacks use local observation time. These are not peer clocks or verified worker-start times. `hsdk-j47` subsequently verifies provisional retention, cross-session mission association, scoped cached lookups and associated mission subscriptions through the same intake; see [association evidence](development.md#mission-association-and-provisional-observation).

## Implemented local session snapshots, references and tool overrides

`src/Factory/Droid/Schema/Session.hs` adds three local definitions; `src/Factory/Droid/Schema/Tools.hs` adds the tool-override body. `test/SessionSpec.hs` now supplies 42 checks across its shared and local surfaces.

| Definition | Haskell type | Field mapping |
| --- | --- | --- |
| `SessionSchema` | `SessionSnapshot` | `messages` → `sessionMessages`; `title` → `sessionTitle`; extensions → `sessionSnapshotAdditionalFields` |
| `SandboxStatusSchema` | `SandboxStatus` | `enabled` → `sandboxEnabled`; `mode` → `sandboxMode`; extensions → `sandboxStatusAdditionalFields` |
| `ToolOverrideParamsSchema` | `ToolOverrideParams` | `additionalToolIds`, `enabledToolIds`, `disabledToolIds`, `restrictToolIds` → corresponding `override*` selectors; extensions → `overrideAdditionalFields` |
| `WorktreeGitRefSchema` | `WorktreeGitRef` | Private constructor; `mkWorktreeGitRef` validates the supplied pattern and 1–255 code-point length; `worktreeGitRefText` preserves the spelling |

Session snapshots reuse complete message codecs without sorting or deduplicating messages. Sandbox mode remains optional and independent of the enabled flag. Tool-override lists preserve absence, empty lists, duplicates and overlapping IDs without computing effective permissions. Restriction is a separate allowlist, not a source of permission elevation. These bodies agree structurally with TypeScript `dist/chunk-5UXINOXG.mjs:3075–3078,3545–3561`; no live session or sandbox is created.

The worktree-reference codec follows the supplied Unicode regular expression rather than stronger Git ref-format rules: for example, `refs//topic`, `branch.lock` and `@` remain wire-valid. It rejects leading dashes, double dots, the excluded punctuation, all Unicode Cc controls and ECMAScript whitespace. The latter requires explicit U+2028, U+2029 and U+FEFF handling beyond Haskell `isSpace`; the initial implementation missed the two separators, and the regression sweep caught and corrected that mismatch.

A JavaScript Unicode-mode evaluation of the supplied pattern enumerated its excluded characters. The native test compares every Unicode scalar value against that oracle, separately testing length and multi-character constraints, and requires no Node.js installation. This is evidence for the reference codec, not complete Draft-07 instance validation or live Git/CLI compatibility. Existing free-form worktree metadata fields have not been strengthened to this new type.

## Implemented automation target inputs

`src/Factory/Droid/Schema/Automation.hs` implements three local input definitions with 28 checks in `test/AutomationSpec.hs`. Read and delete share the same body because their supplied constraints differ only in description annotations.

| Definition | Haskell type | Field mapping |
| --- | --- | --- |
| `AutomationListToolInputSchema` | `AutomationListToolInput` | `executionLocation` → `automationListLocation`; extensions → `automationListAdditionalFields` |
| `AutomationReadToolInputSchema` | `AutomationReadToolInput` (`AutomationTarget`) | `executionLocation` → `automationTargetLocation`; `automationId` → `automationTargetId`; extensions → `automationTargetAdditionalFields` |
| `AutomationDeleteToolInputSchema` | `AutomationDeleteToolInput` (`AutomationTarget`) | The same location/ID/extension fields |

`AutomationExecutionLocation` distinguishes local and remote; targets reuse `NonEmptyText` for the ID without trimming or side effects. The older engine tool-input schema trims IDs (`dist/chunk-5UXINOXG.mjs:5127,5199–5215`), whereas the supplied wire schema preserves nonempty strings. These tool-input schemas are distinct from required daemon automation RPC bodies, whose implemented create/update bindings use their own source-checked parameter schemas. Unused engine-tool refinements do not block SDK functional parity.

## Implemented daemon workspace bodies

`src/Factory/Droid/Schema/Daemon/Workspace.hs` implements 14 daemon-owned definitions and four inline request bodies. `test/DaemonWorkspaceSpec.hs` supplies 175 checks, using the daemon snapshot alongside the local definitions reused by directory validation. These are data codecs, not a daemon connection, filesystem implementation or trust manager.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `DaemonCheckFolderTrustRequestParamsSchema` | `CheckFolderTrustParams` (`FolderPathParams`) | `path` → `folderPath` |
| `DaemonTrustFolderRequestParamsSchema` | `TrustFolderParams` (`FolderPathParams`) | The identical path-only body |
| `DaemonCheckFolderTrustResultSchema` | `CheckFolderTrustResult` | `isTrusted` → `folderIsTrusted`; `trustRootPath` → `folderTrustRootPath`; `promptRequired` → `folderPromptRequired` |
| `DaemonTrustFolderResultSchema` | `TrustFolderResult` | `trustRootPath` → `trustedRootPath` |
| `DaemonGetWorkspaceFileContentResultSchema` | `GetWorkspaceFileContentResult` | `content`, `byteLength`, `encoding`, `mimeType`, `isBinary`, `fingerprint` → corresponding `workspaceFile*` selectors |
| `DaemonWriteWorkspaceFileContentResultSchema` | `WriteWorkspaceFileContentResult` | `byteLength` → `writtenFileByteLength`; `fingerprint` → `writtenFileFingerprint` |
| `DaemonListFilesResultSchema` | `ListFilesResult` | `files` → `listedFilePaths`; `directories` → `listedDirectoryPaths`; `totalFiles` → `listedFilesTotal`; `completeDepth` → `listedFilesCompleteDepth`; `truncated` → `listedFilesTruncated` |
| `DaemonSearchFilesResultSchema` | `SearchFilesResult` | `files` → `searchedFilePaths`; `totalFiles` → `searchedFilesTotal` |
| `DaemonPushCwdFileToUrlRequestParamsSchema` | `PushCwdFileToUrlParams` | `sessionId` → `pushFileSessionId`; `filePath` → `pushFilePath`; `presignedPutUrl` → `pushFilePresignedUrl`; `contentType` → `pushFileContentType` |
| `DaemonPushCwdFileToUrlResultSchema` | `PushCwdFileToUrlResult` | `byteLength` → `pushedFileByteLength` |
| `DaemonPullUrlToCwdFileRequestParamsSchema` | `PullUrlToCwdFileParams` | `sessionId` → `pullFileSessionId`; `presignedGetUrl` → `pullFilePresignedUrl`; `destPath` → `pullFileDestination`; `expectedContentLength` → `pullFileExpectedLength` |
| `DaemonPullUrlToCwdFileResultSchema` | `PullUrlToCwdFileResult` | `byteLength` → `pulledFileByteLength`; `writtenPath` → `pulledFileWrittenPath` |
| `DaemonChangeWorkingDirectoryRequestParamsSchema` | `ChangeSessionWorkingDirectoryParams` | `sessionId` → `changeDirectorySessionId`; `workingDirectory` → `changeDirectoryPath` |
| `DaemonSetupStepProgressNotificationParamsSchema` | `SetupStepProgress` | `sessionId` → `setupProgressSessionId`; `kind` → `setupProgressKind`; `text` → `setupProgressText` |

The following inline bodies occur at each request definition's `allOf/1/properties/params`. Implementing them does not complete the containing request envelope.

| Request body | Haskell type | Field mapping |
| --- | --- | --- |
| `DaemonGetWorkspaceFileContentRequestSchema/params` | `GetWorkspaceFileContentParams` | `sessionId`, `filePath`, `metadataOnly`, `encoding` → `readFileSessionId`, `readFilePath`, `readFileMetadataOnly`, `readFileEncoding` |
| `DaemonWriteWorkspaceFileContentRequestSchema/params` | `WriteWorkspaceFileContentParams` | `sessionId`, `filePath`, `content`, `baseFingerprint` → corresponding `writeFile*` selectors |
| `DaemonListFilesRequestSchema/params` | `ListFilesParams` | `sessionId`, `path`, `showHidden` → corresponding `listFiles*` selectors |
| `DaemonSearchFilesRequestSchema/params` | `SearchFilesParams` | `sessionId`, `query`, `maxResults`, `showHidden` → corresponding `searchFiles*` selectors |

Every record retains its own additional-fields object and redacts `Show` output, including extension fields. Explicit JSON encoding and field access remain sensitive. The existing redacted-record and schema-array test helpers were moved unchanged into `test/SchemaTest.hs`, retaining earlier MCP/discovery checks. The daemon directory-validation body matches the existing `ChangeWorkingDirectoryParams`; its response refers to the existing local `ValidateWorkingDirectoryResult`, so neither codec is duplicated.

Numeric domains follow each declaration: list totals/depth and write/transfer lengths use `Natural`, while search totals/limits and content-read byte lengths use `Scientific`. Identically named fields do not imply identical constraints. Missing options remain absent, including list/search default annotations. Trust flags are independent reports; a false `promptRequired` flag is not converted into a trust grant.

`WorkspaceEncoding` distinguishes utf8/base64 without decoding content. `SetupStepKind` distinguishes worktree creation, setup script and origin pull without performing them. Paths, MIME labels, fingerprints and presigned URLs remain opaque wire strings; no containment check, fingerprint comparison, transfer, trust mutation or process-directory change occurs. Those operational boundaries require their own validation and authorization.

TypeScript defines the older workspace contracts at `dist/chunk-5UXINOXG.mjs:7101–7131,7499–7518,7668–7742`. Its file-list result has only `files`, its list input lacks `path`, and its file-content result lacks the newer fingerprint field. It also materializes list/search defaults. The supplied snapshot governs the richer wire data, including write/concurrency fields; current codecs do not establish older-client or live-daemon interoperability.

## Implemented terminal wire bodies

`src/Factory/Droid/Schema/Daemon/Terminal.hs` implements 15 daemon-owned definitions and inline screen state. `test/TerminalSpec.hs` supplies 147 checks; `test/TimestampSpec.hs` adds 59 date-time checks. These types do not create PTYs, spawn processes, send terminal input, resize screens or dispatch events.

| Definition | Haskell type | Field mapping, excluding extensions |
| --- | --- | --- |
| `CreateTerminalRequestParamsSchema` | `CreateTerminalParams` | `terminalId`, `cols`, `rows`, `cwd`, `env` → corresponding `createdTerminal*` selectors |
| `WriteDataRequestParamsSchema` | `WriteTerminalDataParams` | `terminalId` → `writtenTerminalId`; `data` → `writtenTerminalData` |
| `ResizeRequestParamsSchema` | `ResizeTerminalParams` | `terminalId`, `cols`, `rows` → corresponding `resizedTerminal*` selectors |
| `CloseTerminalRequestParamsSchema` | `CloseTerminalParams` | `terminalId` → `closedTerminalId` |
| `CreateTerminalResultSchema` | `CreateTerminalResult` | `success: true` → `TerminalCreated`; `success: false, error: TerminalIdExists` → `TerminalAlreadyExists`; each constructor carries its branch extensions |
| `TerminalInfoSchema` | `TerminalInfo` | `id`, required nullable `pid`, `cols`, `rows`, `createdAt`, optional `state` → corresponding `terminalInfo*` selectors |
| `ListTerminalsResultSchema` | `ListTerminalsResult` | `terminals` → `listedTerminals` |
| `DaemonCreateTerminalRequestParamsSchema` | `DaemonCreateTerminalParams` | `sessionId` → `daemonCreateSessionId`; base fields → `daemonCreateTerminal` |
| `DaemonWriteDataRequestParamsSchema` | `DaemonWriteTerminalDataParams` | `sessionId` → `daemonWriteSessionId`; base fields → `daemonWriteTerminal` |
| `DaemonResizeRequestParamsSchema` | `DaemonResizeTerminalParams` | `sessionId` → `daemonResizeSessionId`; base fields → `daemonResizeTerminal` |
| `DaemonCloseTerminalRequestParamsSchema` | `DaemonCloseTerminalParams` | `sessionId` → `daemonCloseSessionId`; base fields → `daemonCloseTerminal` |
| `DaemonListTerminalsRequestParamsSchema` | `DaemonListTerminalsParams` (`SessionIdParams`) | Reuses the exact shared session-ID body |
| `TerminalDataNotificationSchema` | `TerminalData` | `type` = `daemon.terminal_data`; `terminalId` → `terminalDataId`; `data` → `terminalDataText` |
| `TerminalExitNotificationSchema` | `TerminalExit` | `type` = `daemon.terminal_exit`; `terminalId` → `terminalExitId`; `exitCode` → `terminalExitCode`; `signal` → `terminalExitSignal` |
| `TerminalNotificationSchema` | `TerminalNotification` | The complete data/exit union, reusing the concrete notification codecs |

Session-scoped bodies share base encoders/parsers while retaining flat JSON. Their session IDs cannot be overridden by base extensions. Creation success and failure reserve only their own fields, so an `error` member on the success branch remains an extension. PID is always present and may be null; dimensions, PID and exit codes retain unconstrained JSON numbers rather than being rounded or forced positive.

Inline `TerminalScreenState` maps serialized/plain text, dimensions, timestamp and optional cursor visibility to `screen*` selectors. No ANSI processing, screen restoration, dimension reconciliation or event lifecycle is performed. Newly defined terminal `Show` instances redact bodies; the shared session-ID alias retains its existing instance. Explicit encoding remains sensitive.

The baselined TypeScript contract is in `dist/chunk-5UXINOXG.mjs:4668–4813`. Its terminal dates use active `z.coerce.date()` decoding, accepting and normalizing inputs beyond the supplied string/date-time wire shape. On 2026-09-13 the user selected the strict wire-string contract instead: no coercion of null, booleans, numbers, arrays or legacy/local-time strings; no calendar rollover or millisecond truncation. Valid offsets, case, precision and leap-second spelling are retained. This is an explicit contract choice, not a claim of equivalence to the older coercive model.

Both `TerminalInfo.createdAt` and `TerminalScreenState.timestamp` use that scalar through `Daemon.listTerminals` and terminal restoration. Invalid values produce `RpcInvalidResult`, do not replace retained terminal state, and leave the connection usable. Native-peer checks cover both fields, exact valid receipts and the reference-derived coercion inputs.

### Date-time validation boundary

`Rfc3339Timestamp` in `Schema/Primitives.hs` has a private constructor and preserves offsets, case and arbitrary fractional precision. `mkRfc3339Timestamp` checks ASCII syntax, Gregorian calendar validity and the UTC month-end position of positive leap seconds; `rfc3339TimestampText` returns the original text. Equality/ordering are textual, not instant comparisons. The `time` package supplies calendar validation rather than duplicated leap-year arithmetic.

This boundary follows [RFC 3339 sections 5.6–5.8](https://www.rfc-editor.org/rfc/rfc3339.txt) and [Draft-07 date-time format guidance](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.7.3.1), with cases informed by the [JSON Schema test suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/optional/format/date-time.json). Aeson 2.2.4.1 was probed: its UTC decoder permits missing seconds/space separators but rejects lowercase separators and fractions beyond twelve digits. Consequently, it is not used as the wire-format validator.

The retained wire-string boundary does not validate IERS occurrence dates or future negative-leap announcements. RFC 3339 section 5.7 makes those distinct from syntax and cautions against generating unannounced leap timestamps. The SDK retains supplied observations; it does not generate leap events or promise astronomical occurrence validation. A positional string such as `9999-12-31T23:59:60Z` can therefore decode without asserting that such an event will occur.

Required impact is bounded: terminal results and `TerminalWireTimestamp` retain this spelling; the existing `WorktreeProfile` codec retains the same documented boundary. MCP `lastModified` validation separately rejects second `60`, following its own baselined contract. No required SDK operation supplies an IERS catalog or leap-event forecasting facility. `hsdk-w8y` is resolved as nonblocking scope, not by claiming full RFC 3339 occurrence validation or adding speculative networking. Existing codecs and calendar/offset rejection tests remain intact.

## Implemented worktree management and profile bodies

`src/Factory/Droid/Schema/Daemon/Worktree.hs` implements 18 daemon-owned definitions with 145 checks in `test/WorktreeSpec.hs`. The supplied 1.205.0 snapshot establishes these newer shapes; no corresponding named worktree-profile/managed-worktree definitions were found in the baselined TypeScript bundle. Runtime profile-save refinement remains separate work.

| Definition | Haskell type | Field mapping, excluding open-record extensions |
| --- | --- | --- |
| `WorktreeSetupScriptSchema` | `WorktreeSetupScript` (`BoundedText 100000`) | At most 100,000 code points; empty scripts remain valid data |
| `WorktreeSetupProfileSourceSchema` | `WorktreeProfileSource` | `local`, `repository` |
| `WorktreeSetupProfileFileContentsSchema` | `WorktreeProfileContents` | Optional `name`, `script`, `cleanupScript`, `initialPrompt` → corresponding `profileContent*` selectors; closed object |
| `WorktreeSetupProfileFileSchema` | `WorktreeProfile` | `id`, `name`, `createdAt`, `updatedAt`, `script`, `cleanupScript`, `initialPrompt`, `source` → corresponding `profile*` selectors |
| `DaemonListWorktreeSetupProfilesRequestParamsSchema` | `ListWorktreeProfilesParams` | `cwd` → `profilesCwd`; closed object |
| `DaemonDeleteWorktreeSetupProfileRequestParamsSchema` | `DeleteWorktreeProfileParams` | `cwd` → `deletedProfileCwd`; `profileId` → `deletedProfileId`; closed object |
| `DaemonListWorktreeSetupProfilesResultSchema` | `ListWorktreeProfilesResult` | `profiles` → `listedProfiles`; `lastUsedProfileId` → `profilesLastUsedId`; `repoRoot` → `profilesRepoRoot` |
| `DaemonSaveWorktreeSetupProfileResultSchema` | `SaveWorktreeProfileResult` | `profile` → `savedWorktreeProfile` |
| `DaemonManagedWorktreeSessionSchema` | `ManagedWorktreeSession` | `sessionId`, `title`, `updatedAt` → corresponding `managedSession*` selectors |
| `DaemonManagedWorktreeSchema` | `ManagedWorktree` | `path`, `repoRoot`, `lifecycle`, `sessions`, `branch`, `isClean`, `sizeBytes` → corresponding `managedWorktree*` selectors |
| `DaemonListManagedWorktreesRequestParamsSchema` | `ListManagedWorktreesParams` | `includeSizes` → `includeWorktreeSizes`; closed object |
| `DaemonListManagedWorktreesResultSchema` | `ListManagedWorktreesResult` | `worktrees` → `listedWorktrees`; `cleanlinessPending` → `worktreeCleanlinessPending`; `sizesPending` → `worktreeSizesPending` |
| `DaemonCleanupWorktreeRequestParamsSchema` | `CleanupWorktreeParams` | `worktreePath` → `cleanupWorktreePath`; `deleteLocalBranch` → `cleanupDeleteLocalBranch`; `deleteRemoteBranch` → `cleanupDeleteRemoteBranch`; `force` → `cleanupForce`; closed object |
| `DaemonCleanupWorktreeResultSchema` | `CleanupWorktreeResult` | `worktreePath` → `cleanupReportedPath`; `archivedSessionIds`, `worktreeRemoved`, `localBranchDeleted`, `remoteBranchDeleted`, `warnings`, `branch`, `preservedReason` → corresponding `cleanup*` selectors |
| `DaemonWorktreePreservedReasonSchema` | `WorktreePreservedReason` | `uncommitted_changes`, `removal_failed` |
| `DaemonSessionArchiveStateChangedNotificationParamsSchema` | `SessionArchiveStateChanged` | `sessionId`, `title`, `archivedAt`, `cwd`, `repoRoot`, `worktreeRemoved` → `archiveSessionId`, `archiveTitle`, `archiveTimestamp`, `archiveCwd`, `archiveRepoRoot`, `archiveWorktreeRemoved` |
| `DaemonWorktreeBranchChangedNotificationParamsSchema` | `WorktreeBranchChanged` | `checkoutPath` → `changedWorktreeCheckoutPath`; `branch` → `changedWorktreeBranch` |
| `DaemonWorktreeRemovedNotificationParamsSchema` | `WorktreeRemoved` | `checkoutPath` → `removedWorktreeCheckoutPath` |

`WorktreeProfileName` adds the 1–100 code-point name domain, reusing `BoundedText` rather than duplicating its upper-bound logic. Scripts and initial prompts reuse the 100,000-point bound; UUID and timestamp fields reuse their existing scalar types. Timestamp occurrence verification remains subject to `hsdk-w8y`. The closed file-content shape permits an empty object and is not confused with the open metadata shape or the runtime-refined save request.

Managed paths, repository roots, session IDs and session titles use `NonEmptyText`; free-form response paths and optional branches retain their broader string domains. Size is a nonnegative integer, while managed-session update time is an unconstrained JSON number. Archive timestamps remain unparsed strings as declared. Flags, pending reports, warnings and preservation reasons are not reconciled into inferred outcomes.

Closed records have no extension map and reject unknown fields. Open records preserve extensions, and record `Show` output is redacted. Shared test machinery now factors the common record-shape checks for open and closed objects; existing open-record checks remain intact. No script, Git operation, profile save/delete, worktree cleanup or session archive occurs during parsing or encoding.

## Creation and resumption configuration mapping

Mapping leaf `hsdk-docs-package-signoff-ip3.2.1.1` established this 2026-09-11 baseline; initialization `.2.1.2`, resume/load `.2.1.3` and operation refinements `.2.1.4` now close all four configuration children. Required protocol/release decisions remain separate. References are pinned Python `SessionConfig` / `Session` / `DroidClient`, TS 0.7.0 `node.d.ts:578–617`, `node.mjs:3215–3252,3828–3924`, and `chunk-5UXINOXG.mjs:3566–3677,15950–16038,16185–16248`. Newer schema fields alone do not expand the functional baseline. See [initialization](development.md#initialization-configuration-delivery) and [load-policy delivery](development.md#resumeload-policy-delivery) for evidence and explicit source differences.

| Configuration surface | Current native equivalent | Exact gap or disposition |
|---|---|---|
| Executable, argv, ordinary/trusted environment and credentials | `DroidLaunchOptions`, process preparation, injected transports, explicit daemon/relay authentication | Existing transport/launch work is reused. Environment injection can supply a local API key without argv exposure. Safe owned child IPC and attribution/observability are separate tracked work, not constructor-code duplicates. |
| Working directory and normal startup timing | Ordered local cwd state/getters plus existing daemon cwd receipts/state | Create prefers validated worktree cwd; load prefers direct cwd, then a legacy path only when omitted. Unknown successful replacement state may inherit known parent cwd; explicit null/invalid state is not concealed. Launch cwd is not fabricated saved metadata. Handle publication remains behind load/restoration boundaries. |
| Machine identity | Optional `droidMachineId` / `droidSessionMachineId`; existing daemon machine options | Local absence selects `default`; explicit empty strings are preserved rather than copying Python's truthy fallback. Local explicit machine options are creation-only. |
| Model | Optional create model in local/daemon options; local resumed sessions support an explicit post-load model update | Existing behavior remains. Local post-load override is a native convenience, not proof of creation-time model selection or a required high-level Python/TS resume option. Daemon resume rejects create-only model options. |
| Reasoning, interaction/autonomy and spec model/reasoning | `SessionConfiguration` feeds owned/injected constructors using existing enums and field helpers | Implemented at initialization, before handle publication; no substitute post-init update. Init strings remain non-nullable, unlike update-patch clears. |
| System prompt | Local prompt options plus `daemonSystemPrompt` / `daemonClientSystemPrompt`; raw override is a separate configuration field | Both structured and older override surfaces are forwarded without conflation. Missing acknowledgment of a requested structured prompt rejects the receipt before settings/handle publication. Resume still rejects creation-only prompt options. |
| Four tool-selection families | Existing `ToolPolicy`, creation encoder and retained load intent | All four preserve list order/duplicates/empty values. `prepareLoadSessionParams` separates the three legal load fields from the post-load restriction update, applied before publication. No invented restriction load parameter. |
| Explicit permission rejection and builtin-skill disablement | Creation and load configuration with existing handler/authentication checks | Explicit true/false are forwarded; omission uses retained intent or the established handler-derived default. These are engine inputs, not local permission decisions. |
| Session source and caller tags | Typed creation metadata and retained load source/origin/location | Provenance survives relevant loads; tags remain creation/settings data, not a load parameter. Canonical Haskell SDK attribution remains `hsdk-haskell-attribution-80m`; no other language identity is forged. |
| External/hosted MCP and OAuth callback | `McpSessionOptions`, validated startup/load fields, init-only block-on-load rejection, scoped hosted server ownership | Already integrated. Preserve omission/empty server lists, callback URI and startup/restore ownership when adding configuration. Do not create another MCP registry or restart hosted servers for every replacement. |
| Worktree creation | Existing daemon worktree capability, low-level init fields and local observed cwd | Existing daemon behavior is preserved. Local init validates worktree paths; load supports the legacy path fallback when direct cwd is omitted. Schema-only newer branch/profile controls are not automatic requirements. |
| Resume/replacement tool and spawn policy | Local backend state and existing daemon load-entry snapshots | Policy merge and generation advance share admission; old token waits/queued writes cannot apply stale load or restriction intent. Failed restriction restoration prevents readiness/handle publication; local rollback reapplies policy and restores cwd. |
| Snapshot size and daemon spawn controls | Exact load selection/message limit and `DaemonLoadConfiguration` | Normal daemon default is full-message load; explicit false is retained. Message limits are positive finite integers. Inactivity disablement/runtime path persist; init duration never goes on load. Unsafe bypass requires explicit load intent and is not copied automatically from init. Definitive close clears per-session policy. |
| Session structured-output capability | Nullable init/load fields using existing `OutputFormat` | Omission preserves retained intent; explicit null clears and object schema replaces. No local JSON Schema keyword evaluation or confusion with per-turn result decoding. |
| Advanced init/load operation parameters | Validated local/daemon Client parameters and shared constructor encoders | Required init/load fields, protected identity/token/extensions and existing receipt owners are integrated. Direct low-level restriction/init-only load fields are rejected rather than silently stripped; raw result objects remain available to advanced callers. |
| Cancellation, readiness and operation timing | Existing owners plus a wire-only deadline variant and daemon stable-session initialization retry | Two attempts, only for wire `RpcRequestTimedOut`, use stable session ID/token/params and fresh RPC IDs. One readiness gate covers both; restoration and caller code stay outside retry. Per-attempt microsecond override and aggregate overflow/preflight checks are implemented; no arbitrary mutation replay. |

**Required implementation boundaries.** The existing children retain their roles: `.2.1.2` owns demonstrated initialization/configuration gaps and validated init parameters; `.2.1.3` owns resume/load parameters, saved-cwd publication and replacement policy retention; `hsdk-runtime-refinements-tfo` owns only operation-required validation/default/null/version decisions. No new hierarchy or generic transport/session runtime is required. Both owned and injected constructors must reach the same option encoders, and explicit identity/token fields must remain protected against extension-map collisions.

**Refinement decisions carried forward.** The published TS init schema tolerates invalid interaction/autonomy values by omission; Python's typed boundary is stricter. Published TS init accepts free-form `sessionLocation` while its load schema narrows it; Python and supplied 1.205.0 load types permit a broader string. Initialization/update spec-field omission/null, positive message limits, explicit false/empty lists, deprecated settings, canonical attribution and structured-output capability clearing require operation-specific decisions. Existing `ToolPolicy`, tag/source/prompt/settings codecs and JSON helpers should be extended or reused, not bypassed by raw JSON throughout normal SDK paths. Source-only response-shape/default claims are not live certification.

Mapping evidence: `/tmp/droid-configuration-map.wMXob7My/parameter-surface.log`, six executed TS builder cases plus two structural Python checks in `configuration-reference.mjs`, a compiled-only existing-surface example, and twelve passing injected-runtime checks. These verify the mapping's current equivalents and discrepancies; they do not implement the missing options. See [mapping fess](development.md#configuration-mapping-delivery).

## Capability matrix

The matrix records implemented portions and target module boundaries, not completed SDK parity. Native process transport, scoped correlation/callbacks, typed local operations and global daemon MCP bindings are implemented. `Factory.Droid` supplies local sessions, streams/results, validated inputs and scoped controls; `Factory.Droid.Daemon` supplies authenticated connection and session scopes over native WebSockets. External MCP configuration, management, authentication forwarding and early observation share the same runtime. Remaining rows are functional work rather than a request to exhaust the schema inventory.

Python `transport.py:82,325–365` supplies the lifecycle reference: configurable SIGTERM grace, followed by SIGKILL, including when close is cancelled. The Haskell channel makes grace an explicit microsecond argument rather than supplying the Python default of five seconds. It uses non-blocking exit polling to avoid cancelling a `waitForProcess` that may already have consumed the child status. Unlike Python's suppressed two-second post-kill timeout, final Haskell reaping defers asynchronous exceptions and depends on the kernel completing exit. Descendants and an absolute cleanup deadline remain outside this slice. See [development and verification](development.md) for the failure reproduction and stress evidence.

| Capability | Haskell destination | Verification destination |
| --- | --- | --- |
| Wire envelopes, methods, errors and metadata | `Factory.Droid.Schema.RPC`, `Factory.Droid.Schema.Metadata`, `Factory.Droid.Schema.Local`, `Factory.Droid.Client` (34 named local operations and global daemon MCP get/update bindings) | `test/RPCSpec.hs`, `test/EnvelopeSpec.hs`, `test/MetadataSpec.hs`, `test/ClientSpec.hs`; native MCP configuration fixtures |
| Optional exhaustive shared/local/daemon codec coverage | `Factory.Droid.Schema.*`; [deferred report](exhaustive-codec-backlog.md) | Separate missing-name/runtime-conformance ledger; not an active functional completion gate |
| JSONL process transport and ACP/StreamJsonRpc launch modes | `Transport.Process.droidProcess` constructs the exact mode-specific command; ordinary Factory sessions use the same mode arguments through launch preparation | Six new mode/argv/raw-frame/failure/exception/cancellation/reaping cases, 210 focused local/process checks and two extracted-source checks passed at the ACP boundary. No ACP method implementation or live interoperability is implied; see [ACP delivery](development.md#acp-process-mode-delivery) |
| Launch arguments, environment, stderr and owned IPC — verified offline | `DroidLaunchOptions`, `prepareDroidProcess`, normal-session plumbing and `withJsonLinesProcessStderr` preserve explicit arguments and sanitizer/trusted ordering. `withJsonLinesProcessIpc` adds owned JSONL IPC through the same-PID C launcher and existing process/session owners; settings, errors and ownership are documented in [owned IPC usage](../README.md#owned-child-ipc) | Sixteen native cases, actual Node exchange, compiled example and full tooling pass. MacOS and GNU/Linux ARM64 each passed 3,935 tests under GHC9.10.3 and9.12.4 at that checkpoint. Coverage includes exact values, process/session flags, startup/blocked-I/O cancellation, blocking-mode stderr, duplex/stdout ownership and concurrent unrelated execs. Independent audit findings were reproduced and fixed; see [fess](development.md#owned-child-ipc-implementation-and-fess-checkpoint) and [Linux evidence](development.md#linux-execution-checkpoint). The current compiler target is9.12.4; other-architecture/libc and broader live certification remain separate. |
| Existing-daemon WebSocket connection and authentication | `Factory.Droid.Transport.WebSocket` and `Factory.Droid.Daemon` implement explicit credentials, verified TLS by default and bounded setup/messages/cleanup; no automatic reconnect | Offline plain/TLS peers cover trust/hostname rejection, framing, authentication, deadlines, cancellation and disconnect. The [live daemon check](development.md#september-14-new-key-live-verification) additionally verifies authentication and reconnect on an owned loopback endpoint; no remote TLS certification is inferred. |
| Host-supplied IPC channels | `Transport.IPC.IpcMessageChannel` adapts host text callbacks, availability and disconnect into scoped object I/O for the existing RPC reader, without taking host connection ownership | Seventeen native tests, five extracted-source checks and a compiled-only example pass. Invalid-message retirement is atomic; early events, late callbacks, prefix draining, cancellation and unsubscribe ownership are covered. See [IPC delivery and fess](development.md#host-supplied-ipc-delivery) |
| In-process runtime channels | Supplied connect/disconnect/message/close/error callbacks and pending-readiness gates use `Transport.InProcess` and the shared private callback owner; IPC retains its public API | Fifteen new cases and all seventeen IPC regressions pass. Runtime errors are nonterminal at the raw transport layer; close data, cleanup ordering, cancellation and readiness delegation remain explicit. See [in-process delivery and fess](development.md#in-process-channel-delivery) |
| Normal sessions over supplied transports | `ObjectTransport`, local session-only options and daemon client options feed existing constructors; explicit credentials/inherited auth and raw-RPC readiness gates are separate from locality and controller completion | Twelve injected cases, existing local/daemon regressions, source checks and a compiled example pass. RPC request namespaces prevent deterministic cross-scope ID reuse; hosted MCP rejects unknown locality. See [injection delivery and fess](development.md#injected-session-runtime-delivery) |
| Relay authentication and token refresh | `Transport.Relay` authenticates the existing WebSocket owner and supplies the existing daemon constructors; per-scope organization snapshots and API-key/bearer/grant separation are explicit. Daemon token providers refresh at auth/init/load without cached fallback | Twenty-one native relay cases plus existing injected/daemon regressions, eight source checks and a compiled example pass. Failed I/O retires buffered prefixes as well as socket operations. See [relay delivery and fess](development.md#relay-authentication-delivery) |
| Binary relay tunnels | `relayTunnelTarget`, `withRelayTunnel`, binary send/receive, last-known openness and explicit close share the WebSocket/auth/prefix owners. Full close metadata and the thirty-second setup default remain distinct from object JSON-RPC | Fourteen native tunnel cases plus existing WebSocket/relay cases, eight extracted-class source checks and a compiled-only example pass. Early binary bytes are preserved; text is not data; no application acknowledgment or replay is inferred. See [binary tunnel delivery](development.md#binary-relay-tunnel-delivery) |
| Explicit retry/reconnection policies | `Factory.Droid.Retry`: bounded profiles, both jitter conventions, predicates/hooks/custom delays, explicit recovery/abort and delegated ownership. Connection factories commit before caller work; existing resource owners close failed attempts | Eighteen checks include native relay/daemon acquisition, callback/cleanup no-replay, caller-thread identity, abort boundaries and delegation. Ten source checks and a compiled-only example pass; see [retry delivery](development.md#connection-retry-delivery). |
| Connection readiness and status | `Factory.Droid.Connection` coalesces polling and holds native connection scopes; source pre-spawn/auth budgets, explicit ensure-running, failure/status inspection and in-place identity/epoch-protected auth repair reuse existing owners | Twenty-five readiness cases and existing RPC/health/permission/retry/load checks pass. Teardown ownership, zero-budget and close-classification regressions were reproduced/fixed; eleven source checks and a compiled-only example pass. See [readiness delivery](development.md#connection-readiness-delivery). |
| Connection/request lifecycle hooks | Existing open markers and status plus shared RPC guard/barrier/settlement/pending inspection, dispatcher error/close, logical identity and explicit adapter kind. Normal local handlers and daemon attachment/connection subscriptions are wired through the same owners | Nineteen hook cases and retained process/IPC/in-process/relay, readiness, RPC and pending-authority checks cover ordering, failure, cancellation, filtering and unsubscribe. Eight executed-source cases plus two structural checks; compiled-only example and full integration receipts in [hook delivery and fess](development.md#connection-hook-delivery). |
| Request correlation, server requests and raw notifications | `Factory.Droid.Protocol` / `Protocol.Dispatch`; ordered reply observation and local/daemon session subscriptions use the existing intake. Restored daemon requests enter the same owned worker admission path | Channel/dispatcher tests cover first-acceptance ordering, cancellation, terminal drain, callback requests, boundaries and concurrent active-ID admission; daemon routing and restored-worker cleanup |
| One-shot execution and persistent sessions | `Factory.Droid` implements local new/resumed scopes and one-shot helpers; `Factory.Droid.Daemon` implements new/resumed existing-daemon scopes and follow-up turns | `test/DroidSpec.hs`, `test/DaemonSpec.hs`; actual local one-shot and local/daemon multi-turn HELLO/recall stream-result-session checks passed. |
| Session title and CLI working directory | `Factory.Droid.renameDroidSession` and `changeDroidWorkingDirectory` implemented | Native success/false/error, callback, resolved-path and unchanged-caller-directory checks |
| Complete/partial streaming, messages, usage and outcomes | Local `sendDroidEvents` and daemon `sendEvents` share complete/all-event modes, typed known events, raw unadapted notifications and result accumulation. Text-turn APIs retain outcomes; `sendPrompt` checks success | Native event order/filtering, snapshots/retractions, partial terminal text, raw/malformed payloads and callback failures; daemon session/turn-ID isolation and ACK ordering. Basic text streaming/result identity is verified live on both paths; richer event variants retain offline coverage. |
| Explicit interruption | Local `interruptDroidSession` and daemon `interruptSession` wait for submission and fence following turns; settled interruption preserves usability | Native idle/callback/retry/cancellation tests and local ordering mutants; daemon session-ID, partial-result and reuse assertions. Live local and daemon turns were interrupted on the first nonempty chunk and settled as cancelled. |
| Resume, fork, compaction, rewind and ownership transfer | Local resume/rewind/successor controls retain retirement/rollback. Daemon resume restores pending interactions; connection-level rewind inspection/execution, compact and fork return typed reports without automatic loading or local replacement policy | Native routing/options/results, partial restoration counts, errors, cancellation and caller-deadline checks pass. Compaction's 240-second default is source-checked. Coordinated multi-session ownership has separate offline evidence under F1; live replacement/interoperability remains unverified. |
| Settings, system prompts, spec mode, context statistics and detailed context breakdown | Local/daemon updates, prompt/context queries and observed settings use shared ownership. Dedicated initialization and retained resume/load configuration are verified under the separate configuration parent | Native ACK/view coherence, configuration forwarding, context values, callbacks, errors and cancellation/interruption pass. Stale views and publication/generation defects were reproduced and corrected; no live verification claim. |
| Worker termination, explicit close and caller-supplied bug reports | Local and daemon controls/reports are implemented. Named report methods share runtime UTF-16 source validation while preserving the broader canonical code-point codec | Native explicit routing/content/options, source rejection before dispatch, errors/cancellation/deadlines and no-implicit-action checks pass. No real worker termination, report upload/persistence or live interoperability is claimed |
| Model discovery and local saved-session discovery | `Factory.Droid.listDroidModels` and low-level `Client.listModels` are implemented. `listDroidSessions` selects from the native file layer with cwd/root/outside/limit controls, favorites, deterministic deduplication and preserved observations | Native model/scanner/timestamp tests, 24 selection cases and shared source fixtures pass. Subsequent Mac/Linux native timestamp checks and the 3,935-test suites close all3 discovery children and their parent; broader matrix/live certification remains separate. |
| Images, documents and structured output | `Factory.Droid.Input` validates image/text/PDF values; local and daemon input/output APIs use the same turn engine, explicit object schemas and raw or `FromJSON` output adaptation. Frame limits remain configurable | Native source/size/signature/metadata/UTF-8/Base64 and output tests; daemon wire attachment/structured-output fixture. Actual-source attachment FD/race evidence is retained. The image/PDF helpers are not full parsers, and output handling does not derive schemas automatically. Hosted MCP validation is documented separately; no new live verification. |
| Permissions, typed actions and user questions | `Factory.Droid.Interaction` and local/daemon scopes share validated adapters and safe defaults. Daemon manual scopes expose pending metadata and explicit answers; permission associations and execution-only questions retain isolation | Native default/manual/automatic paths, request-bound validation, token/surface isolation, load restoration, inactive decisions, handler replacement and owned cancellation pass. Response acceptance is local choice, not remote ACK; live permission/question workflows were not exercised. |
| Native tool controls, skills, commands and hook events | Local tool/skill/command discovery and skill controls are implemented. Daemon `listSkills`, `listCommands` and `setSkillDisabled` now provide their baselined counterparts; tool policy remains in settings patches and typed tool/hook events are exposed | Shared codec tests plus native populated/empty metadata, flags/levels/extensions, callback queries, ordinary errors, read cancellation/reuse and mutation interruption/invalidation pass. No local path reads, skill/command execution or invented daemon list-tools RPC |
| External MCP configuration, registry discovery, management and OAuth | Local/daemon report, add/remove/toggle and auth operations; typed startup/OAuth/stored/update configuration in `Schema.MCP.Config`; immutable load/rollback policy; sessionless global daemon connection/get/update; opt-in connection-scoped MCP observation before init/load and through replacement | Native JSONL/WS tests cover forwarding/normalization, omitted/empty/false, header arrays/maps, config errors, global no-session routing, ACK versus completion, early/foreign/malformed events, callback isolation and cancellation/join cleanup. Observer-registration and display regressions are sensitive to their fixes. Real provider OAuth management was not exercised. |
| Haskell-defined MCP tools and session-owned HTTP servers | `MCP.Tool`, `MCP.Server` and the owned native `MCP.Validator` implement raw/typed/structured handlers, precise input/output validation, rich results and authenticated loopback ownership | All styles execute through local/daemon startup/load and local replacement/rollback. Missing-worker HTTP errors, real worker reaping during close, handler finalizers, limits and concurrent/manual/shared ownership pass. Licensed native deployment replaces the restricted profile; [offline acceptance](development.md#hosted-session-integration-acceptance) is supplemented by the full macOS/GNU/Linux ARM64 suites. No live Factory-hosted tool turn is claimed. |
| Attribution, safe logs, metrics and trace propagation | `Factory.Droid.Observability` supplies injectable logging, exact metrics, trace providers and shared-owner instrumentation. The approved native attribution policy omits unsupported optional SDK metadata and uses a truthful free-form daemon caller. | All five observability children are verified offline, including metadata/payload privacy, sink failure isolation, async identity and admission after trace-provider I/O. Native omission/sanitizer contracts and the live daemon path pass without false language branding. |
| Daemon session collections, search, archival and queued messages | Opened/available lists, stored message pages, search, archive/unarchive/observation and ACK-aware queue operations are implemented; retained queues and load coordination are detailed below | Native no-load resource routing, catalog data/errors/events, queue transitions/reconstruction, early/late ACK, legacy replies, RPC/session correlation and cancellation/ownership checks pass. Catalog, archival and queue mutations were not exercised against a live backend. |
| Daemon workspace trust, files, terminals and default settings | Workspace/trust/file operations, setup progress, terminal operations/restoration, observed cwd and global default settings use existing connection ownership. The accepted terminal contract preserves validated RFC3339 text without JavaScript date coercion. | Native routing, trust/cwd isolation, terminal state/events, default-setting resets, exact fields, errors/deadlines/cancellation pass. Read-only default settings were queried in the live daemon check; no remote trust, file or terminal mutation is certified. |
| Workspace-targeted file transfers and proxy-token resources | Session-scoped daemon URL push/pull and global `getProxyToken` are implemented; no SDK token cache or expiry policy is inferred | Native binary transfer checks plus parameterless proxy-request, caller-metadata, token/empty-string, error/cancellation/deadline and redacted-display checks pass. No local transfer ownership or live token-usability claim |
| Custom models, SSH keys, relay, update and logout controls | Custom-model CRUD, `installSshKey`, `triggerUpdate`, relay start/stop/status, `onRelayStatusChanged` and explicit `Daemon.logout` / `Client.logoutDaemon` use existing connection ownership | Typed/native fields, strict-empty-stop, early/malformed/filtering events, callback RPC, literal logout ACK/envelope rules, errors/cancellation/deadlines/disconnects pass. Accepted facade logout clears authentication/pending/deferred authority before return; rejection preserves it. No implicit cleanup logout or real account/key/update/relay/provider operation is claimed |
| Plugins, marketplaces and automations | Plugin/marketplace operations and all sixteen baselined daemon automation lifecycle/configuration RPCs are implemented | Native exact requests/results, source/policy data, identity/scaffolds/history/visuals, required/full config versus nullable model clearing, privacy-only version omission, errors and cancellation/deadlines pass. No real installation, scheduling, writes, rendering, privacy/model change or agent-run proof |
| Git, worktrees, semantic diffs and feedback | Worktree options/receipts/defaults, observed cwd, Git inspection/checkout/PR-status lookup, publishing/readiness, semantic-diff cache/generation and explicit bug-report RPCs are implemented | Native variants/defaults/nullability/bounds, explicit payloads/outcomes, source preflight, errors and cancellation/deadlines pass. No real Git/provider/model/report work, automatic acknowledgement/save or live interoperability claim |
| Experimental crons, missions and Software Factory resources | All six cron methods/state observation; mission readiness/lifecycle/snapshots/association; all twelve Software Factory workstream/signal/change/activity/review/event operations are implemented | Native wire, optional/numeric/projection/privacy, error/deadline/cancellation and mission ownership checks pass. No local scheduler, backend store, real cron/mission/activity execution or worker-process orchestration is claimed; see [cron](development.md#daemon-cron-delivery) and [Software Factory evidence](development.md#software-factory-delivery) |
| Session/mission state stores, subscriptions and low-level controllers | Shared attachments, snapshots, mission association, load/readiness, optimistic submission, conversation/progress/selection, queues, child hydration, terminals, cwd and pending/deferred decisions use existing native owners | All nine coordinated-state children have offline integration evidence. Relay/hooks, configuration, stream/helpers, owned child IPC and saved-session discovery also have accepted evidence; see the individual records and the separately scoped platform/live/distribution checks. |
| Optimistic submission | `SessionState` and daemon preparation/send/observation/local-confirm/cancel operations use one backend and existing connection ownership. Request, placeholder and persisted-message identities remain separate | Native early/late/foreign/malformed confirmation, rejection, monotonic display expiry, caller cancellation/custom exceptions, disconnect, legacy replies, exact wire values, load recovery and lease cleanup pass. Loads require fresh explicit IDs rather than the ambiguous published text heuristic; no synthetic stream messages, renderer, remote rollback or replay |
| Retained conversation history and content | Parent/order repair, raw and checked views, text/thinking/tool updates, orphan adoption, retraction and basic selection use the same immutable session model and daemon intake | Native callbacks observe updated state; malformed recognized events remain checked-invalid until load. Provisional timestamps are not persistence receipts; zero/false metadata and explicit durations remain meaningful. Selected streaming/reference/teardown checks pass; no renderer or tool execution is supplied |
| Todos, progress, phases and retries | Published todo normalization/live result ordering and load restoration; monotone phase observation, timestamp-insensitive consecutive progress deduplication and shared sixty-second cleanup | Native value/order/reload/expiry checks and eight extracted-reference cases pass. Real native expiry completed in 60.004s. Raw numeric inputs remain exact despite field-local JavaScript text coercion; no scheduler, tool execution or retry policy is added |
| Hooks and progressive selection | Typed start/report/lease-expired outcomes, hidden-data filtering, thirty-entry progressive expansion, todo-preserving cache pruning and hydration-floor metadata are implemented through the existing model and waiter | Thirteen new cases and six extracted-reference checks pass; real hook expiry completed in 90.000s without fabricating a completion report. Local selection setters perform no remote history mutation |
| Queue state and explicit processing | Ordered enqueue/replacement, kind-filtered dequeue/clear, front restoration, processed-ID guards, attachment-aware pause, accepted-load reconstruction and explicit local sending reuse existing owners | Twenty new queue-state cases, the existing RPC cases and five selected source checks pass. Missing idle work pauses instead of implicit replay; exact identity replaces unsafe text/clock delivery inference. Failed sends do not overwrite newer queue observations |
| Child discovery, linkage and invocation hydration | Typed availability/load metadata, parent/tool lookup, provisional registration/cwd inheritance, configurable hydration and summary APIs reuse existing owners; current-target revisions and terminal observations extend those owners | Summary-only hydration does not register or execute children. Current terminal/error events update an existing registered summary without requiring a settings tag; pure metric refresh remains separate. Late snapshots preserve newer child and working-state observations. See [current freshness](evidence/2026-09-17/cc15/README.md) and [terminal policy](evidence/2026-09-17/summary-terminal/README.md); frozen-policy differences remain explicit |
| Terminal restoration and output writers | Retained metadata/selection, exact serialized snapshots, buffered output, explicit/automatic restoration and attachment-owned writer registration reuse existing state, RPC intake and lifetimes | Twenty-four new tests, six existing terminal RPC cases and eight source checks passed at that checkpoint; later timestamp-contract checks also pass. Newer output/reset/metadata and request generations are protected; absent screen state cannot erase retained data. No PTY/renderer/ANSI interpreter or implicit terminal creation/close is supplied. |
| Authoritative cwd observations | Accepted init/load precedence, resolved change results, typed cwd events, invalid observations and provisional parent inheritance use `SessionState` and existing ordered intake | Twelve new cases and five source checks pass; pure and daemon event paths agree. Empty/absent reports remain distinct, immutable receipts stay unchanged, and no local path normalization, process-cwd change or implicit trust/validation occurs. See [cwd delivery](development.md#authoritative-cwd-delivery) |
| Pending and deferred interactions | Connection-owned metadata/tokens, enumeration/waits/subscriptions/external fanout, explicit responses, inactive decision storage, mutable handlers and logout clearing extend existing dispatcher/load/state seams | Thirty-two new native cases and eight source checks pass. Method-aware completion prevents permission/question collisions; retirement cannot drop restored replacements. Source automatic replay/resume is replaced by explicit fresh validation, and tool identity is session-scoped. See [contracts and full fess](development.md#pending-interaction-delivery) |
| Queue review helpers | `SessionState.isReviewableQueuedMessage`, `queueDisplayGroup` and `queueReviewPriority` classify existing queue kinds without changing storage or execution | Reference-derived classification/priority cases and a compiled README example pass; zero priority remains reviewable. See [queue review acceptance](development.md#queue-review-helper-delivery). |
| Factory REST compute, template and session APIs | `Factory.Droid.REST` implements all thirteen baselined computer/template/metrics/remote-session helpers; `Schema.REST` supplies typed contracts and pagination | Native HTTP method/path/query/body/result/error, TLS rejection, bounded reads, cancellation/timeout/closure, redirect/retry prevention and redaction checks pass. Port-wrap and body-error regressions failed before correction. Offline REST delivery only; advanced daemon resources and live/platform sign-off remain separate |
| Legacy query/stream capabilities, notification conversion, stream feeding and public compatibility exports | Native equivalents in the modules above | Explicit compatibility mapping and fixtures |
| Documentation, compiled examples and distribution | Cabal package, installation/migration guidance and Haddock | All 77 README Haskell blocks compile without execution; package-local links and Haddock URL/files pass. Separate unpacked builds/tests and documented upstream Haddock limitations are recorded in the release receipts. |

Implementing an operation is not authorization to exercise its external effects during development. Live model calls, remote mutations, publication, and cost-bearing checks require explicit authorization.

The expanded rows follow Python `client.py:603–624,1027–1049,1197–1233,1325–1347,1524–1547`, its exported `RunStream` and legacy notification converter, and TypeScript `dist/index-D_SzTnFR.d.ts:106201–106406,108401–108537,111762–111922,112167–114985`. Low-level load coordination, external interaction dispatch, branch divergence and workspace-targeted transfer methods must be mapped independently of the smaller convenience-resource interfaces. Static inventory does not establish their runtime semantics.

### Existing-daemon core operation contracts

The following bindings are implemented by `Factory.Droid.Daemon` and exercised against offline peers. Their references are the baselined TypeScript 0.7.0 implementation: authentication (`dist/chunk-5UXINOXG.mjs:15884–15903`), ACK handling (`10130` onward), message submission (`10545` onward), load restoration (`16316–16464`) and interaction scope (`23669–23727`). The guide supplies the direct WebSocket endpoint, without an inferred path suffix (`docs/typescript-sdk-reference.md:115–118`). These references do not establish live compatibility with the selected CLI protocol version.

| Operation/event | Public capability and observable contract |
| --- | --- |
| `daemon.authenticate` | Scope creation authenticates with API key or token/optional act-as grant; `authenticatedUser` exposes validated user/organization identity. Remote rejection remains `RpcRemoteFailure`; no initialization follows a rejected authentication. |
| `daemon.initialize_session` | `withSession` uses a caller-selected ID or a fresh UUID, typed creation/spawn configuration and protected current credentials. Required identity/settings, requested prompt acknowledgment and optional worktree receipt are validated before publication. Two wire-timeout attempts retain identical session/params; user work and restoration are not replayed. |
| `daemon.load_session` | `withResumedSession`, configured borrowed attachments and explicit configured loads select a generation-owned policy, fetch a fresh token and validate/restore the snapshot. Restrictions are applied after load and before pending-request restoration/readiness. Conditional writer admission rejects superseded queued effects. Default full-message loading can be overridden explicitly; `daemonLoadedState` remains an immutable receipt, not a claim of complete history for a partial selection. |
| `daemon.update_session_settings` | `Daemon.updateSettings` uses the mutation lease and waits through ACK for a correlated `settings_updated`. `getSettings` reads the ordered shared observation without RPC or prediction. `Client.updateDaemonSessionSettingsRaw` returns the immediate reply. |
| `daemon.rename_session` | `Daemon.renameSession` preserves legacy success/extension fields; an ACK requires correlated, valid `session_title_updated` before returning success. `Client.renameDaemonSessionRaw` exposes immediate replies. Neither adds a title cache. |
| `daemon.list_skills`, `daemon.list_commands` | `Daemon.listSkills`/`listCommands` and corresponding typed `Client` bindings preserve metadata/list wrappers, extensions and optional project availability. Owned queries use read-only leases; no local discovery or execution is performed. |
| `daemon.get_context_breakdown` | `Daemon.getContextBreakdown`/`Client.getDaemonContextBreakdown` reuse the complete shared context report, preserving categories, skill/droid/MCP entries and exact reported token values. Ordinary RPC completion supports callback queries without a notification wait. |
| `daemon.set_skill_disabled` | `Daemon.setSkillDisabled`/`Client.setDaemonSkillDisabled` preserve false/true input, absent/user/project settings level and actual success. Owned mutation uncertainty uses the existing interruption/invalidation policy; explicit session IDs override extensions. |
| `daemon.add_user_message` | Shared input/output/turn APIs send the owned session ID and a fresh message UUID. `{accepted:true}` additionally requires a matching `create_message.requestId`; notification may precede or follow the ACK. Legacy full responses remain supported. One submission budget covers both stages. |
| `daemon.interrupt_session` | `interruptSession` submits the owned session ID after message submission, fences the next turn and preserves a settled cancellation result. Incomplete/uncertain turns retain the common invalidation policy. |
| `daemon.request_permission`, `daemon.ask_user` | Owned/associated permission and owned-only question routing, safe cancellation of unrelated requests, original execution ID in replies, ordinary-error isolation and connection-owned cancellation/finalization. |
| `daemon.session_notification` | Session routing is mandatory. Terminal completion additionally matches the submitted message's `turnId`; foreign session/turn payloads cannot settle or poison the owned turn. Missing IDs and malformed adapted events fail explicitly. |
| Session lifecycle notifications | Owned close, unsubscribe, inactivity or process-exit notifications invalidate the connection, even when a success completion is already queued. Ordinary scope exit sends no saved-session close/delete/logout request and does not terminate the external daemon. |

Transport failures remain explicit at the WebSocket boundary and become the existing channel read/write failures through `RpcChannel`; asynchronous cancellation retains its identity. No background reconnection or daemon provisioning is implied. Shared local controls, REST resources and remaining daemon operation families are not completed by these bindings.

### Diagnostic submission contracts

`daemon.submit_bug_report` maps to `Client.submitDaemonBugReport` and `Daemon.submitBugReport`, reusing `SubmitBugReportParams` and `SubmitBugReportResult`. Session routing overrides extensions; raw comments/logs, absent versus empty values, source metadata and returned report IDs remain data. There is no automatic collection, attribution, sanitization, retry or session loading.

Both this binding and local `Client.submitBugReport` run the shared source preflight before dispatch. It counts UTF-16 via the standard text encoder and reflects each existing `BoundedText` limit; canonical decoding still uses code points. The Python baseline exposes no source field, so its comment/log behavior is unchanged. The generic low-level `call` and standalone codecs remain caller-controlled, not automatic runtime validators.

References: TS 0.7.0 `chunk-5UXINOXG.mjs:1992–2000,5784–5796,11164–11171,17249–17257`; CLI 0.212.1 static source near byte 204339270 has the same `.max` bounds; Python `schemas/client.py:1279–1288` and `client.py:1197–1225`. Extracted published source with Zod 3.24.0 and Haskell checks agree on 45 Unicode vectors. Native local/daemon checks verify preflight, exact content, errors and cancellation/deadlines; no real upload or negotiated cross-version compatibility is claimed.

### Daemon session lifecycle control contracts

| Operations | Haskell mapping / contract |
| --- | --- |
| `daemon.get_rewind_info`, `daemon.execute_rewind` | `Client.getDaemonRewindInfo`/`executeDaemonRewind` and `Daemon.getRewindInfo`/`executeRewind` reuse session/message/file records and all three inspection lists or all four restoration/deletion counters. No local filesystem work or inferred success from a returned ID. |
| `daemon.compact_session`, `daemon.fork_session` | `Client.compactDaemonSession`/`forkDaemonSession` and the corresponding `Daemon` methods preserve raw instructions, optional title/tags and reported successor IDs/counts. Explicit session routing overrides body extensions. High-level compaction uses 240 seconds; low-level budgets are not overridden. |
| `daemon.kill_worker_session`, `daemon.close_session` | `Client.killDaemonWorkerSession`/`closeDaemonSession` and `Daemon.killWorkerSession`/`closeSession` explicitly target remote sessions. Close has optional `preserveEmptyDraft` from the supplied schema; omission retains the TS 0.7.0 request shape. No implicit close on scope exit or local process termination. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:6908–6983,7744–7828,10366,10605–10622,11173–11212,16744–16759,16999–17021` and supplied daemon requests/shared control schemas. These are connection operations returning data, not automatic session replacement or store updates. Owned lifecycle notifications remain authoritative. Native peers cover six high/low paths, failures, all mutation cancellations and a short caller compaction deadline; the 240-second high-level value is source/configuration evidence only.

### Daemon queued-message operation contract

`daemon.resolve_queued_user_message` maps to `Client.resolveDaemonQueuedUserMessageRaw` (immediate RPC reply) and `Daemon.resolveQueuedUserMessage` (ACK-aware completion). Both reuse `ResolveQueuedMessageParams`: `update_queue` requires `end_of_turn`/`end_of_loop`; `delete` has no typed placement. The explicit session ID overrides extensions. The high-level operation reuses submission's listeners and deadline, accepts pre-ACK completion, matches session plus the new RPC ID and preserves legacy result fields; malformed matching events, remote/result errors, timeout, cancellation and disconnect remain explicit.

References: TS 0.7.0 `chunk-5UXINOXG.mjs:10156–10311,10583–10596,16673–16699`; supplied shared control/daemon request/result contracts. Haskell retains its single overall deadline rather than restarting a separate completion timer after the ACK. Native cases and a rejected no-wait mutant verify this operation, not controller queue-store updates or in-flight load guards. Those remain separate functional requirements.

### Daemon Git inspection operation contracts

| Operations | Haskell mapping / contract |
| --- | --- |
| `daemon.list_git_branches`, `daemon.get_git_branch_divergence` | Corresponding Client/Daemon bindings preserve raw cwd/branch strings, required-nullable current branch and tracked/no-remote/unavailable results; tracked counts use `Natural`. |
| `daemon.checkout_git_branch` | Explicit optional create/resolution fields; checked-out and needs-resolution branches, missing-only untracked default and raw RPC error data are retained. No automatic resolution or local Git work. |
| `daemon.get_git_diff` | Typed session/base/stats input; legacy bare report normalization, explicit success/unavailable union, defaulted sections and cached PR metadata. Unknown reasons fall back locally without weakening adjacent fields. |
| `daemon.resolve_pull_request_statuses` | A validated zero-to-twenty lookup batch, typed subjects/reasons and ordered returned statuses; nullable/absent branch/remote/status and provider/lifetime data are not inferred or cached. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:4528–4546,7217–7444,11574–11623` and supplied daemon schemas. The supplied checkout branch adds optional `pullFailure`; structured managed-worktree conflict data remains available in the existing RPC error payload. Five operation paths are covered offline; the remaining publishing/readiness/semantic-diff group is below.

### Daemon publishing, readiness and semantic-diff contracts

| Operations | Haskell mapping / contract |
| --- | --- |
| `daemon.git_push`, `daemon.git_commit`, `daemon.create_pr` | Corresponding Client bindings and `Daemon.gitPush`/`gitCommit`/`createPullRequest` preserve explicit session/message/title/base/draft/link fields and actual results. No local publication or link inference. |
| `daemon.inspect_mission_readiness`, `daemon.acknowledge_mission_readiness_warning` | Separate inspection and acknowledgement calls using raw cwd; typed warning states/levels, independent flags and required-nullable remote URL. No inferred trust/acknowledgement. |
| `daemon.get_semantic_diff_cache`, `daemon.save_semantic_diff_cache`, `daemon.generate_semantic_diff` | Preserve branch keys, required-nullable misses, explicit save content/truncation and generated content/session ID data. Generation has a 180-second high-level deadline; no automatic cache save or session ownership. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:7450–7499,7542–7653,11625–11706,16832–16854` and supplied schemas. Nine grouped codec/native cases cover all eight high/low requests, outcomes/errors and cancellation/deadlines. The 180-second value is source/configuration evidence, not an elapsed-time or model-performance claim.

### Daemon automation lifecycle operation contracts

| Operations | Haskell mapping / contract |
| --- | --- |
| `daemon.list_automations` | `Client.listDaemonAutomations`, `Daemon.listAutomations`; optional base path, raw entries and optional pending setup reports. Invalid entries and distinct directory/UUID/machine/computer identities are retained. |
| `daemon.run_automation` | `Client.runDaemonAutomation`, `Daemon.runAutomation`; explicit address/computer/scaffold data, returned preparation prompt/cwd/model/reminder. No SDK-created agent session or duplicate reminder. |
| `daemon.pause_automation`, `daemon.resume_automation` | Corresponding Client/Daemon bindings share `AutomationStatusResult`, preserving actual success, ID, status and optional error. |
| `daemon.get_automation_history`, `daemon.get_automation_visual` | Typed address/filter inputs and history/visual reports; no time/pagination inference, rendering or URL fetch. |
| `daemon.rename_automation`, `daemon.delete_automation` | Typed target/name and existing success/error contract; no local files, cache update or retry. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:5324–5564,5669–5695,11225–11360,17243–17292,23083–23127` and supplied schemas. `defaultAutomationAddress` mirrors the legacy directory alias, while direct records preserve omission or separate IDs. The supplied snapshot adds reusable memory-file/model-fallback/session-privacy/setup fields without implying negotiated live compatibility. Creation/configuration is covered below.

### Daemon automation configuration operation contracts

| Operations | Haskell mapping / contract |
| --- | --- |
| `daemon.create_automation`, `daemon.fork_automation` | Corresponding Client/Daemon bindings preserve directory/backend/fork identity, raw strings, scaffold data, flags and actual creation results. No local scaffolding or automatic second request is added. |
| `daemon.update_automation_model` | Required nullable model; omission is invalid and null is distinct from empty string. |
| `daemon.update_automation_privacy` | Typed automation/session privacy and optional creator; this binding omits only `factoryProtocolVersion`, preserving other caller metadata and connection defaults. |
| `daemon.update_automation_prompt`, `daemon.update_automation_schedule` | Required raw strings, including empty values; no tool-input or cron-validation policy is substituted. |
| `daemon.apply_automation_config`, `daemon.update_automation` | Shared required name/schedule/prompt configuration, outer-owned extensions, optional fields/clears and actual success/error/failure-reason reports. |

All use existing connection ownership and ordinary RPC deadlines. References: TS 0.7.0 `chunk-5UXINOXG.mjs:5565–5780,10077–10095,11280–11385` and supplied daemon schemas. The runtime privacy omission is explicit and tested against reserved-body injection and later calls. Eleven grouped cases plus existing lifecycle tests cover all sixteen automation operations offline, without claiming backend execution/persistence or full live interoperability.

### Daemon plugin and marketplace operation contracts

| Wire operations | Haskell mapping / contract |
| --- | --- |
| `daemon.list_available_plugins`, `daemon.list_installed_plugins` | `Client.listDaemonAvailablePlugins`/`listDaemonInstalledPlugins`, `Daemon.listAvailablePlugins`/`listInstalledPlugins`; preserve typed metadata and optional free-form scope. |
| `daemon.install_plugin`, `daemon.uninstall_plugin`, `daemon.set_plugin_enabled`, `daemon.update_plugin` | Corresponding Client/Daemon bindings use explicit targets and existing mutation leases. False/error values and mixed per-plugin update results remain observable. |
| `daemon.list_marketplaces`, `daemon.add_marketplace`, `daemon.remove_marketplace`, `daemon.update_marketplace` | Corresponding Client/Daemon bindings preserve source variants, optional target/name, provenance/removability and per-marketplace outcomes. No metadata-driven authorization or local installer is added. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:6004–6221,11072–11161,17203–17242` and supplied daemon schemas. Source objects follow the SDK's declared-field projection (including reported local-path removal); other records retain open-schema extensions. Git refs use SDK trim/min(1), not the worktree-ref validator. SHA pins follow the runtime's case-insensitive forty-hex pattern even though the supplied schema omitted the regex flag. An offline ECMAScript probe and native tests verify these decisions. Missing removability is unknown, not permission. Ordinary replies use the existing thirty-second budget and existing error/invalidation policy.

### Daemon management operation contracts

| Operation/event | Haskell mapping and contract |
| --- | --- |
| `daemon.trigger_update` | `Client.triggerDaemonUpdate` / `Daemon.triggerUpdate`; empty params, actual triggered flag and optional message. |
| `daemon.install_ssh_key` | `Client.installDaemonSshKey` / `Daemon.installSshKey`; nonempty unnormalized public-key text, actual installed flag, no local installation. |
| `daemon.get_proxy_token` | `Client.getDaemonProxyToken` / `Daemon.getProxyToken`; params omitted through the existing `BaseRequest` representation and `requestResult` path; caller metadata retained and reserved fields protected. |
| `daemon.relay.start`, `daemon.relay.stop`, `daemon.relay.get_status` | Corresponding Client/Daemon bindings; URL/identity report, strictly empty stop result and optional-field-preserving status. These manage the daemon relay, not an SDK relay transport. |
| `daemon.relay.status_changed` | `Daemon.onRelayStatusChanged`; shared `RelayStatus` codec and existing connection dispatcher/error/unsubscribe ownership. |

References: TS 0.7.0 `chunk-5UXINOXG.mjs:5813–5841,7654–7666,8063–8113,10756–10800,11707–11715,16856–16858,17047–17060` and supplied daemon definitions. Native peers verify management wire behavior; no live system/key/token/relay outcome is claimed.

### Daemon custom-model operation contracts

| Operation | Haskell mapping | Observable contract |
| --- | --- | --- |
| `daemon.list_custom_models` | `Client.listDaemonCustomModels`, `Daemon.listCustomModels`; `ListCustomModelsResult` | Empty global request; preserve raw summaries, invalid rows and key/mask flags without local filtering. |
| `daemon.upsert_custom_model` | `Client.upsertDaemonCustomModel`, `Daemon.upsertCustomModel`; `UpsertCustomModelParams` | Nonempty model/provider; optional raw index/expected-model guard; API-key omission versus empty string; nullable output/image overrides; no local merging/retry. |
| `daemon.delete_custom_model` | `Client.deleteDaemonCustomModel`, `Daemon.deleteCustomModel`; `DeleteCustomModelParams` | Required raw index and expected-model string, including empty string for an invalid entry. Upsert/delete share `UpdateCustomModelsResult`, retaining actual success and returned rows. |

These reuse ordinary global connection ownership and thirty-second high-level deadlines; low-level deadlines remain caller-owned. References: TS 0.7.0 `chunk-5UXINOXG.mjs:6368–6398,6463–6473,10729–10756,17038–17045` and supplied daemon definitions. The supplied delete response reuses the upsert result definition. Numeric wire acceptance does not promise valid fractional runtime indices; operational native fixtures use ordinary indices. Key retention is the daemon's documented behavior, not an SDK keystore or verified live persistence claim.

### Daemon default-settings operation contracts

| Baseline operation | Haskell implementation | Observable contract and verification |
| --- | --- | --- |
| `daemon.get_default_settings` | `Client.getDaemonDefaultSettings`, `Daemon.getDefaultSettings`; `Schema.Daemon.Settings.DefaultSettings` | Empty global request, without session initialization/load; model/availability, autonomy, worktree, compaction, cloud sync, spec/subagent/mission defaults, management/source metadata, presets and resolution history. `DaemonDefaultsSpec` covers full field round trips, strict adjacent fields, field-local fallbacks, map normalization and whole-chain rejection. |
| `daemon.update_session_defaults` | `Client.updateDaemonSessionDefaults`, `Daemon.updateSessionDefaults`; `UpdateSessionDefaultsParams`/`UpdateSessionDefaultsResult` | Strict enums; omission versus permitted null resets, explicit false/empty values, fractional limits and tier reset arrays. Preserve `success = False` and returned defaults rather than synthesizing state. Native requests, subsequent queries, remote/invalid-result errors, zero-deadline no-send and async cancellation/reuse are verified. |

Both use existing connection/RPC ownership and ordinary thirty-second high-level deadlines. No local file access, inheritance calculation, cache, retry or rollback is added. Contract references: TS 0.7.0 `chunk-5UXINOXG.mjs:6225–6475`, supplied request/result definitions, and the selected CLI result around byte `204432291`. The CLI adds `availableAutonomyLevels`; its invalid-list fallback is preserved. Newer caching/auto-delete fields remain extensions, not newly required typed APIs. Open-record extensions follow the supplied schema convention rather than TypeScript's unknown-key stripping; management maps retain the reference's declared-key filtering and fallback rules. See [usage](../README.md#daemon-default-settings) and [verification](development.md#daemon-default-settings-delivery).

### External MCP operation contracts

Both session backends expose the following operation families. Local requests use `droid.*` with implicit active-session routing; daemon session requests use `daemon.*` with the owned `sessionId`. Global daemon configuration is deliberately separate. The [README](../README.md#external-mcp-management) maps these to public functions and typed configuration.

| Wire operation or event | Implemented contract |
| --- | --- |
| Initialize/load `mcpServers`, `mcpOAuthCallbackUri`; initialize `blockOnMcpLoad` | Stdio/HTTP/SSE startup union, ordered header arrays, OAuth omission/false/options and explicit empty lists; normalize before I/O and replay immutable load policy through local replacement/rollback. Explicit init-only flag on resume is rejected. |
| `add_mcp_server` | Typed name/type and optional transport fields; header maps, validated OAuth and preserved Boolean result. Explicit startup-to-add conversion uses last exact-name header value. |
| `list_mcp_servers`, `list_mcp_tools`, `list_mcp_registry` | Typed reports retain peer flags, errors, source metadata and extensions without synthesizing state. |
| `remove_mcp_server`, `toggle_mcp_server`, `toggle_mcp_tool` | Server mutations use user settings level; tool toggle does not. False acknowledgement remains data. |
| `authenticate_mcp_server` | Five-minute RPC budget, no exclusive auth lock; acknowledgement is not authentication completion. |
| `submit_mcp_auth_code`, `submit_mcp_auth_error` | Explicit server/code/error/state fields; optional error description preserved. No SDK token exchange or browser action. |
| `cancel_mcp_auth`, `clear_mcp_auth` | Distinct explicit actions; neither follows automatically from a timeout or disconnect. Session mutations retain existing lease/invalidation rules. |
| `daemon.get_mcp_config`, `daemon.update_mcp_config` | Authenticated sessionless connection; no implicit session ID. Stored configuration uses maps and source metadata. Read/add support OAuth false; global update requires options. Results preserve success, reports and optional error. |
| `mcp_status_changed`, `mcp_auth_required`, `mcp_auth_completed` | Typed metadata on normal streams/observers, plus opt-in connection-scoped observation before init/load and through replacement/rollback. Source ID, malformed events, channel errors and callback cleanup are explicit. Completion has no request/state ID; per-attempt correlation is not invented. |

Configuration preserves unknown extensions but reserves transport-family fields against branch override. URI-valued fields use the supplied RFC 3986 contract and selected OAuth metadata refinements, not WHATWG alias equivalence. The selected CLI's nonempty merge overlays user/filesystem configurations after inline entries; empty lists skip the merge, and readiness gating is bounded. These source-sensitive facts and offline evidence are recorded in [development notes](development.md#external-mcp-configuration-delivery). SDK-hosted tools, broader daemon resources and live/platform conformance remain outside this completed offline capability.

## Material contract differences

### SDK language attribution: approved omission

`shared.schema.json#/definitions/SdkClientMetadataSchema/properties/language` accepts only `"typescript"` and `"python"`. Python's `SdkClientMetadata` enforces the same restriction; `ClientRequestAttribution` requires this metadata when `client` is `"sdk"`. Consequently, honest `"haskell"` attribution cannot satisfy the supplied enum unchanged.

The older TypeScript package attaches a session tag containing its language and version. This does not establish Haskell support in newer request-attribution, subprocess-environment or daemon-authentication contracts. On 2026-09-13 the user approved omission of unsupported optional language metadata. Native sessions do not add SDK attribution or an automatic SDK tag. Because the SDK branch requires language/version, omission means leaving optional `requestAttribution` absent, not constructing an incomplete SDK object.

The local path strips `FACTORY_UPSTREAM_CLIENT_TYPE` and `FACTORY_UPSTREAM_SDK` from inherited/ordinary environment values. Explicit trusted environment overrides and caller-owned low-level metadata retain their existing contracts. Daemon authentication keeps the truthful free debugging string `caller = "haskell-sdk"`; this is not the restricted language enum, an authentication grant or upstream branded-language support. Codecs still accept only the declared enum values for decoding and explicit caller-owned messages. Native serialization, launch sanitization and authentication checks verify this policy; live certification remains separate.

### Runtime refinements: operational contracts and optional markers

The supplied `x-factory-runtime-only` markers are not an instruction to reproduce every private/backend schema. The approved functional scope uses the pinned Python and published TS SDK operations with explicit selected-CLI/wire decisions. Required predicates below are implemented and tested; the absence of exhaustive, version-matched 1.205.0 predicates for unused definitions is not a release gate. This does not relabel older source as current 1.205.0 semantics.

| Area | Required operational contract and disposition |
|---|---|
| Permission decisions | Private constructors require edited content for `proceed_edit`, including valid empty text; offered-choice membership and local approval remain separate checks. |
| System prompts | Selected CLI 1.201.1 preset preprocessing produces canonical output; nonblank content and requested-prompt acknowledgment are checked. Older TS omission of structured prompt support does not remove the Python-required capability. |
| Settings, init and load | Existing enum fallbacks, strict neighboring fields, init/update null differences, positive limits, exact false/empty values, output clearing, protected extensions and generation-owned publication have explicit native checks and source decisions. |
| Worker completion progress | `Schema.Mission.nonBlankField` implements ECMAScript blank normalization only for commit/repo fields, preserving other strings; progress unions, snapshots and event wiring are implemented. |
| Custom-model operations | Catalogs use `ModelInfo` availability invariants; daemon CRUD uses its own summary/upsert schemas and reports invalid rows intentionally. These paths do not consume `ManagedCustomModelSchema` or execute credential helpers. |
| Automation operations | Published create/update methods validate `DaemonCreateAutomationRequestParamsSchema` / `DaemonUpdateAutomationRequestParamsSchema`, not the engine's `AutomationCreateToolInputSchema` / `AutomationEditToolInputSchema`. Native RPC bodies follow those separate contracts. |
| Managed disk configuration / loop tools | Managed-model registry/auth-mode checks and loop-tool predicates are not required by the inspected SDK call paths. They remain optional schema/backend work; no unconstrained managed-model substitute or helper execution was introduced. |
| Worktree setup-profile save | The newer save-profile input/refinement is present in supplied schemas but absent from the pinned published SDK runtime's operation surface. Existing worktree functionality and represented report codecs remain; this optional marker is not a configuration gate. |

The older managed-model schema lacks `authMode` and uses its package's model registry, so it must not be advertised as exhaustive current managed-file validation. Native custom-model RPCs instead preserve their actual input/report contracts. Non-temporal wire reconciliation and the approved timestamp/attribution choices are resolved; live/platform/package certification retains its own issues. See [refinement reconciliation](development.md#operation-refinement-reconciliation) for routing and execution evidence.

`RequestPermissionResultSchema` maps to private `RequestPermissionResult` plus validated construction/accessors in `Schema.Interaction`. Both older SDKs require `editedSpecContent` for `proceed_edit` and allow an empty string (TypeScript `dist/chunk-5UXINOXG.mjs:3273–3285`; Python `schemas/cli.py:1177–1199`); CLI 1.201.1 enforces the same condition near byte 204394882. The codec implements that selected runtime refinement and preserves other strict optional fields/extensions. The high-level adapter separately enforces offered-choice membership and safe fallback. This operational agreement does not assert exhaustive 1.205.0 runtime semantics.

`WorkerCompletedEntrySchema.commitId` and `.repoPath` normalize strings whose ECMAScript trim result is empty to absence, preserving nonblank spelling (published `chunk-5UXINOXG.mjs:2392–2395,2505–2515`). The native field-local implementation and Unicode/null/type tests are in `Schema.Mission` and `MissionEventSpec`; these progress entries and their snapshots are no longer pending implementation. This is a baselined operational decision, not a claim to reconstruct every 1.205.0 preprocessing annotation.

Settings-update/list-tools fallback fields and current init/load refinements are implemented, with adjacent malformed values rejected rather than hidden by a global fallback. Automation RPC inputs are not engine tool inputs. The unused newer save-worktree-profile refinement therefore remains optional without delaying completed SDK operations. Historical source-input requests remain in the issue history; the approved functional scope supersedes their former blanket gate.

### Resource ownership and completion

Python closes supplied transports as well as transports it creates; injection does not imply borrowed ownership (`client.py:253–297`, `_high_level/_client.py:38–47`). Its high-level stream requires an explicit turn-completed event. The legacy response iterator also supports an idle-state fallback for older runtimes; the two contracts must not be conflated.

Node replacement operations load a successor and retire the source handle. Daemon replacement operations return successor identifiers and leave the source usable (published TypeScript reference, session replacement section). Shared Haskell machinery must retain this distinction rather than impose one runtime's ownership rules on the other.

### Validation and privacy

Python's raw structured-output mode checks for an object but does not perform complete local JSON Schema validation; typed output performs model validation (`_high_level/output.py:45–122`). Documentation must state the corresponding Haskell guarantees precisely.

The Python receive path admits JSON arrays before object dispatch, and one null-ID error-logging path assumes an object error payload (`transport.py:268–275`, `protocol.py:466–515`). These are defects, not compatibility requirements. Haskell boundary validation must reject malformed frames without crashing the dispatcher or exposing payloads in diagnostics.

## Evidence to date

- Both SDK checkout revisions, the published npm archive and all four schema fingerprints were verified; the reference audit and five regression checks pass.
- Connection-readiness/shared-runtime integration on macOS ARM64/GHC 9.10.3: warning-free all-component build and **3,679 tests passing in 12.10s**; full formatting/lint, five audits/reference integrity, eleven source checks and a compiled-only example pass. See [readiness delivery and full fess](development.md#connection-readiness-delivery).
- Historical retry/shared-connection boundary: **3,654 tests passed in 11.12s**, with ten source checks and a compiled-only example. Its eighteen retry cases remain in the current suite; see [retry delivery](development.md#connection-retry-delivery).
- Historical binary tunnel/shared-WebSocket boundary: **3,636 tests passed in 11.28s**, with eight tunnel-class and eight shared-auth source checks and its compiled example. The fourteen tunnel cases remain in the current suite; see [binary tunnel delivery](development.md#binary-relay-tunnel-delivery).
- Historical JSON relay authentication boundary: **3,622 tests passed in 10.93s**, with eight source checks and a compiled-only example. Its twenty-one relay cases remain in the current suite; see [relay authentication delivery](development.md#relay-authentication-delivery).
- Historical supplied-session integration boundary: **3,601 tests passed in 11.35s**, with five source checks and a compiled-only example. Its twelve injection cases remain in the current suite; see [injection delivery](development.md#injected-session-runtime-delivery).
- Historical in-process/shared-callback boundary: **3,589 tests passed in 11.72s**, with five source checks and its compiled example. The reproduced setup-cleanup and cancellation/join defects remain documented in [in-process delivery](development.md#in-process-channel-delivery).
- Historical supplied-IPC boundary: **3,574 tests passed in 11.52s**, with five source checks and its compiled example. Its seventeen regressions remain in the current suite after callback-owner extraction; see [IPC delivery](development.md#host-supplied-ipc-delivery).
- Historical launch/environment/stderr checkpoint: **3,557 tests passed in 11.20s**, with four source checks and its compiled example. Child IPC was unimplemented at that checkpoint and was subsequently delivered through the same-PID native launcher; see the [launch checkpoint](development.md#launch-configuration-checkpoint) and [owned IPC acceptance](development.md#owned-child-ipc-implementation-and-fess-checkpoint).
- Historical ACP/shared-launcher boundary: **3,551 tests passed in 10.54s**, with two source checks and its compiled example. The pending fixture's ordering correction retained its assertions and passed 20 repetitions; the original failure remains recorded in [ACP delivery](development.md#acp-process-mode-delivery).
- Historical coordinated-state boundary: **3,545 tests passed in 10.32s**, with eight pending-source checks and its compiled example. Its real passing receipt did not expose the later-found fixture scheduling flaw; the correction and original failed full-suite receipt remain recorded. Earlier child/terminal/cwd evidence remains in the delivery records.
- Historical workspace/file increment: 3,144 tests passed. Local and daemon/external-MCP commit snapshots were separately rebuilt and passed their respective 3,005/3,092-test suites; these do not certify later changes.
- Historical REST-checkpoint Ormolu/HLint, reference audits and compiled catalog/workspace examples passed. Its Haddock link warnings and `[no-repository]` advisory did not certify later binding increments. Subsequent validator, timestamp/attribution, compiler/platform, documentation and live evidence is recorded separately above.
- The REST source distribution matched 110 packaged files at packaging time, built with `-Werror`, passed 3,125 tests and all six attachment FD/race markers. The README REST example compiled without execution. These receipts precede final verification-note updates; earlier 3,110-test stopping-point package receipts remain historical.
- Live local evidence includes the explicitly versioned CLI0.217.0 one-shot and the later 34.89-second multi-turn/interruption/closed-handle check, whose patch version was not recorded. The native daemon's seven-marker scenario passed against pinned CLI0.217.0 in32.32s. These are bounded scenarios, not OAuth or every-operation live certification.
- Current GHC9.12.4 suites pass all3,942 tests on observed macOS and GNU/Linux ARM64. A separate unpacked distribution also passes its full suite, native worker/audits/attachment checks and77 compile-only README examples. Other architecture/libc and unexecuted live operations remain outside the observed evidence; configured CI jobs alone are not execution proof.
- The Obr exporter reintroduces an EOF blank line in `PLAN.org`; `hsdk-org-export-whitespace-x4c` remains open. Whitespace checks are not disabled.
- Repository-only `docs/HANDOFF.md` and `docs/evidence/2026-09-08/README.md` provide operational recovery and preserved stopping-point receipts. They are not shipped in the Cabal archive; packaged documentation retains the functional matrix, reference manifest and scoped verification narratives.
