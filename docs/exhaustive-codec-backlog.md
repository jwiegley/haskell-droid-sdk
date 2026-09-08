# Deferred exhaustive codec backlog

<!-- report-id: exhaustive-codec-backlog -->
<!-- report-version: 1 -->
<!-- snapshot-date: 2026-09-07 -->
<!-- schema-baseline: 1.205.0 -->
<!-- status: deferred; explicit opt-in required -->

## Purpose and scope

This report preserves the remaining **exhaustive schema/codec work** for possible later resumption. It is not a completion gate for the active functional SDK goal. Required SDK operations may acquire the types and validation they need without completing this inventory. Existing codecs and tests remain in place.

The inventory targets the supplied **Factory protocol 1.205.0** corpus. It separates missing named representations, partially represented containing contracts, and unresolved validation semantics. Implementing these codecs would not, by itself, implement daemon connections, REST resources, MCP servers or other SDK operations.

**Current position:** 317 of 739 owned definitions have mapped Haskell representations; **422 named representations remain**. Separately, 84 runtime-only annotation occurrences require authoritative behavioral evidence. Those are not 84 additional missing codecs, and 422 names do not imply 422 new Haskell record types.

## 1. Snapshot and evidence

| Owner | Owned definitions | Mapped named representations | Missing named representations |
| --- | ---: | ---: | ---: |
| `shared.schema.json` | 98 | 93 | **5** |
| `droid.schema.json` | 223 | 177 | **46** |
| `daemon.schema.json` | 418 | 47 | **371** |
| **Total** | **739** | **317** | **422** |

`protocol.schema.json` indexes those same 739 definitions; it is not a fourth collection to implement. The verified corpus contains 2,209 references and 183 distinct method names. The exact missing names appear in [Appendix A](#appendix-a-complete-missing-name-ledger).

“Mapped” means an explicit record, enum, validated scalar or concrete alias is associated with the named definition and its codec implementation. It does **not** mean exhaustive field-level validation, runtime refinement conformance, cross-version compatibility or operational SDK parity has been proved. Complete aliases count; implemented inline subtrees do not automatically count as their containing definitions.

The totals were reconciled from declarations and explicit rename mappings in [the existing parity inventory](parity.md), with source inspection of aliases and JSON instances. The [refresh procedure](#7-refreshing-this-report) reproduces the name calculation and exposes the source locations for review. Lexical matching is not a substitute for that review.

### Baselines

| Reference | Fixed baseline | Protocol |
| --- | --- | --- |
| Supplied schemas | Four files in `schema/`; supplied from `/Users/johnw/Desktop/droid-json-schema` | **1.205.0** |
| Python SDK | 0.4.0, commit `6d06b4e613aab3990cf2ced469b5c4e80053e52e` | 1.192.0 |
| TypeScript reference checkout | Commit `87b4c4c4e4fc093e10a94f1e767ed57b7cbf597e` | Guides/examples, not implementation |
| Published TypeScript implementation | `@factory/droid-sdk` 0.7.0 | 1.151.0 |
| Selected local CLI | Droid 0.212.1 | 1.201.1 |

The SDK versions and the schema target are not interchangeable. Older runtime predicates are historical evidence, not authoritative 1.205.0 behavior. The actual 1.201.1 local prompt path has been exercised; that does not establish full 1.205.0 codec conformance.

[The provenance manifest](../schema/manifest.json) records these schema SHA-256 fingerprints:

| File | SHA-256 |
| --- | --- |
| `shared.schema.json` | `73c36c7f9f49a5f0de342459db97381559813a5519465cb77a6e68f25ed5ddbc` |
| `droid.schema.json` | `42217102d3b8446b1daee1ff2b906df24ddf67324a10d6d0ceb9b5bd650e1300` |
| `daemon.schema.json` | `0c3ee2b216dec8ac0838ef723a0d3da66372c50f7fce853ebd54dd03e497c40a` |
| `protocol.schema.json` | `07793ded8fbbea30e53f31858b80253f2c278320ff20a7565cd6e0a507ce9863` |

This is an **uncommitted working-tree snapshot**, not coverage attributable to the repository's current commit alone. A digest of the 26 `src/Factory/Droid/Schema/**/*.hs` files plus `src/Factory/Droid/Internal/JSON.hs` is `897f5702c8d7391dac5eddce27be5e94b014fca9fa7971ae585625d91d6d2fa7`. It hashes sorted relative path, NUL, file bytes, NUL for each file. The manifest file's own digest is `b276c32631af3d97fc724885e5654edd2b3e52b93357e73f4de811e5ce30b67e`.

The latest recorded library verification is 2,691 Haskell tests, including eleven high-level tests, and five reference-audit tests on Apple Silicon macOS/GHC 9.10.3. Build, lint, formatting and independent distribution checks are recorded in [development notes](development.md#current-local-sdk-delivery) and `/tmp/droid-local-final.z239ap7y`. This report's inventory audit was rerun; those Haskell builds were not repeated merely to produce documentation.

Some prose near the end of `docs/parity.md` predates the usable local SDK and still describes its launcher, permission policy or live verification as absent. Do not use that historical prose to infer current operational gaps. The named-codec tables, current source and development evidence serve different purposes.

## 2. What remains to be represented

### Shared: five configuration definitions

| Definition | Work remaining | Existing material to reuse |
| --- | --- | --- |
| `ManagedCustomModelSchema` | Complete managed-model configuration and its custom checks, including the registry-dependent `baseModelId` predicate | [Models](../src/Factory/Droid/Schema/Models.hs), model catalogs, strict enums, `CompactionModel`, Bedrock metadata |
| `CustomModelsSchema` | Named collection over the managed-model contract | Existing list/alias patterns; depends on managed-model completion |
| `McpOAuthOptionsSchema` | OAuth options and cross-field/URL/secret refinements | [MCP records](../src/Factory/Droid/Schema/MCP.hs); older SDK source is only historical evidence |
| `McpOAuthConfigSchema` | Complete OAuth configuration composition | OAuth options and existing MCP configuration substructures |
| `SystemPromptConfigSchema` | Complete system-prompt alternatives and preset preprocessing | Existing text/content primitives; future settings/prompt API boundary |

All five reach runtime-only annotations. A schema-shaped record alone would not close their conformance work. No credential helper, OAuth exchange, command execution or network access is implied by implementing a codec.

### Local Droid: 46 definitions

| Family | Missing names | Main work |
| --- | ---: | --- |
| Initialization, loading, settings and tool discovery | 15 | Full request/result bodies and enclosing envelopes; settings updates and their notification; context-specific fallbacks |
| MCP configuration and add-server operation | 6 | HTTP/SSE alternatives, server union/list and add-server parameters/request |
| User questions and permissions | 5 | Question request/response wrappers; permission request/response wrappers and refined result |
| Mission state and progress | 14 | Remaining progress leaves, normalized worker completion, complete progress union, state and progress notification |
| Automation creation/editing and loop input | 3 | Cross-field validation of input bodies; no scheduler implementation implied |
| Complete request/notification unions | 3 | `ClientRequestSchema`, `CliRequestOrNotificationSchema`, `SessionNotificationSchema` and their dependency closure |

The current high-level launcher deliberately projects only the initialization and event fields needed by its usable local workflow. Its successful run is not a complete codec for `InitializeSessionRequestParamsSchema`, `InitializeSessionResultSchema` or the complete session-notification union.

The fourteen local missing definitions outside runtime-marked dependency chains are candidates for independent work. The remaining 32 reach an annotated contract; that does not mean every constituent field is unknown.

### Daemon: 371 definitions

Existing named daemon coverage is limited to 14 workspace definitions, 15 terminal definitions and 18 worktree/profile definitions. The remainder spans:

- Authentication, connection state, request unions and relay/proxy-token metadata.
- Session initialization/loading, collections, search, archival, queued/optimistic messages, settings, defaults and management provenance.
- Scoped counterparts of local discovery, interaction, MCP and control operations.
- Enclosing workspace/file/terminal/worktree requests, responses and notifications whose smaller bodies already exist.
- Git status/diffs/branches, pull requests, semantic diffs and related reports.
- Automation configuration, templates, scheduling metadata, history and visuals.
- Cron scopes, payloads, records, management bodies and notifications.
- Custom models, SSH installation reports, plugins, marketplaces and update controls.
- Mission-readiness/deletion inspection and Software Factory workstream/activity/change/event/signal records and envelopes.

Of these missing daemon names, 330 do not reach an `x-factory-runtime-only` annotation; 41 do. Absence of that marker does not waive ordinary schema, format or compatibility validation.

### Work size is not a record count

A structural classification of the 422 missing roots gives:

| Root shape | Count |
| --- | ---: |
| Method-bearing envelopes | 157 |
| Objects/maps | 142 |
| Unions, including many response wrappers | 110 |
| Other intersections | 5 |
| String domains | 6 |
| Arrays | 2 |

Classification checks for a declared method before other root shapes. It is a sizing aid, not a proposed Haskell module design. Many envelopes and response unions can be concrete aliases over [existing RPC machinery](../src/Factory/Droid/Schema/RPC.hs), as demonstrated by [Schema.Local](../src/Factory/Droid/Schema/Local.hs). Do not build a second serialization framework or a new record for every name.

## 3. Runtime behavior not supplied by JSON Schema

The corpus contains **84 annotation occurrences in 25 definitions**:

| Owner | Custom refinements | Normalizations | Invalid-value fallbacks | Total |
| --- | ---: | ---: | ---: | ---: |
| Shared | 5 | 1 | 0 | 6 |
| Local Droid | 4 | 2 | 12 | 18 |
| Daemon | 2 | 8 | 50 | 60 |
| **Total** | **11** | **11** | **62** | **84** |

Following `$ref` links, 78 definitions reach at least one of those 25 annotated definitions. All 78 are currently in the missing-name ledger: five shared, 32 local and 41 daemon. The other **344 missing names** have no such marked dependency. These are graph counts, not estimates of distinct predicates or implementation hours; copied/nested policies repeat within the corpus.

No inspected evidence establishes complete 1.205.0 runtime conformance for the marked policies. Existing enums, scalars, inline fields and sibling records do cover useful pieces. In particular, `ModelFallback` is a reported fallback record, not an implementation of the 62 invalid-input fallback occurrences. Parsing alternative valid RPC response branches is also a different kind of fallback.

An authoritative, version-matched `droid-sdk-core` implementation or behavioral contract is still needed to determine predicates, fallback values, trigger domains and preprocessing order. See `hsdk-runtime-refinements-tfo`. Do not infer behavior from annotation prose alone or relabel an older SDK predicate as current.

### 3.1 Custom refinements: eleven occurrences

Paths below are relative to `schema/`; a bare definition reference means the definition root.

| Location | Occurrences | Contract still needed |
| --- | ---: | --- |
| `shared.schema.json#/definitions/ManagedCustomModelSchema` and `/properties/baseModelId` | 2 | Managed-model cross-field checks and correct built-in-model registry predicate |
| `shared.schema.json#/definitions/McpOAuthOptionsSchema`, `/properties/clientMetadataUrl`, `/properties/clientSecret` | 3 | OAuth cross-field, URL and secret rules |
| `droid.schema.json#/definitions/AutomationCreateToolInputSchema` | 1 | Creation-time cross-field validation |
| `droid.schema.json#/definitions/AutomationEditToolInputSchema` | 1 | Edit-time cross-field validation |
| `droid.schema.json#/definitions/LoopToolInputSchema` | 1 | Complete refined loop-input contract |
| `droid.schema.json#/definitions/RequestPermissionResultSchema` | 1 | Permission-result branch requirements |
| `daemon.schema.json#/definitions/DaemonAuthenticateRequestSchema/allOf/1/properties/params` | 1 | Authentication parameter refinement |
| `daemon.schema.json#/definitions/DaemonSaveWorktreeSetupProfileRequestParamsSchema` | 1 | Save-profile cross-field validation |

Historical evidence for `proceed_edit` requires `editedSpecContent` while permitting an empty string: TypeScript `dist/chunk-5UXINOXG.mjs:3273–3285`, Python `schemas/cli.py:1177–1199`, and the selected CLI's 1.201.1 bundle agree. That agreement does not prove the complete 1.205.0 contract. Keep the existing safe cancellation policy separate from this broader result-codec obligation.

Older managed-model/OAuth checks are available in the TypeScript bundle around lines 2657–2789. The managed-model shape lacks newer `authMode`, and its model-registry predicate is version-dependent. Neither omission should be hidden behind an unconstrained placeholder or execution of an `apiKeyHelper`.

### 3.2 Normalizations: eleven occurrences

Brace lists denote one occurrence per listed property.

- `shared.schema.json#/definitions/SystemPromptConfigSchema/anyOf/1` — preset preprocessing (1).
- `droid.schema.json#/definitions/WorkerCompletedEntrySchema/properties/{commitId,repoPath}` — worker completion strings (2).
- `daemon.schema.json#/definitions/DaemonGetGitDiffResultSchema` — result preprocessing (1).
- `daemon.schema.json#/definitions/DaemonListAvailableSessionsResultSchema/properties/sessions/items/properties/mission/properties/{completedFeatures,createdAt,elapsedMs,title,totalFeatures,updatedAt,workingDirectory}` — mission summary preprocessing (7).

The older worker-completion preprocessor maps strings whose JavaScript `trim()` result is empty to absence and preserves nonblank spelling (`dist/chunk-5UXINOXG.mjs:2392–2395,2505–2515`). Verify the current policy, its ordering and exact whitespace semantics before implementing it. Generic Haskell trimming is not sufficient evidence.

The daemon Git-diff and mission-summary markers do not disclose their preprocessing algorithms. Post-normalization field shapes are not a specification of accepted raw input.

### 3.3 Invalid-value fallbacks: 62 occurrences

**Local Droid: 12**

- `droid.schema.json#/definitions/InitializeSessionRequestParamsSchema/properties/{autonomyLevel,interactionMode}` (2).
- `droid.schema.json#/definitions/ListToolsRequestParamsSchema/properties/{autonomyLevel,interactionMode}` (2).
- `droid.schema.json#/definitions/SessionSettingsSchema/properties/{autonomyLevel,availableAutonomyLevels,interactionMode}` (3).
- `droid.schema.json#/definitions/SettingsUpdatedNotificationSchema/properties/settings/properties/{autonomyLevel,availableAutonomyLevels,interactionMode}` (3).
- `droid.schema.json#/definitions/UpdateSessionSettingsRequestParamsSchema/properties/{autonomyLevel,interactionMode}` (2).

**Daemon outside the defaults-management map: 14**

- `daemon.schema.json#/definitions/DaemonGetDefaultSettingsResultSchema/properties/{autonomyLevel,availableAutonomyLevels,interactionMode,maxAutonomyLevel,resolutionChain}` (5).
- `daemon.schema.json#/definitions/DaemonGetGitDiffUnavailableResultSchema/properties/unavailableReason` (1).
- `daemon.schema.json#/definitions/DaemonInitializeSessionRequestSchema/allOf/1/properties/params/properties/{autonomyLevel,interactionMode}` (2).
- `daemon.schema.json#/definitions/DaemonUpdateSessionSettingsRequestSchema/allOf/1/properties/params/properties/{autonomyLevel,interactionMode}` (2).
- `daemon.schema.json#/definitions/DaemonListAvailableSessionsResultSchema/properties/sessions/items/properties/worktree` (1).
- `daemon.schema.json#/definitions/DaemonListOpenedSessionsResultSchema/properties/sessions/items/properties/worktree` (1).
- `daemon.schema.json#/definitions/DaemonPullRequestLookupSchema/properties/reason` (1).
- `daemon.schema.json#/definitions/DaemonPullRequestStatusSchema/anyOf/2/properties/reason` (1).

**Defaults-management map: 36**

Prefix: `daemon.schema.json#/definitions/DaemonSessionDefaultsManagementMapSchema`.

- Definition root (1).
- `/properties/{autonomyLevel,autonomyMode,cloudSessionSync,compactionModel,compactionModelMode,compactionThresholdCheckEnabled,compactionTokenLimit,compactionTokenLimitPerModel,enableOneHourAnthropicCaching,interactionMode,mission,modelId,reasoningEffort,runInWorktree,specModeModelId,specModeReasoningEffort,specSaveDir,subagent,subagentAutonomyLevel,worktreeAutoDeleteLimit,worktreeDirectory}` (21).
- `/properties/mission/properties/{orchestratorModel,orchestratorReasoningEffort,skipScrutiny,skipUserTesting,validationWorkerModel,validationWorkerReasoningEffort,workerModel,workerReasoningEffort}` (8).
- `/properties/subagent/properties/{heavyModel,heavyReasoningEffort,lightModel,lightReasoningEffort,mediumModel,mediumReasoningEffort}` (6).

Determine the replacement value, invalid-input domain, missing/null treatment and nesting/order at each annotated location. Keep standalone enum decoders strict. A fallback attached to one containing field must not weaken every use of `AutonomyLevel`, `DroidInteractionMode` or another shared type.

## 4. Existing representations that do not close the whole contract

| Existing implementation | What remains separate |
| --- | --- |
| `WithEnvelope`, `MethodRequest`, `RpcResponse` and generic JSON-RPC bodies | Missing concrete method/response aliases and complete discriminated protocol unions; generic support does not count every absent named envelope |
| `SessionSnapshot` in [Session](../src/Factory/Droid/Schema/Session.hs) | It represents the small `SessionSchema`, not initialization/loading results or full session settings |
| `LastCallTokenUsage` in [Usage](../src/Factory/Droid/Schema/Usage.hs) | Inline body only; not a separate owned definition or a completed `LoadSessionResultSchema` |
| Four inline workspace read/write/list/search parameter bodies in [Daemon.Workspace](../src/Factory/Droid/Schema/Daemon/Workspace.hs) | Their enclosing named daemon request envelopes remain missing; separately implemented result definitions do count |
| Terminal and worktree notification parameter records | The corresponding complete notification envelopes remain separate mappings |
| Question bodies/results and permission details in [Interaction](../src/Factory/Droid/Schema/Interaction.hs) | Question envelopes, permission envelopes and the refined permission result |
| `MissionPhase`, feature/handoff/worker reports and independent notifications in [Mission](../src/Factory/Droid/Schema/Mission.hs) | Full `MissionStateSchema`, progress-entry union and dependent progress notification |
| Loop interval/state in [Loop](../src/Factory/Droid/Schema/Loop.hs) | Refined `LoopToolInputSchema`; no scheduling follows from decoding |
| Closed profile contents, names/scripts and save-result bodies in [Daemon.Worktree](../src/Factory/Droid/Schema/Daemon/Worktree.hs) | The separately refined save request |
| MCP input-schema metadata and stdio/header/status records in [MCP](../src/Factory/Droid/Schema/MCP.hs) | OAuth-bearing HTTP/SSE server configurations; `McpToolInputSchema` is an inline metadata record, not a JSON Schema instance validator |
| The selected field projection in [Factory.Droid](../src/Factory/Droid.hs) | Complete 1.205.0 initialization/event codecs; the operational projection is valid for its tested workflow but not exhaustive representation coverage |

Conversely, legitimate aliases must not be mistaken for omissions. Examples include `ScriptRunResultValue = Value`, the Bedrock string map, empty/open object inputs, message types specialized over ordinary/cached content, `HostId`, `HookCommand`, and concrete method envelopes. A plain dynamic representation is appropriate when that is the schema's actual domain, not as a replacement for an unimplemented known structure.

### Residual semantic work beyond the 84 markers

1. **Date-time occurrence rules.** `Rfc3339Timestamp` checks grammar, Gregorian dates, numeric offsets, arbitrary fractional precision and UTC month-end placement of second 60 while preserving spelling. It does not verify actual IERS leap-second occurrence or future negative-leap rules. See `hsdk-w8y`, [Primitives](../src/Factory/Droid/Schema/Primitives.hs) and [TimestampSpec](../test/TimestampSpec.hs). Decide and test the intended format-assertion policy before claiming complete date-time conformance. Do not impose date-time validation on fields declared merely as strings.
2. **Unicode compatibility.** `BoundedText` already enforces JSON Schema code-point lengths, including non-BMP boundary tests. Older TypeScript/Zod diagnostics count UTF-16 code units. Reconcile that version-specific acceptance difference without silently truncating values or tightening the canonical wire type. See `hsdk-diagnostic-text-limits-81n`, [SourcesSpec](../test/SourcesSpec.hs) and [WorktreeSpec](../test/WorktreeSpec.hs).
3. **Cross-version presence and domains.** Model catalogs, numeric usage/context values, content/message fields, RPC overlaps and MCP inputs differ across the supplied schema and older SDKs. Examples include Python integer/null domains, TypeScript defaults for omitted MCP args/env, Python trimming/extension rejection, and fields introduced after the older TypeScript baseline. Record which contract each codec implements; add an explicit adapter only if a chosen compatibility target requires it.
4. **SDK identity is a policy issue, not a missing language codec.** The supplied language enum permits only Python and TypeScript; [Metadata](../src/Factory/Droid/Schema/Metadata.hs) represents it correctly. The local Haskell launcher now omits unsupported SDK identity and strips inherited upstream SDK labels, and that path has been exercised. Broader daemon/authentication attribution still needs its own evidence. Do not add an undeclared language literal or claim the local policy is still unimplemented merely because historical prose says so.
5. **Conformance of represented definitions.** The 317-name ledger does not prove every accepted input and encoded output matches every applicable Draft-07 keyword, default, union overlap, format and extension rule. Retain existing fixtures and smart-constructor/coercion regressions, but review transitive semantic closure before marking a represented definition exhaustively complete.

## 5. Suggested restart plan

The following plan applies **only after an explicit decision to resume exhaustive codec work**. It does not replace the active functional SDK plan.

| Phase | Work | Exit evidence | Relative effort / dependencies |
| --- | --- | --- | --- |
| 0. Re-establish the target | Confirm whether the objective is the immutable 1.205.0 corpus alone or separately specified versioned input contracts. Refresh this inventory against code added by functional SDK work. | Agreed codec-only scope, verified fingerprints, reviewed mapped/missing sets and source snapshot | Small; perform before using these counts as current |
| 1. Obtain runtime contracts | Acquire version-matched source or authoritative predicates, normalizations and fallbacks. Record exact source revision and schema pointer. | An evidence ledger for the 84 occurrences, including repetitions and evaluation order; unresolved entries remain explicit | Externally dependent; no defensible completion date while this source is unavailable |
| 2. Complete independent representations | Work through missing leaves, then records/unions and wrappers with no marked dependencies. Prefer already implemented bodies and reusable envelopes. | Named mappings, codec instances/aliases and independent positive/negative fixtures for each completed family | Largest known-volume lane: 344 missing names, chiefly daemon definitions; can proceed independently of phase 1 after opt-in |
| 3. Close source-sensitive families | Implement proven shared configuration contracts, settings, refined inputs, normalized progress and dependent containers. | Field-level runtime fixtures and transitive closure for the 78 affected definitions | High uncertainty; depends on phase 1, with reusable shapes from phase 2 |
| 4. Reconcile semantic differences | Resolve date-time occurrence policy and selected cross-version Unicode/presence/domain differences. Audit already represented contracts for the same issues. | Explicit contract decisions, reference-derived fixtures and compatibility boundaries | Medium/high; not proportional to the number of types |
| 5. Finish the ledger and package evidence | Complete named unions/envelopes, reconcile every claimed mapping, review comments/docs, and run the package checks once the chosen slice is stable. | No unexplained codec gaps under the agreed exhaustive target; passing compiler/tests/lint/Haddock/package/source-distribution checks with remaining advisories stated | Depends on prior phases; operational SDK and live-platform verification remain separately scoped |

**Smallest useful first slice:** missing request/response wrappers around already represented bodies, such as question or workspace/terminal contracts. Verify the actual enclosing schema before assuming an alias is enough. The [existing local aliases](../src/Factory/Droid/Schema/Local.hs) are the model to reuse.

**Highest-leverage source questions:** MCP OAuth options are reachable from 29 definitions including themselves; OAuth configuration from 26; system-prompt configuration from 11; normalized worker completion from 10; permission results from four. These dependency counts overlap and must not be summed. They identify where authoritative source could unlock several containing representations at once.

### Effort and uncertainty

No reliable hours-to-completion estimate follows from 422 names. The missing set contains many cheap aliases, large settings/resource records, and a smaller group of source-blocked semantics. Earlier session time also includes operational transport work, repeated verification and research, so extrapolating it linearly would be misleading.

Treat phase 2 as substantial but bounded implementation work and phases 1/3/4 as the main schedule risks. After one representative wrapper family and one substantial record family are completed, use measured changed-code/test effort to estimate the remaining annotation-free families. Do not assign a completion date to unavailable runtime contracts.

## 6. Testing, cleanup and eventual sign-off

Retain [SchemaTest](../test/SchemaTest.hs) and the existing record/enum helpers. The reference audit verifies fingerprints, references, associations and inventory counts; it is **not** a Draft-07 instance validator. Schema-backed round trips alone can allow an encoder and decoder to share the same defect.

For each newly completed or re-audited contract:

- Check required, optional and required-nullable fields independently; cover absent/null/present and wrong JSON types.
- Check literal/discriminator membership, unknown enum values, union overlap and branch precedence without confusing valid wire shape with operational success.
- Cover declared numeric precision/bounds, string length/regex/format constraints, non-BMP and whitespace edges, collection bounds, references and nested intersections.
- Preserve open/closed object policy, reserved-key ownership and typed-field precedence over extension fields. Exercise `toJSON` and `toEncoding` as well as decoding.
- For each runtime policy, use authoritative raw-input → normalized/fallback-output and rejection fixtures. Include cross-field combinations and evaluation order; do not invent fallbacks.
- Reuse exact-number values, private validated wrappers and nominal-role protections. Retain the existing Unicode, timestamp and coercion regressions rather than reopening solved defects.
- Keep secrets out of fixtures and logs. Record fields and explicit JSON may still be sensitive despite redacted `Show`. Provider signatures remain opaque protocol data; no new cryptographic verification is implied.
- Review Haddock and mapping prose for claims stronger than their tests. Replace historical blanket claims with version- and evidence-specific statements.

A future exhaustive sign-off should establish a mapping and evidence for every owned definition under the chosen baseline, including aliases and transitive dependencies, and close or explicitly resolve every relevant runtime/format discrepancy. It should not infer daemon/REST behavior, resource ownership, credential exchange, scheduling or successful SDK operations from completed serializers.

Existing tracking references:

| Issue | Relevance |
| --- | --- |
| `hsdk-types-codecs-2rb` | Historical codec umbrella and previous coverage checkpoints; this report supersedes its old exhaustive queue as the deferred handoff |
| `hsdk-runtime-refinements-tfo` | Missing version-matched refinement/normalization/fallback source |
| `hsdk-diagnostic-text-limits-81n` | Code-point versus older UTF-16 diagnostic limits |
| `hsdk-w8y` | Leap-second occurrence policy |
| `hsdk-protocol-baselines-wvf` | Distinct SDK/schema/CLI baselines |
| `hsdk-haskell-attribution-80m` | Broader identity policy; not a missing enum codec |
| `hsdk-documentation-diagnostics-jvc` | Haddock links and repository metadata advisories |

## 7. Refreshing this report

Run the immutable-reference audit first:

```sh
python3 -B scripts/reference_schemas.py
python3 -B scripts/reference_schemas.py --inventory
python3 -B -m unittest discover -s scripts -p 'test_*.py' -v
```

The following read-only calculation reproduces the name ledger. Run from the repository root. It uses explicit documentation renames plus real declarations; **inspect their aliases and JSON instances before accepting a mapping**. It does not establish field-level conformance and must not become an automatic coverage badge.

```sh
python3 -B - <<'PY'
import hashlib
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, 'scripts')
import reference_schemas as audit

documents = audit.load_verified(Path('schema'))
inventory = audit.inventory(documents)
declarations = {}
codec_files = sorted(Path('src/Factory/Droid/Schema').rglob('*.hs'))
for path in codec_files:
    source = path.read_text()
    for match in re.finditer(r'^(?:data|newtype|type)\s+(\w+)', source, re.M):
        if match[1] != 'role':
            declarations[match[1]] = str(path)
renames = {}
for line in Path('docs/parity.md').read_text().splitlines():
    match = re.match(r'\| `([^`]+Schema)` \| (.*?) \|', line)
    if match:
        renames[match[1]] = re.findall(r'`([^`]+)`', match[2])

for owner in ('shared', 'droid', 'daemon'):
    definitions = documents[f'{owner}.schema.json']['definitions']
    represented = {}
    for name in definitions:
        candidates = [name.removesuffix('Schema')] + renames.get(name, [])
        target = next((t for t in candidates if t in declarations), None)
        if target is not None:
            represented[name] = target
    missing = sorted(set(definitions) - represented.keys())
    print(owner, 'mapped', len(represented), 'missing', len(missing))
    for name, target in sorted(represented.items()):
        print('MAPPED', name, '=>', target, declarations[target])
    for name in missing:
        print('MISSING', name)

for entry in inventory['definitions']:
    for annotation in entry['runtimeConstraints']:
        print('RUNTIME', entry['reference'] + annotation['pointer'],
              annotation['constraint'])

digest = hashlib.sha256()
for path in sorted(codec_files + [Path('src/Factory/Droid/Internal/JSON.hs')]):
    digest.update(str(path).encode() + b'\0' + path.read_bytes() + b'\0')
print('Codec source digest:', digest.hexdigest())
PY
```

For the marked dependency count, walk `$ref` edges between owning definitions, preserving the referenced owner file and taking the definition name before any deeper JSON-pointer suffix. Count a definition once when its reachable set includes a directly annotated definition. Do not count `protocol.schema.json` index entries as additional definitions. At this snapshot the result is 78, with no intersection with the 317 mapped roots.

When updating:

1. Refresh the date, source digest and baseline fingerprints; distinguish changed code from a changed schema target.
2. Reconcile explicit mappings with actual instances/aliases, not merely matching names or schema mentions in tests.
3. Regenerate Appendix A, deduplicate names, and verify membership/counts against all three owner files. Preserve owner-qualified names such as `droid.McpServerConfigSchema` and `daemon.McpServerConfigSchema` exactly.
4. Recompute direct markers and their dependency closure; update evidence status separately from representation counts.
5. Mark partial subtrees explicitly. Remove a missing containing name only after its complete mapping and required validation exist.
6. Link new authoritative source and regression evidence. Keep source-blocked or unverified behavior visible.
7. Do not automatically reintroduce this deferred inventory as a functional SDK completion gate.

## Appendix A. Complete missing-name ledger

Each name below is relative to `#/definitions/` in its indicated owner file. These are exact schema keys, not proposed Haskell type names. “Missing” means no mapped complete named representation was found; reusable substructures may already exist.

### Shared — 5

<!-- BEGIN MISSING shared.schema.json -->
```text
CustomModelsSchema
ManagedCustomModelSchema
McpOAuthConfigSchema
McpOAuthOptionsSchema
SystemPromptConfigSchema
```
<!-- END MISSING shared.schema.json -->

### Local Droid — 46

<!-- BEGIN MISSING droid.schema.json -->
```text
AddMcpServerRequestParamsSchema
AddMcpServerRequestSchema
AskUserRequestSchema
AskUserResponseSchema
AutomationCreateToolInputSchema
AutomationEditToolInputSchema
CliRequestOrNotificationSchema
ClientRequestSchema
HandoffItemsDismissedEntrySchema
HttpMcpSchema
InitializeSessionRequestParamsSchema
InitializeSessionRequestSchema
InitializeSessionResponseSchema
InitializeSessionResultSchema
ListToolsRequestParamsSchema
ListToolsRequestSchema
ListToolsResponseSchema
LoadSessionRequestParamsSchema
LoadSessionRequestSchema
LoadSessionResponseSchema
LoadSessionResultSchema
LoopToolInputSchema
McpServersSchema
MilestoneValidationTriggeredEntrySchema
MissionAcceptedEntrySchema
MissionPausedEntrySchema
MissionProgressEntryNotificationSchema
MissionResumedEntrySchema
MissionRunStartedEntrySchema
MissionStateSchema
ProgressLogEntrySchema
RequestPermissionRequestSchema
RequestPermissionResponseSchema
RequestPermissionResultSchema
SessionNotificationSchema
SessionSettingsSchema
SettingsUpdatedNotificationSchema
SseMcpSchema
UpdateSessionSettingsRequestParamsSchema
UpdateSessionSettingsRequestSchema
WorkerCompletedEntrySchema
WorkerFailedEntrySchema
WorkerPausedEntrySchema
WorkerSelectedFeatureEntrySchema
WorkerStartedEntrySchema
droid.McpServerConfigSchema
```
<!-- END MISSING droid.schema.json -->

### Daemon — 371

<details>
<summary>Expand all 371 daemon definition names</summary>

<!-- BEGIN MISSING daemon.schema.json -->
```text
AutomationPendingSetupSchema
AutomationPrivacyLevelSchema
AutomationScaffoldSkillFileSchema
AutomationScaffoldSkillSchema
AutomationTemplateIdSchema
CloseTerminalRequestSchema
CreateTerminalRequestSchema
CreateTerminalResponseSchema
CronCreateRootScopeSchema
CronCreateScopeSchema
CronCreateSessionScopeSchema
CronKindSchema
CronPayloadSchema
CronRecordSchema
CronRootScopeSchema
CronScopeSchema
CronSessionScopeSchema
CronStatusSchema
DaemonAcknowledgeMissionReadinessWarningRequestSchema
DaemonAddMarketplaceRequestSchema
DaemonAddMarketplaceResponseSchema
DaemonAddMarketplaceResultSchema
DaemonAddMcpServerRequestSchema
DaemonAddUserMessageRequestSchema
DaemonAddUserMessageResponseSchema
DaemonApplyAutomationConfigReasonSchema
DaemonApplyAutomationConfigRequestSchema
DaemonApplyAutomationConfigResponseSchema
DaemonApplyAutomationConfigResultSchema
DaemonArchiveSessionRequestSchema
DaemonArchiveSessionResponseSchema
DaemonArchiveSessionResultSchema
DaemonAskUserResponseSchema
DaemonAskUserResultSchema
DaemonAskUserSchema
DaemonAuthenticateMcpServerRequestSchema
DaemonAuthenticateRequestSchema
DaemonAuthenticateResponseSchema
DaemonCancelMcpAuthRequestSchema
DaemonChangeWorkingDirectoryRequestSchema
DaemonCheckFolderTrustRequestSchema
DaemonCheckFolderTrustResponseSchema
DaemonCheckoutGitBranchConflictDataSchema
DaemonCheckoutGitBranchRequestParamsSchema
DaemonCheckoutGitBranchRequestSchema
DaemonCheckoutGitBranchResponseSchema
DaemonCheckoutGitBranchResultSchema
DaemonCleanupWorktreeRequestSchema
DaemonCleanupWorktreeResponseSchema
DaemonClearMcpAuthRequestSchema
DaemonCloseSessionRequestSchema
DaemonCloseTerminalRequestSchema
DaemonCompactSessionRequestSchema
DaemonConnectionStatusNotificationSchema
DaemonCreateAutomationRequestSchema
DaemonCreateAutomationResponseSchema
DaemonCreateAutomationResultSchema
DaemonCreateCronRequestParamsSchema
DaemonCreateCronRequestSchema
DaemonCreateCronResponseSchema
DaemonCreateCronResultSchema
DaemonCreatePRRequestSchema
DaemonCreatePRResponseSchema
DaemonCreatePRResultSchema
DaemonCreateTerminalRequestSchema
DaemonCronStateChangedNotificationParamsSchema
DaemonCronStateChangedNotificationSchema
DaemonCustomModelSummarySchema
DaemonDeleteAutomationRequestSchema
DaemonDeleteCronRequestParamsSchema
DaemonDeleteCronRequestSchema
DaemonDeleteCronResponseSchema
DaemonDeleteCronResultSchema
DaemonDeleteCustomModelRequestSchema
DaemonDeleteCustomModelResponseSchema
DaemonDeleteWorktreeSetupProfileRequestSchema
DaemonExecuteRewindRequestSchema
DaemonExecuteRewindResponseSchema
DaemonExecuteRewindResultSchema
DaemonForkAutomationRequestSchema
DaemonForkAutomationResponseSchema
DaemonForkAutomationResultSchema
DaemonForkSessionRequestSchema
DaemonForkSessionResponseSchema
DaemonForkSessionResultSchema
DaemonGenerateSemanticDiffRequestSchema
DaemonGenerateSemanticDiffResponseSchema
DaemonGenerateSemanticDiffResultSchema
DaemonGetAutomationHistoryRequestSchema
DaemonGetAutomationHistoryResponseSchema
DaemonGetAutomationHistoryResultSchema
DaemonGetAutomationVisualRequestSchema
DaemonGetAutomationVisualResponseSchema
DaemonGetAutomationVisualResultSchema
DaemonGetContextBreakdownRequestSchema
DaemonGetDefaultSettingsRequestSchema
DaemonGetDefaultSettingsResponseSchema
DaemonGetDefaultSettingsResultSchema
DaemonGetGitBranchDivergenceRequestParamsSchema
DaemonGetGitBranchDivergenceRequestSchema
DaemonGetGitBranchDivergenceResponseSchema
DaemonGetGitBranchDivergenceResultSchema
DaemonGetGitDiffDataSchema
DaemonGetGitDiffRequestSchema
DaemonGetGitDiffResponseSchema
DaemonGetGitDiffResultSchema
DaemonGetGitDiffSuccessResultSchema
DaemonGetGitDiffUnavailableResultSchema
DaemonGetMcpConfigRequestSchema
DaemonGetMcpConfigResponseSchema
DaemonGetMcpConfigResultSchema
DaemonGetProxyTokenRequestSchema
DaemonGetProxyTokenResponseSchema
DaemonGetProxyTokenResultSchema
DaemonGetRewindInfoRequestSchema
DaemonGetSemanticDiffCacheRequestSchema
DaemonGetSemanticDiffCacheResponseSchema
DaemonGetSemanticDiffCacheResultSchema
DaemonGetSessionMessagesRequestSchema
DaemonGetSessionMessagesResponseSchema
DaemonGetSessionMessagesResultSchema
DaemonGetWorkspaceFileContentRequestSchema
DaemonGetWorkspaceFileContentResponseSchema
DaemonGitCommitRequestSchema
DaemonGitPushRequestSchema
DaemonHoldSessionCronsRequestParamsSchema
DaemonHoldSessionCronsRequestSchema
DaemonHoldSessionCronsResponseSchema
DaemonHoldSessionCronsResultSchema
DaemonInitializeSessionRequestSchema
DaemonInspectMissionReadinessRequestParamsSchema
DaemonInspectMissionReadinessRequestSchema
DaemonInspectMissionReadinessResponseSchema
DaemonInspectMissionReadinessResultSchema
DaemonInspectWorktreeDeletionRequestParamsSchema
DaemonInspectWorktreeDeletionRequestSchema
DaemonInspectWorktreeDeletionResponseSchema
DaemonInspectWorktreeDeletionResultSchema
DaemonInstallPluginRequestSchema
DaemonInstallPluginResponseSchema
DaemonInstallPluginResultSchema
DaemonInstallSshKeyRequestSchema
DaemonInstallSshKeyResponseSchema
DaemonInstallSshKeyResultSchema
DaemonInterruptSessionRequestSchema
DaemonKillWorkerSessionRequestSchema
DaemonListAutomationsRequestSchema
DaemonListAutomationsResponseSchema
DaemonListAutomationsResultSchema
DaemonListAvailablePluginsRequestSchema
DaemonListAvailablePluginsResponseSchema
DaemonListAvailablePluginsResultSchema
DaemonListAvailableSessionsFilterSchema
DaemonListAvailableSessionsRequestSchema
DaemonListAvailableSessionsResponseSchema
DaemonListAvailableSessionsResultSchema
DaemonListCommandsRequestSchema
DaemonListCommandsResponseSchema
DaemonListCommandsResultSchema
DaemonListCronsRequestParamsSchema
DaemonListCronsRequestSchema
DaemonListCronsResponseSchema
DaemonListCronsResultSchema
DaemonListCustomModelsRequestSchema
DaemonListCustomModelsResponseSchema
DaemonListCustomModelsResultSchema
DaemonListFilesRequestSchema
DaemonListFilesResponseSchema
DaemonListGitBranchesRequestParamsSchema
DaemonListGitBranchesRequestSchema
DaemonListGitBranchesResponseSchema
DaemonListGitBranchesResultSchema
DaemonListInstalledPluginsRequestSchema
DaemonListInstalledPluginsResponseSchema
DaemonListInstalledPluginsResultSchema
DaemonListManagedWorktreesRequestSchema
DaemonListManagedWorktreesResponseSchema
DaemonListMarketplacesRequestSchema
DaemonListMarketplacesResponseSchema
DaemonListMarketplacesResultSchema
DaemonListMcpRegistryRequestSchema
DaemonListMcpServersRequestSchema
DaemonListMcpToolsRequestSchema
DaemonListModelsRequestSchema
DaemonListOpenedSessionsRequestSchema
DaemonListOpenedSessionsResponseSchema
DaemonListOpenedSessionsResultSchema
DaemonListSkillsRequestSchema
DaemonListSkillsResponseSchema
DaemonListSkillsResultSchema
DaemonListTerminalsRequestSchema
DaemonListWorktreeSetupProfilesRequestSchema
DaemonListWorktreeSetupProfilesResponseSchema
DaemonLoadSessionRequestSchema
DaemonLoadSessionSpawnOptionsSchema
DaemonLogoutRequestSchema
DaemonOptimisticMessageNotificationParamsSchema
DaemonOptimisticMessageNotificationSchema
DaemonPauseAutomationRequestSchema
DaemonPauseAutomationResponseSchema
DaemonPauseAutomationResultSchema
DaemonPullRequestLookupSchema
DaemonPullRequestStatusResultSchema
DaemonPullRequestStatusSchema
DaemonPullRequestSubjectSchema
DaemonPullUrlToCwdFileRequestSchema
DaemonPullUrlToCwdFileResponseSchema
DaemonPushCwdFileToUrlRequestSchema
DaemonPushCwdFileToUrlResponseSchema
DaemonRelayGetStatusRequestSchema
DaemonRelayGetStatusResponseSchema
DaemonRelayGetStatusResultSchema
DaemonRelayStartRequestSchema
DaemonRelayStartResponseSchema
DaemonRelayStartResultSchema
DaemonRelayStatusChangedNotificationParamsSchema
DaemonRelayStatusChangedNotificationSchema
DaemonRelayStopRequestSchema
DaemonRelayStopResponseSchema
DaemonRelayStopResultSchema
DaemonRemoveMarketplaceRequestSchema
DaemonRemoveMcpServerRequestSchema
DaemonRenameAutomationRequestSchema
DaemonRenameSessionRequestSchema
DaemonRenameSessionResponseSchema
DaemonRequestPermissionResponseSchema
DaemonRequestPermissionResultSchema
DaemonRequestPermissionSchema
DaemonRequestSchema
DaemonResizeRequestSchema
DaemonResolvePullRequestStatusesRequestParamsSchema
DaemonResolvePullRequestStatusesRequestSchema
DaemonResolvePullRequestStatusesResponseSchema
DaemonResolvePullRequestStatusesResultSchema
DaemonResolveQueuedUserMessageRequestSchema
DaemonResolveQueuedUserMessageResponseSchema
DaemonResumeAutomationRequestSchema
DaemonResumeAutomationResponseSchema
DaemonResumeAutomationResultSchema
DaemonResumeSessionCronsRequestSchema
DaemonResumeSessionCronsResponseSchema
DaemonResumeSessionCronsResultSchema
DaemonRunAutomationRequestSchema
DaemonRunAutomationResponseSchema
DaemonRunAutomationResultSchema
DaemonSaveSemanticDiffCacheRequestSchema
DaemonSaveWorktreeSetupProfileRequestParamsSchema
DaemonSaveWorktreeSetupProfileRequestSchema
DaemonSaveWorktreeSetupProfileResponseSchema
DaemonSearchFilesRequestSchema
DaemonSearchFilesResponseSchema
DaemonSearchSessionsRequestSchema
DaemonSearchSessionsResponseSchema
DaemonSearchSessionsResultSchema
DaemonSessionArchiveStateChangedNotificationSchema
DaemonSessionDefaultsManagementMapSchema
DaemonSessionListFilterSchema
DaemonSessionNotificationParamsSchema
DaemonSessionNotificationSchema
DaemonSetPluginEnabledRequestSchema
DaemonSetSkillDisabledRequestParamsSchema
DaemonSetSkillDisabledRequestSchema
DaemonSettingsManagementInfoSchema
DaemonSetupStepProgressNotificationSchema
DaemonSfCreateWorkstreamRequestSchema
DaemonSfCreateWorkstreamResponseSchema
DaemonSfCreateWorkstreamResultSchema
DaemonSfDeleteWorkstreamRequestSchema
DaemonSfDeleteWorkstreamResponseSchema
DaemonSfDeleteWorkstreamResultSchema
DaemonSfGetWorkstreamRequestSchema
DaemonSfGetWorkstreamResponseSchema
DaemonSfGetWorkstreamResultSchema
DaemonSfHydrateWorkstreamContentRequestSchema
DaemonSfHydrateWorkstreamContentResponseSchema
DaemonSfHydrateWorkstreamContentResultSchema
DaemonSfListActivitiesRequestSchema
DaemonSfListActivitiesResponseSchema
DaemonSfListActivitiesResultSchema
DaemonSfListChangesRequestSchema
DaemonSfListChangesResponseSchema
DaemonSfListChangesResultSchema
DaemonSfListEventsRequestSchema
DaemonSfListEventsResponseSchema
DaemonSfListEventsResultSchema
DaemonSfListSignalsRequestSchema
DaemonSfListSignalsResponseSchema
DaemonSfListSignalsResultSchema
DaemonSfListWorkstreamsRequestSchema
DaemonSfListWorkstreamsResponseSchema
DaemonSfListWorkstreamsResultSchema
DaemonSfMarkEventsReadRequestSchema
DaemonSfMarkEventsReadResponseSchema
DaemonSfMarkEventsReadResultSchema
DaemonSfMarkEventsUnreadRequestSchema
DaemonSfMarkEventsUnreadResponseSchema
DaemonSfMarkEventsUnreadResultSchema
DaemonSfPublishWorkstreamContentRequestSchema
DaemonSfPublishWorkstreamContentResponseSchema
DaemonSfPublishWorkstreamContentResultSchema
DaemonSfResolveActivityReviewRequestSchema
DaemonSfResolveActivityReviewResponseSchema
DaemonSfResolveActivityReviewResultSchema
DaemonSfUpdateWorkstreamRequestSchema
DaemonSubmitBugReportRequestSchema
DaemonSubmitBugReportResponseSchema
DaemonSubmitBugReportResultSchema
DaemonSubmitMcpAuthCodeRequestSchema
DaemonSubmitMcpAuthErrorRequestSchema
DaemonToggleMcpServerRequestSchema
DaemonToggleMcpToolRequestSchema
DaemonTriggerUpdateRequestSchema
DaemonTriggerUpdateResponseSchema
DaemonTriggerUpdateResultSchema
DaemonTrustFolderRequestSchema
DaemonTrustFolderResponseSchema
DaemonUnarchiveSessionRequestSchema
DaemonUninstallPluginRequestSchema
DaemonUpdateAutomationModelRequestSchema
DaemonUpdateAutomationPrivacyRequestSchema
DaemonUpdateAutomationPromptRequestSchema
DaemonUpdateAutomationRequestSchema
DaemonUpdateAutomationScheduleRequestSchema
DaemonUpdateCronRequestParamsSchema
DaemonUpdateCronRequestSchema
DaemonUpdateCronResponseSchema
DaemonUpdateCronResultSchema
DaemonUpdateMarketplaceRequestSchema
DaemonUpdateMarketplaceResponseSchema
DaemonUpdateMarketplaceResultSchema
DaemonUpdateMcpConfigRequestParamsSchema
DaemonUpdateMcpConfigRequestSchema
DaemonUpdateMcpConfigResponseSchema
DaemonUpdateMcpConfigResultSchema
DaemonUpdatePluginRequestSchema
DaemonUpdatePluginResponseSchema
DaemonUpdatePluginResultSchema
DaemonUpdateSessionDefaultsRequestSchema
DaemonUpdateSessionDefaultsResponseSchema
DaemonUpdateSessionDefaultsResultSchema
DaemonUpdateSessionSettingsRequestSchema
DaemonUpdateSessionSettingsResponseSchema
DaemonUpsertCustomModelRequestSchema
DaemonUpsertCustomModelResultSchema
DaemonValidateWorkingDirectoryRequestSchema
DaemonValidateWorkingDirectoryResponseSchema
DaemonWarmupCacheRequestSchema
DaemonWorktreeBranchChangedNotificationSchema
DaemonWorktreeBranchPullRequestSchema
DaemonWorktreeRemovedNotificationSchema
DaemonWriteDataRequestSchema
DaemonWriteWorkspaceFileContentRequestSchema
DaemonWriteWorkspaceFileContentResponseSchema
ListTerminalsRequestSchema
ListTerminalsResponseSchema
McpServerInfoSchema
McpStdioServerConfigSchema
NewSessionPromptPayloadSchema
RelayAuthenticateResponseSchema
ResizeRequestSchema
SameSessionPromptPayloadSchema
SfActivityEntrySchema
SfChangeEntrySchema
SfEventEntrySchema
SfSignalEntrySchema
SfWorkstreamEntrySchema
SlackAutomationSessionPrivacySchema
SubagentModelSettingsSchema
TerminalRequestSchema
WriteDataRequestSchema
daemon.McpServerConfigSchema
```
<!-- END MISSING daemon.schema.json -->

</details>
