# Droid SDK Haskell: completion program

<!-- handoff-id: droid-sdk-haskell-review-2026-09-15 -->
<!-- handoff-version: 3 -->
<!-- goal-id: mu1x1cp9-b9dv6d -->

Updated September 17 after the user authorized completing both SDK repairs and verification. This is the current handoff, not an SDK completion certificate. The read-only stopping-point handoff is preserved at `e4f4ba7:docs/HANDOFF.md`; the preceding implementation handoff remains at `e22b9b6:docs/HANDOFF.md` and in the original audit archive.

## Read this first

**The SDK is not yet complete against the pinned functional targets.** The repairs verified so far are listed below; other confirmed gaps, claim accounting and final calibration remain.

The latest request, “Continue and complete those two separate jobs remaining,” supersedes the earlier read-only/no-fixes boundary. Implement the required repairs and finish verification, working sequentially with local commits. The confirmed goal tree has twelve milestones: the two original comparisons, policy/process repairs, MCP repairs, existing-owner creation/cache/directory work, reconnect-state/SLI work and ten current-only operations are now verified. Any old read-only wording in archived goal/report text is historical; it does not override this scope amendment. Within the original four-part audit, the position remains:

1. Frozen Python/TypeScript comparison — complete.
2. Pinned current Python/TypeScript comparison — complete.
3. Verify implementation, tests, documentation and release claims — partial.
4. Deliver severity-ranked findings and two verdicts — pending.

**Do not push or publish externally.** The user explicitly cancelled pushing. Earlier records of a missing remote and failed push remain history, not an outstanding task. Local archives are not an off-machine backup until separately transferred.

The comprehensive roadmap is outside the repository, as requested:

`~/dl/droid-sdk-haskell-remaining-scope-2026-09-15.md`

Run the **`fess` skill at the end of every downstream subtask**, recording findings, verification limits and next action. A parent self-audit is not an independently attested review.

## Current repair progress

Work state and complete execution receipts: `~/Products/droid-sdk-haskell/completion-20260916/`, beginning with `state.json`. Preserve the original September 15 corpus and ledgers separately from repaired-source verification; do not make an old contradicted claim appear true by checking only the new code.

- **FC-04 / `hsdk-hzs`: repaired.** Local load intent now retains validated settings acknowledgements in ordered intake, and successor/rollback loads drain those observations before snapshotting policy. Nine new cases cover partial updates, explicit empty lists, rejection/malformed replies, rollback, reverse acknowledgements and callback-safe replacement. Initial regressions failed 7/39; final 39-case policy checks and full 3,951-test suites pass on macOS/GNU/Linux ARM64 with GHC 9.12.4 `-Werror`. Twenty repetitions of the final policy suite also pass with eight test workers. See [FC-04 evidence and fess](evidence/2026-09-16/fc04/README.md).
- **FC-09 / `hsdk-maf`: repaired.** SDK-built Droid commands request descriptor isolation; preparation inherits that default while the generic JSONL owner retains caller-supplied `CreateProcess` policy. Four native default cases failed before the correction; all seven descriptor cases and the 31-case process group now pass. Full 3,958-test suites pass on macOS/GNU/Linux ARM64, including explicit stdio and existing IPC ownership checks. See [FC-09 evidence and fess](evidence/2026-09-16/fc09/README.md).
- **FC-05 / `hsdk-rk7`: repaired.** The existing process owner now supplies structured clean/nonzero/signal exit diagnostics, with bounded EOF/write settlement, buffered-frame and framing-error precedence, and unchanged cancellation identity. Both owned IPC channels share the status. Ten of 59 focused tests failed before behavior was wired; final 3,970-test suites pass on macOS/GNU/Linux ARM64 under GHC 9.12.4 `-Werror`. Ten parallel repetitions of five selected exit/cancellation cases pass. See [FC-05 evidence and fess](evidence/2026-09-16/fc05/README.md).
- **FC-10 / `hsdk-gsr`: repaired.** Embedded resource results now validate a text-or-blob union while preserving native extensions and rejecting malformed common fields. Five of 38 hosted cases failed before the fix; fifteen new cases include actual native HTTP results and rejection controls. Full 3,985-test suites pass on macOS/GNU/Linux ARM64. The actual MCP 1.29/Zod 4.6.5 client schema accepts all six original native resource outputs. See [FC-10 evidence and fess](evidence/2026-09-16/fc10/README.md).
- **FC-11 / `hsdk-v68`: repaired.** MCP annotations validate their actual wire timestamps without inserting seconds. Three minute-form regressions failed before the fix; seventeen vectors now match the actual pinned client validator, with accepted values preserved through native HTTP. Full 4,002-test suites pass on macOS/GNU/Linux ARM64; shared RFC 3339 and terminal code remain unchanged. See [FC-11 evidence and fess](evidence/2026-09-16/fc11/README.md).
- **FC-12 / `hsdk-gh1`: repaired.** Standard backend options disable all five pinned content encodings and JSON-media assertions without rewriting `$schema`. Before-fix Rust and twelve hosted cases failed; afterward five Rust tests, 73 hosted cases and full 4,020-test suites pass on macOS/GNU/Linux ARM64. Fresh actual Python hosted results agree with both native platforms and an owned worker installation on all seven original cases. Rebuilt worker identities and unchanged notices/dependencies are recorded. See [FC-12 evidence and fess](evidence/2026-09-16/fc12/README.md).
- **FC-07 / `hsdk-laj`: existing-owner creation delivered.** `DaemonSessionOptions`, `withSessionOn` and its handlers variant reuse the existing authentication, initialization, readiness and attachment owners; legacy creation delegates to the same initializer. Fourteen new cases cover shared/concurrent scopes, exact fields and fresh credentials, duplicate/invalid admission, rejected/malformed receipts, cancellation, stale-provider fencing, early handlers, retry and future-load policy. The original public-API probe failed to compile; afterward 37 initialization checks and full 4,034-test suites pass on macOS/GNU/Linux ARM64, plus ten parallel repetitions of the fourteen new cases. Cache and provisional-state retirement are verified separately below; constructor checks alone did not establish them. See [FC-07 evidence and fess](evidence/2026-09-16/fc07/README.md).
- **FC-08 / `hsdk-0aq`: registered-session cache controls delivered.** Capacity, explicit recency, active selection and eligible removal use existing state/load/binding ownership. Retiring leases remain protected; fresh failed creation is removed only under its own generation/token, preserving prior or superseding state. Child summaries, independent load policy and terminal identity counters survive payload retirement. The original native probe freshly retained 25 snapshots versus both actual pinned TypeScript references' 20; repaired native macOS/Linux probes retain 20. Seventeen strengthened tests, full 4,051-test suites and twenty parallel repetitions pass; the real README example compiles on both platforms. This is a soft registered-cache policy, not a hard memory bound. See [FC-08 evidence and fess](evidence/2026-09-16/fc08/README.md).
- **Directory / `hsdk-dko`: delivered.** Registration, machine association, atomic directory snapshots/STM observation, machine membership/cwd work counts and scoped invalidation reuse the existing cache/readiness owners. Ten new cases cover empty/unknown values, pre-init and optimistic inherited cwd, reassignment, cache membership, observers, malformed state and cross-machine/superseding loads. Actual frozen/current reference programs agree on eight directory and four optimistic snapshots. Final 4,061-test suites and two compiled public examples pass on macOS/GNU/Linux ARM64; twenty parallel repetitions pass on macOS. The initial invalid-null-cwd fixture failure remains recorded; the wire codec was not relaxed. See [directory evidence and fess](evidence/2026-09-16/directory/README.md).
- **Computer-connect SLI / `hsdk-of9m`: delivered.** The specialized wrapper supplies UUID/trigger/start-type context, success predicates, classified failures, exact metric names/labels and explicit active-span attributes through existing observability. Generic observation and protocol exception shapes are unchanged. Anonymous RPC timeouts require explicit known method context for auth classification; no missing metadata is guessed. Twenty-eight focused tests and full 4,089-test suites pass on macOS/GNU/Linux ARM64; twenty parallel repetitions pass on macOS. Nine public cases match actual frozen/current helpers and both native platforms after validating and normalizing only UUID/duration values. Auth/private-error reference branches are static evidence plus native tests, not fresh reference execution. Probe-constructor/extension and README pragma failures remain retained. See [SLI evidence and fess](evidence/2026-09-17/sli/README.md).
- **Continuity / `hsdk-25l6`: delivered.** Scoped logical state, snapshots/STM observers and state-aware connection/controller plans retain the existing cells across sequential physical generations. Principal checks precede queued intake; old handles/reply tokens do not revive. Dispatcher callbacks and attached leases quiesce before handoff, while late caller-owned queue/submission publication is generation-fenced. A self-audit reproduced and fixed two stale-publication races despite earlier green suites. Seventeen state cases, three dispatcher-setup cases and one pure retirement case were added. Final 4,110-test suites pass on macOS/GNU/Linux ARM64; twenty parallel repetitions of 46 focused cases pass on macOS. Native controlled reconnect probes retain data; actual reference manager checks are memory-only, not reference-network reconnect execution. See [continuity evidence and fess](evidence/2026-09-17/continuity/README.md).
- **Current operations / `hsdk-116c`: delivered.** All ten CC-01 model/profile/worktree/content/workspace requests now have named typed Client and Daemon APIs. Profile preparation matches the pinned SDK's trim/UTF-16/refinement/UTC rules without changing raw wire codecs. Cleanup uses its special request budget; workspace writes do not auto-load an inactive worker. Eighty-eight controlled-peer/codec cases, full 4,198-test suites and two compiled examples pass on macOS/GNU/Linux ARM64; twenty focused parallel repetitions pass on macOS. Actual reference methods/schemas were exercised through transport capture, not a live daemon. No file/profile/worktree mutation was performed against a service. See [current-operation evidence and fess](evidence/2026-09-17/current-operations/README.md).
- **CC-06 / `hsdk-5q7i`: current ordering policy delivered.** Existing message helpers now use timestamp/ID fallback order and root cycles at their oldest member, retaining native portable UTF-16 collation and exact numbers. The current manager's separate stable multi-root merge policy is unchanged. Eight of nine new regressions failed before implementation; afterward 56 state checks and full 4,207-test suites pass on macOS/GNU/Linux ARM64. Both native platforms agree on all 25 projected comparison rows; two intentional locale-policy rows differ from recorded current TypeScript. Frozen-target and pre-existing manager differences are retained explicitly. See [CC-06 evidence and fess](evidence/2026-09-17/cc06/README.md).
- **CC-15 / `hsdk-nha6`: child-summary freshness delivered.** Captured logical-owner revisions fence late parent-load summaries per child while unrelated rows still hydrate. The existing payload store and parent epochs remain authoritative. Local summary administration and capture work without a physical connection; old connected setters keep their checks. Sixteen controlled regressions, full 4,223-test suites and two public examples pass on macOS/GNU/Linux ARM64; twenty focused parallel repetitions pass on macOS. Nine visible-state matrix rows match current reference and both native platforms. The initial partial build timeout, malformed fixture envelopes and physical-to-logical API refinement remain recorded. See [CC-15 evidence and fess](evidence/2026-09-17/cc15/README.md).
- **Terminal summaries / `hsdk-ooh6`, `hsdk-u3yb`: current policy delivered.** Explicit completion/error observations update existing registered summaries, including empty histories and untagged sessions. Last-reason metadata distinguishes expected process self-exits and clears at established new-turn boundaries. A held-load regression exposed a supporting working-state freshness gap; per-entry live revisions now preserve newer working state and reasons without discarding unrelated receipt data. Thirty-five new cases, full 4,258-test suites and one compiled public example pass on macOS/GNU/Linux ARM64; twenty final 84-case parallel repetitions pass on macOS. All 34 final summary/reason comparison rows match actual current controller/manager components and both native platforms. Reference network load races were not executed. See [terminal-summary evidence and fess](evidence/2026-09-17/summary-terminal/README.md).
- **CC-16 / `hsdk-yonw`: current ancestry policy delivered.** Existing insertion skips persisted hooks and system user-only receipts, with a conversation pointer distinct from the transcript tail. Loads, reclassification, truncation and retirement maintain that pointer; streamed placeholders and tool fallback use it. Thirty new tests, a strengthened orphan-adoption assertion, 86 focused cases and full 4,288-test suites pass on macOS/GNU/Linux ARM64; the public example compiles on both. All 142 projected snapshots across 41 cases agree with actual current manager/store components and both native platforms. Missing-tool-type and wrong-reference-signature attempts remain retained and superseded explicitly. See [CC-16 evidence and fess](evidence/2026-09-17/cc16/README.md).
- **Next:** CC-17 retained-option administration under `complete-current-state`. The combined milestone remains open at **7/12 verified milestones**; this is not an SDK completeness percentage. `hsdk-8b6` remains on native attribution; `hsdk-c9j4` tracks a source-identified message-omission retention candidate for native-contract calibration, not a confirmed defect. `hsdk-w50g` and `hsdk-g6o9` record original-claim reconciliations for the verification stage.
- Remaining milestones: current state contracts; remaining native policy decisions; original/repaired claim verification; packaged release validation; final frozen/current assessments.

The September 15 recovery kit describes the pre-repair checkpoint. Its bytes and historical receipts are not rewritten to imply that these later repairs were already present. The external roadmap remains the work inventory, with its former no-repair/push instructions superseded by this section.

## What is saved

The implementation and its earlier records are committed in a logical sequence:

| Commit | Scope | Exact staged-snapshot check |
|---|---|---|
| `319b3a0` | Shared native runtime, transports, validator, integrated session/state/resources | GHC 9.12.4 `-Werror`; 3,875 serial tests |
| `df7f7a5` | Typed REST resources | `-Werror`; 3,889 serial tests |
| `4c27114` | Native saved-session discovery | `-Werror`; 3,942 serial tests |
| `e22b9b6` | Implementation documentation, package checks and issue history | Nine Python tests, schema audit and clean package check |

These fresh commit checks ran on macOS ARM64. Rust tests and relevant formatting/lint checks also passed. The first core build exceeded a 300-second tool deadline; its partial log and successful unchanged continuation remain distinct. A discarded REST-first staging attempt required unavailable old dependency sources and was never committed. No bounds were relaxed to pass it.

The September 15 full implementation-diff whitespace check reported three defects in **unchanged upstream vendor files**. Their bytes match the original crate; project-owned files and the actual vendor regex patch passed. Do not relabel that archived full check as clean.

At the September 15 checkpoint, tracker export added only the known trailing blank line; removing it restored the pre-export `PLAN.org`. New repair issues are now recorded separately without rewriting earlier completion history. The same unrelated trailing-blank export behavior is normalized when needed, not repaired as part of SDK work. The generated `dist-newstyle` symlink remains ignored, not committed.

## Authoritative audit state

Original working evidence:

`~/Products/droid-sdk-haskell/completeness-review-20260915`

Portable evidence and recovery instructions:

[`docs/evidence/2026-09-15/README.md`](evidence/2026-09-15/README.md)

Within the recovered capsule, use these files under `review/`:

- `review-state.json` and `workspace-before.json`;
- `frozen-coverage-index.md` and the six frozen comparison ledgers;
- `current-comparison.md` and `current/` release/delta manifests;
- `verification/comments-reviewed-042.json`;
- `verification/documents-reviewed-034.json`;
- `verification/supplemental-claims.json`;
- probe reports, original/corrected sources, logs and status receipts.

The audit input HEAD was `6172dc89773c7dcb42b92b6c49ecc019e656d444`, but the reviewed working tree contained substantial uncommitted implementation. Its **440 regular files** are preserved in `halt/audit-input-source.tar.gz`, SHA-256 `823a43813b714303873b862a6efabdc83e3e5f94c35bd1f079831652d631672e`. The 441st entry is the recorded, excluded build symlink.

Use that original source for claim locations and IDs. In particular, the **35 pending HANDOFF units refer to the original handoff**, not this replacement. Do not regenerate the inventory against the administrative checkpoint and silently change the denominator.

### Exact remaining counts

- Comments: **1,307 accounted** — 1,303 verified, 3 contradicted, 1 unverified; zero pending. The seven recovered HTML comments and 2,560 unchanged-vendor exclusions are retained.
- Documents: **590 reviewed**, **159 nonclaims**, **2,214 pending** of 2,963. Reviewed dispositions: 555 verified, 10 contradicted, 25 unverified.
- Supplemental claims: 67, kept separate.

| Pending document | Units |
|---|---:|
| `docs/development.md` | 1,089 |
| `docs/parity.md` | 943 |
| `docs/exhaustive-codec-backlog.md` | 147 |
| Original `docs/HANDOFF.md` | 35 |

All main README and archived September 8 evidence documents are accounted for. The backlog's first 95 lines were inspected but not dispositioned. Its dated 317-mapped/422-missing calculation and 26-schema-file digest still need historical binding; this is not an implementation quota.

## Findings that must not be lost

- **FC-04, original release:** acknowledged local tool-policy updates were lost during replacement replay. The September 16 repair above resolves this for the new working source; preserve the original failing evidence and claim dispositions.
- **FC-09, original release:** SDK-default launches inherited an unrelated inheritable fixture descriptor. The September 16 default-constructor repair above resolves that boundary; explicitly supplied low-level descriptor policy is still honored. No real secret was read.
- **FC-05, original release:** exit/signal diagnostics collapsed to `EndOfStream`. The September 16 repair retains them at the process boundary and through the public RPC failure-cause accessor; the existing RPC request-error taxonomy remains unchanged.
- **FC-10, original release:** rich embedded resources incorrectly required constraints from both variants. The union correction above resolves this acceptance gap without changing native extension preservation.
- **FC-11, original release:** minute-only MCP annotation strings were accepted after temporary normalization. The local predicate correction above now validates the actual emitted spelling; terminal/general scalar policies are unchanged.
- **FC-12, original release:** Draft 7 content keywords altered handler admission, including under negation. The standard-options correction above now matches the intended non-asserting policy while retaining declared dialect and other validation.
- **FC-07/08 and integrated directory, original release:** public creation on an active opaque daemon owner, cache administration and machine-group administration were absent or partial. The new owner APIs above provide these operations, including guarded provisional-state retirement. The specialized SLI adapter is now separate from the unchanged generic observer. Cross-generation continuity, retained-policy administration and attribution decisions still require their remaining work. Generic RPC or unrelated application copies do not automatically satisfy them.
- TypeScript 0.9.1 adds ten actual operations without native callers and changes child-summary freshness, ancestry and ordering policies. Keep those current-only changes separate from frozen defects.
- Credit valid composition: typed protocol close, explicit-content construction and bounded dynamic retry are demonstrated. Serialized versus fail-fast turns and stricter hosted names require calibrated treatment, not names-only gap claims.

The report/ledger files contain source locations, cases and limitations. The severity ordering still needs final calibration. No unauthorized execution, hard memory/spending guarantee or exhaustive process census was established.

## Verification boundaries

Fresh review evidence includes GHC 9.12.4 `-Werror` builds and 3,942 serial tests on macOS and GNU/Linux ARM64, Rust 3/Python 9 checks, 77 compile-only README snippets and package/attachment/Haddock URL-file checks. Initial parallel macOS failures are preserved; passing focused/serial reruns do not erase them. The exact tested implementation inputs are retained.

No new live Factory/model/authentication call occurred during this review. Historical local and CLI 0.217.0 daemon/new-key successes are real, but do not prove every cloud/TLS/hosted/OAuth variant. Old 401s are not current authentication blockers. Never read, print, hash or place real credentials in arguments merely to resume this work.

Do not equate CI configuration, dependency graphs or reviewer reports with execution. Actual review Rust/Cargo tools were 1.97.x; configured Rust 1.98.1 was not executed. No x86_64, musl, Windows or hosted-CI execution is claimed.

Historical uncertainties remain explicit: the original focused 67-test daemon receipt, some early source bindings and exact command/activity/authorization evidence, the September 8 `63aa21ac…` tarball, backlog dating and the dated compiler-selection user receipt. Later passes are not substitutes for those missing originals.

## Exactly how to resume

1. **Recover and verify.** Clone the approved remote or verified Git bundle. Follow the capsule README to check and extract evidence and the original audit source. Retain failed receipts and earlier ledgers unchanged. Missing build caches are expected.
2. **Confirm authority once.** Read this handoff, the external roadmap, `review/review-state.json` and the archived `halt/recovery-assets/review-goal-snapshot.json`. Inspect live goal state once; follow the user's explicit resume instruction. The archive does not itself authorize execution or publication.
3. **Restore the existing objective.** Reuse `mu1x1cp9-b9dv6d` and its confirmed twelve-milestone repair/verification plan. If native state is absent, obtain confirmation before restoring it with the two original comparisons complete and subsequent repair progress bound to its evidence. Do not revive the earlier implementation goal or failed schema child.
4. **Pin the environment.** Use `direnv exec .` and GHC 9.12.4/base 4.21. Keep the original frozen Python 0.4.0/TypeScript 0.7.0 and cutoff-current Python 0.4.0/TypeScript 0.9.1 authorities. Do not upgrade references, tools or bounds for convenience.
5. **Continue the repair milestones.** Begin with the next open item in Current repair progress. Add discriminating regressions, repair the existing owner, verify the affected contracts and commit code, evidence and issue state together. Generic raw RPC and extra application-owned bookkeeping do not establish missing integrated behavior.
6. **Finish claims and final validation.** Review original units by stable ID against the preserved source/receipts; verify repaired promises separately. Complete the severity-ranked findings, coverage reconciliation, supported-platform/package checks and two repaired-SDK assessments. An incomplete verdict remains truthful history for the original release, but does not satisfy the newly authorized SDK-completion outcome.
7. **Run `fess` after every subtask.** Preserve actual command outcomes, gaps and scope changes. Refocus on continuation/compaction and at least hourly using a real clock; do not claim uninterrupted cadence where no check is recorded.

A suitable fresh-session instruction is:

> Read `docs/HANDOFF.md`, `~/Products/droid-sdk-haskell/completion-20260916/state.json` and the confirmed goal tasks. Continue the authorized SDK repair and verification program, starting with the next open milestone. Keep the original audit corpus and pinned references fixed. Use existing owners and public Haskell composition; do not introduce an unused-schema quota, unrelated cleanup, reviewer waves or redundant live calls. Finish all required capability, claim and release verification. Run `fess` after every subtask, preserve failures, commit locally, and do not push.

Publication is not part of the current work. Do not request a repository URL or retry the old failed push.
