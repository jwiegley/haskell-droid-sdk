# Droid SDK Haskell: completion program

<!-- handoff-id: droid-sdk-haskell-review-2026-09-15 -->
<!-- handoff-version: 3 -->
<!-- goal-id: mu1x1cp9-b9dv6d -->

Updated September 16 after the user authorized completing both SDK repairs and verification. This is the current handoff, not an SDK completion certificate. The read-only stopping-point handoff is preserved at `e4f4ba7:docs/HANDOFF.md`; the preceding implementation handoff remains at `e22b9b6:docs/HANDOFF.md` and in the original audit archive.

## Read this first

**The SDK is not yet complete against the pinned functional targets.** FC-04, FC-09 and FC-05 have been repaired and verified below; other confirmed gaps, claim accounting and final calibration remain.

The latest request, “Continue and complete those two separate jobs remaining,” supersedes the earlier read-only/no-fixes boundary. Implement the required repairs and finish verification, working sequentially with local commits. The confirmed goal tree has twelve milestones: the two original comparisons are complete, and the first repair milestone is now verified. Any old read-only wording in archived goal/report text is historical; it does not override this scope amendment. Within the original four-part audit, the position remains:

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
- **Next:** `repair-mcp-contracts`, covering FC-10/11/12 rich-result, timestamp and content-annotation semantics. The retained-policy/descriptor/process-diagnostic milestone has its three required repairs verified.
- Remaining milestones: MCP semantics; integrated daemon creation/cache/directory; reconnect/SLI; ten current-only operations; current state contracts; remaining native policy decisions; original/repaired claim verification; packaged release validation; final frozen/current assessments.

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
- **FC-10/11/12:** hosted rich-resource union validation, MCP timestamp grammar and Draft 7 content-annotation policy differ from the evaluated reference behavior. The annotation discrepancy changes actual handler admission, including under negation.
- Integrated creation on an existing daemon owner, cache administration, retained-policy administration and specialized SLI semantics remain missing or partial in the identified contracts. Generic RPC or unrelated application copies do not automatically satisfy them.
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

> Read `docs/HANDOFF.md`, `~/Products/droid-sdk-haskell/completion-20260916/state.json` and the confirmed goal tasks. Continue the authorized SDK repair and verification program, starting with the next open policy/process item. Keep the original audit corpus and pinned references fixed. Use existing owners and public Haskell composition; do not introduce an unused-schema quota, unrelated cleanup, reviewer waves or redundant live calls. Finish all required capability, claim and release verification. Run `fess` after every subtask, preserve failures, commit locally, and do not push.

Publication is not part of the current work. Do not request a repository URL or retry the old failed push.
