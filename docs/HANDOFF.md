# Droid SDK Haskell: completeness-review checkpoint

<!-- handoff-id: droid-sdk-haskell-review-2026-09-15 -->
<!-- handoff-version: 2 -->
<!-- goal-id: mu1x1cp9-b9dv6d -->

Updated after the September 15 review and user-requested stopping-point work. This is the current handoff, not an SDK completion certificate. The preceding implementation handoff is preserved at `e22b9b6:docs/HANDOFF.md` and in the archived audit input.

## Read this first

**The SDK is not strictly complete against either pinned functional target.** Concrete defects and missing integrated capabilities are established. The final comparative report remains unfinished because claim verification and final calibration are incomplete. No audit finding was repaired during this checkpoint.

The user authorized preserving the handoff, committing all outstanding work, attempting a push and writing an external roadmap. That was a temporary exception to the review's read-only boundary, not authorization to implement fixes or restart broad inventories. The review remains **2/4 steps complete**:

1. Frozen Python/TypeScript comparison — complete.
2. Pinned current Python/TypeScript comparison — complete.
3. Verify implementation, tests, documentation and release claims — partial.
4. Deliver severity-ranked findings and two verdicts — pending.

**Push is not complete.** No Git remote is configured; a repository URL was requested. Do not guess the destination, create a public repository or force-push unrelated history. Local archives are not an off-machine backup until transferred.

The comprehensive roadmap is outside the repository, as requested:

`~/dl/droid-sdk-haskell-remaining-scope-2026-09-15.md`

Run the **`fess` skill at the end of every downstream subtask**, recording findings, verification limits and next action. A parent self-audit is not an independently attested review.

## What is saved

The implementation and its earlier records are committed in a logical sequence:

| Commit | Scope | Exact staged-snapshot check |
|---|---|---|
| `319b3a0` | Shared native runtime, transports, validator, integrated session/state/resources | GHC 9.12.4 `-Werror`; 3,875 serial tests |
| `df7f7a5` | Typed REST resources | `-Werror`; 3,889 serial tests |
| `4c27114` | Native saved-session discovery | `-Werror`; 3,942 serial tests |
| `e22b9b6` | Implementation documentation, package checks and issue history | Nine Python tests, schema audit and clean package check |

These fresh commit checks ran on macOS ARM64. Rust tests and relevant formatting/lint checks also passed. The first core build exceeded a 300-second tool deadline; its partial log and successful unchanged continuation remain distinct. A discarded REST-first staging attempt required unavailable old dependency sources and was never committed. No bounds were relaxed to pass it.

The full staged whitespace check reports three defects in **unchanged upstream vendor files**. Their bytes match the original crate; project-owned files and the actual vendor regex patch pass. Do not call the full check clean.

The tracker was exported. Its sole new byte was the known trailing blank line; removing that line restored the exact pre-export `PLAN.org`. No issue history was rewritten. The generated `dist-newstyle` symlink is ignored, not committed.

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

- **FC-04:** successful local tool-policy updates are not retained for replacement replay; a later fork/load can restore older, broader restrictions. Reproduced on macOS/Linux; references retain the updates.
- **FC-09:** ordinary native launch inherits an unrelated inheritable fixture descriptor; explicit closure and reference defaults prevent it. No real secret was read.
- **FC-05:** exit/signal diagnostics collapse to `EndOfStream` instead of retaining reference exit/signal detail.
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
3. **Restore the existing objective.** Reuse `mu1x1cp9-b9dv6d` if present. If native state is absent, obtain confirmation before recreating the same objective/four-step plan with the first two steps complete. Do not revive the earlier implementation goal or the failed schema child.
4. **Pin the environment.** Use `direnv exec .` and GHC 9.12.4/base 4.21. Keep the original frozen Python 0.4.0/TypeScript 0.7.0 and cutoff-current Python 0.4.0/TypeScript 0.9.1 authorities. Do not upgrade references, tools or bounds for convenience.
5. **Continue task 3.** Review the remaining units by stable ID, using original source and corresponding historical receipts. Record verified/contradicted/unverified with explicit levels and scope; classify nonclaims. Reject duplicate/unknown/nonpending merges unless a documented correction is intended. Reuse checked source, not a new broad inventory.
6. **Reconcile and deliver task 4.** Finish the severity-ranked report, two verdicts, receipt/coverage reconciliation and limitations. An evidence-backed incomplete verdict satisfies the review; fixes require separate authorization.
7. **Run `fess` after every subtask.** Preserve actual command outcomes, gaps and scope changes. Refocus on continuation/compaction and at least hourly using a real clock; do not claim uninterrupted cadence where no check is recorded.

A suitable fresh-session instruction is:

> Read `docs/HANDOFF.md` and the external September 15 roadmap, verify the evidence capsule, and resume completeness-review goal `mu1x1cp9-b9dv6d` at task 3. Keep the original audit corpus and pinned references fixed. Do not implement fixes, introduce an unused-schema quota, launch new reviewer waves or repeat settled live checks. Finish claim accounting and the two evidence-backed verdicts. Run the `fess` skill at the end of every subtask and preserve all failures and verification limits.

If the immediate task is only to finish publication, obtain the repository URL, inspect its refs and push the completed local history normally. Resolve non-fast-forward history with the user; do not force it. No code review resumption is needed merely to provide that missing destination.
