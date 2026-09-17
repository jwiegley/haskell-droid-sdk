# SDK release verification and validation

## Decision and scope

**The repaired SDK meets the agreed test-backed release criteria on macOS and GNU/Linux ARM64.** The required implementation repairs, reference-test comparison and source-package validation are complete. Two non-blocking tooling findings remain below; not every auxiliary check passed.

This decision follows the user's [September 17 scope amendment](evidence/2026-09-17/release-scope.md): Python/TypeScript SDK test parity is sufficient for this release, without exhaustive historical-document verification. It is not a proof of universal behavioral equivalence, an official unpublished TypeScript-suite pass, or live-service certification.

| Comparison | Verdict |
|---|---|
| **Frozen: Python 0.4.0 + TypeScript 0.7.0** | **Meets the test-backed native-functional release criteria**, with the explicit ownership, representation and composition differences below. No unresolved release-blocking discrepancy remains in the tested scope. This is not a literal clone of every frozen policy. |
| **Cutoff-current: Python 0.4.0 + TypeScript 0.9.1** | **Meets the test-backed native-functional release criteria.** The ten current operations and required owner/state-policy deltas are implemented and tested; current-only rules remain distinguished from frozen behavior. |
| **Original audited input** | **Remains incomplete as audited.** Later repairs do not erase its reproduced defects, incorrect claims or unavailable historical evidence. |

“Current” means the releases selected at the audit cutoff, **2026-09-15T00:12:35Z**, not a moving latest-version claim. Python is pinned to `6d06b4e613aab3990cf2ced469b5c4e80053e52e`; the TypeScript guide is pinned to `87b4c4c4e4fc093e10a94f1e767ed57b7cbf597e`, with the actual 0.7.0/0.9.1 package implementations separately fingerprinted. The 44 released Python source files match the frozen source. Reference identities and executed test inputs are retained in the [test-parity record](evidence/2026-09-17/release-parity/verification.json).

## Reconciled functional findings

These rows reconcile the implementation findings, not the deferred historical-document inventory. Source paths refer to the tested source commit identified below; evidence records contain the cases, hashes and failed attempts.

### Frozen/shared target

| Findings | Final disposition | Native owner and evidence |
|---|---|---|
| FC-01: remote close ownership | Calibrated native choice; explicit typed close composition verified | [Internal.Session](../src/Factory/Droid/Internal/Session.hs), [Client](../src/Factory/Droid/Client.hs); [lifecycle evidence](evidence/2026-09-17/lifecycle-turns/README.md) |
| FC-02/03: closing SDK waits and overlapping turns | Repaired: connection closure wakes SDK waits; overlap fails before a second submission | [Internal.Session](../src/Factory/Droid/Internal/Session.hs); [lifecycle evidence](evidence/2026-09-17/lifecycle-turns/README.md) |
| FC-04: acknowledged policy retention | Repaired through existing retained policy/state ownership | [Internal.Session](../src/Factory/Droid/Internal/Session.hs); [FC-04](evidence/2026-09-16/fc04/README.md) |
| FC-05/09: exit diagnostics and descriptor isolation | Repaired; exit/signal identity and intentional low-level descriptor policy retained | [Transport.Process](../src/Factory/Droid/Transport/Process.hs); [FC-05](evidence/2026-09-16/fc05/README.md), [FC-09](evidence/2026-09-16/fc09/README.md) |
| FC-06: hosted tool-name admission | Repaired: protocol strings are not rejected by an advisory naming style | [MCP.Tool](../src/Factory/Droid/MCP/Tool.hs); [name checks](evidence/2026-09-17/mcp-names/README.md) |
| FC-07/08: existing-owner creation and cache administration | Delivered through the existing daemon owner and state | [Daemon](../src/Factory/Droid/Daemon.hs); [creation](evidence/2026-09-16/fc07/README.md), [cache](evidence/2026-09-16/fc08/README.md), [directory](evidence/2026-09-16/directory/README.md) |
| FC-10/11/12: MCP resources, timestamps and content annotations | Repaired against actual reference behavior; malformed-schema and infrastructure errors remain distinct | [MCP.Tool](../src/Factory/Droid/MCP/Tool.hs), [MCP.Validator](../src/Factory/Droid/MCP/Validator.hs), [native worker](../native/schema-validator/src/main.rs); [FC-10](evidence/2026-09-16/fc10/README.md), [FC-11](evidence/2026-09-16/fc11/README.md), [FC-12](evidence/2026-09-16/fc12/README.md) |
| Integrated SLI and reconnect continuity | Delivered with existing observability and logical/physical ownership boundaries | [Observability](../src/Factory/Droid/Observability.hs), [Connection](../src/Factory/Droid/Connection.hs), [Daemon](../src/Factory/Droid/Daemon.hs); [SLI](evidence/2026-09-17/sli/README.md), [continuity](evidence/2026-09-17/continuity/README.md) |
| Observation omissions and truthful producer defaults | Repaired without replacing authoritative loads or impersonating another SDK | [SessionState](../src/Factory/Droid/SessionState.hs), [Attribution](../src/Factory/Droid/Internal/Attribution.hs), [REST](../src/Factory/Droid/REST.hs); [omissions](evidence/2026-09-17/message-omission/README.md), [attribution](evidence/2026-09-17/attribution/README.md) |

### Current-target delta

| IDs | Final disposition |
|---|---|
| **CC-01** | All ten named Client/Daemon operations delivered: daemon models, setup-profile list/save/delete, managed-worktree list/cleanup/inspection, workstream publish/hydrate and workspace-file write. [Operations and exact-value/default checks](evidence/2026-09-17/current-operations/README.md). |
| **CC-06** | Ordering/cycle behavior repaired; the portable UTF-16 versus locale-tie choice remains explicit. [Ordering evidence](evidence/2026-09-17/cc06/README.md). |
| **CC-08, CC-15–17** | Integrated administration, per-child hydration freshness, terminal/working-state freshness, conversation ancestry and retained-option set/get/delete delivered. [Directory](evidence/2026-09-16/directory/README.md), [summary revisions](evidence/2026-09-17/cc15/README.md), [terminal state](evidence/2026-09-17/summary-terminal/README.md), [ancestry](evidence/2026-09-17/cc16/README.md), [retained options](evidence/2026-09-17/cc17/README.md). |
| **CC-09/11** | Truthful current attribution and process/terminal diagnostic policy are covered by the attribution, FC-05 and terminal-state records above. |
| **CC-02–05, CC-07/10/13/14/18** | Existing native APIs or ordinary typed composition cover rate-limit reasons, retraction, append/model/system-prompt operations, explicit content, archive force, initialization budgets, workspace metadata, readiness/ensure-running and bounded dynamic retry. See the [behavioral crosswalk](evidence/2026-09-17/release-parity/crosswalk.json), plus [ConnectionHooksSpec](../test/ConnectionHooksSpec.hs), [DaemonLoadCoordinationSpec](../test/DaemonLoadCoordinationSpec.hs), [RetrySpec](../test/RetrySpec.hs) and the public [Retry](../src/Factory/Droid/Retry.hs) owner. Raw method strings alone are not credited as delivery. |
| **CC-12** | Windows process-tree behavior is outside the confirmed macOS/Linux scope. No Windows parity claim is made. |

## Verification results

The [behavioral crosswalk](evidence/2026-09-17/release-parity/crosswalk.json) maps every collected Python module once to native assertion files and reuses 21 source-bound repair manifests. It is a test-family correspondence, not a one-for-one port of every Python assertion or a completeness percentage.

| Check | Observed result |
|---|---|
| Pinned Python upstream suite, isolated owned copy | **1,526 passed; four live examples skipped**. Live-test module excluded; three collection warnings retained. |
| Published TypeScript test suite | **Unavailable in the pinned packages/guide.** Actual pinned-runtime/component differential checks are used instead; no official-suite pass is claimed. |
| Final native reliability candidate | **4,370 passed** in three macOS 32-worker runs and two Linux eight-worker runs. No serial-only waiver. |
| Fresh source archive, macOS ARM64 | **4,370 passed**, 41.61s; warning-strict build; rebuilt/installed Rust worker; **five Rust / nine Python utility tests passed**. |
| Fresh source archive, GNU/Linux ARM64 | **4,370 passed**, 66.34s; warning-strict build; rebuilt/installed Rust worker; **five Rust / nine Python utility tests passed**. |
| Source/archive identity | All **400 files** match the source commit and remain unchanged in both extracted trees. Installation regenerates the same archive. |
| Public installation and consumer | Installed macOS SDK/launcher/worker, logical state, native IPC and reaping pass. Launcher codesign verification passes without modification. |
| Package checks | Schema fingerprints, attachment descriptor/race probes, notices, `cabal check`, Ormolu, HLint and Rust formatting pass within their recorded scopes. |

Toolchain: **GHC 9.12.4/base 4.21.2.0, Cabal 3.16.1.0, Rust 1.97.1, Cargo 1.97.0**. The reference suite used Python 3.10.21. Actual platforms are ARM64; caches are warm and owned. No x86_64, musl, Windows, hosted-CI or cold-provisioning claim is inferred.

The [reliability record](evidence/2026-09-17/release-reliability/README.md) preserves the original 28/37 failures and later cancellation-fixture failures. Corrections are in five test files, not SDK runtime code: teardown is timed after callbacks; fixture watchdog kills cannot masquerade as SDK cleanup; configured test deadlines admit startup while rejecting premature expiry; and response-wait cancellation is fenced after physical writer completion. Explicit watchdog/budget changes and two new fixture controls are documented, not hidden.

## Release artifact and source identity

[Download the tested source archive](evidence/2026-09-17/packaged-release/source/droid-sdk-0.1.0.0.tar.gz).

```text
Source commit: 690342ac915eac0b5b26e4aaf7c9c3eaf21ce183
Archive:       droid-sdk-0.1.0.0.tar.gz
Bytes:         1882153
SHA-256:       989d3cf0f5a71234aaad085ce85ebade3ecbf32ddb7d0631daad7fd3390ab893
```

[Package verification](evidence/2026-09-17/packaged-release/verification.json) binds the archive, original bytes of retained failures, worker hashes and installed consumer. Subsequent commits add evidence/reporting, not different package source. The archive is a source release, not a bundle of proprietary CLI binaries or prebuilt workers.

## Remaining findings and limits

1. **P3 — offline documentation preview fails URL validation (`hsdk-rw11`).** High-level Haddock requires uncached dependency sources offline. Direct Setup generated HTML with dependency warnings; 22,714 of 158,103 checked references contain unresolved templates. The preview is not shipped or published, and this report does not mark it passed. Proper dependency/interface configuration remains a tooling follow-up under the user-approved test-parity release scope.
2. **P3 — Cabal installation footprint (`hsdk-hhgu`).** The launcher-only command also creates example/private-fixture links; the private fixture's link is dangling although its actual `libexec` binary exists. The public launcher and installed consumer work. No link repair, signing bypass or global installation was used to obtain that result.
3. **Evidence limits, not hidden passes.** There is no accessible official TypeScript suite in the pinned artifacts and no new live Factory/model/account run. Historical live receipts remain distinct evidence, not current-package cloud/TLS/OAuth certification. Linux package/worker checks do not imply a separately executed Linux installed-library consumer.

Native differences remain deliberate: borrowed scopes require explicit remote-close composition; active caller callback IO remains caller-owned; exact numeric/extension retention and portable ordering need not reproduce JavaScript representation artifacts; current ancestry policies differ from frozen policies; document inputs use attachment composition; SDK identity remains truthfully Haskell. These boundaries are not replaced with an unqualified equality claim.

## Historical accounting and final self-audit

The original audited input archive `823a43813b714303873b862a6efabdc83e3e5f94c35bd1f079831652d631672e`, its claim IDs and previous failures remain preserved. **2,214 historical document units remain unreviewed by explicit scope choice**, not silently verified. The completed original workspace no-autoload correction also retains its previous mistaken verdict. The historical-only ordering-claim task remains outside this release gate.

Per-unit `fess` sections are parent self-audits, not independent reviewer attestations. They disclose fixture changes, failed attempts, actual command results and limits. The optional documentation failure was separated from package integrity rather than rewritten as success; installed-consumer harness errors were corrected without changing SDK behavior or relaxing `-Werror`.

All work is committed locally. **Nothing was pushed or published.**
