# SDK parity and reference baseline

This document records existing implementation and the functional SDK parity backlog. The resumed goal covers the combined Python/TypeScript SDK capabilities, including daemon/REST functionality. Exhaustive schema-definition and codec coverage are no longer independent completion requirements; types and validation needed by required operations remain in scope.

The 1.205.0 schemas and baselined SDK implementations remain contract references. Their inventories do not require implementing unused definitions merely to reach a coverage total. Existing codecs and tests are retained.

The [deferred exhaustive codec report](exhaustive-codec-backlog.md) records all missing named representations, runtime constraints, partial-contract boundaries and a restart plan. It is an opt-in handoff, not an active functional completion gate.

## Reference provenance

`schema/manifest.json` records the immutable source revisions and schema fingerprints. The four schema documents were supplied at `/Users/johnw/Desktop/droid-json-schema`; the checked-in copies are byte-for-byte identical. The earlier Downloads path is superseded.

| Reference | Baseline | Advertised Factory protocol |
| --- | --- | --- |
| Python SDK | `0.4.0`, commit `6d06b4e613aab3990cf2ced469b5c4e80053e52e` | `1.192.0` |
| TypeScript reference checkout | commit `87b4c4c4e4fc093e10a94f1e767ed57b7cbf597e` | Not an implementation |
| Published `@factory/droid-sdk` | `0.7.0` | `1.151.0` |
| Supplied JSON Schema | Draft-07, four documents | `1.205.0` |
| Locally installed Droid | `droid --version`: `0.212.1` | Embedded constant `1.201.1` (static inspection; no live handshake) |

The TypeScript checkout contains documentation and examples, with a dependency on the published SDK. Its implementation and declarations were retrieved from the exact public npm archive, without installing or executing the package. Registry integrity was independently checked:

```text
https://registry.npmjs.org/@factory/droid-sdk/-/droid-sdk-0.7.0.tgz
sha512-v3kYE754zYDUyPgEvtC1GNYdj80GVoiTP9FPDkWNDcFhCrRVvQ0B6juZlnitGfwNLM/qIsz9PZaN79ZCnzFm2Q==
```

The extracted archive is currently available at `/tmp/droid-sdk-0.7.0-inspect/package`. This temporary path is an inspection convenience, not a build dependency. The archive contains bundled implementation files and declarations; its source maps do not provide recoverable source content. The protocol constants occur in Python `src/droid_sdk/schemas/constants.py` and the npm archive's `dist/chunk-5UXINOXG.mjs`.

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

Diagnostic limits follow JSON Schema's Unicode-character domain ([Draft-07 validation, section 6.3.1](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.6.3.1)). The older TypeScript Zod validators count UTF-16 code units instead ([Zod issue 3355](https://github.com/colinhacks/zod/issues/3355)), so some non-BMP inputs differ. This is a recorded compatibility gap, not established interoperability; see `hsdk-diagnostic-text-limits-81n`.

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

MCP record `Show` instances emit only the type name and a redaction marker, including for arbitrary extension fields. Explicit field access and JSON encoding retain complete values and must not be used as safe logging. This is not a completed observability subsystem. No credential helper, server command, callback URL or authentication exchange is executed. The older metadata/auth contracts are in TypeScript `dist/chunk-5UXINOXG.mjs:2324–2380,2993–3005,3947–3994` and Python `schemas/mcp.py:91–248`; the supplied newer `blockedByPolicy` field is retained explicitly.

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

Worker-completion progress normalization is source-dependent. Consequently, the complete progress union, progress notifications and mission snapshot remain unimplemented; the independent `MissionWorkerCompleted` notification is a different, simpler schema and does not imply that those contracts have been completed.

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

`AutomationExecutionLocation` distinguishes local and remote; targets reuse `NonEmptyText` for the ID. No identifier trimming, location default, lookup, schedule or deletion occurs. The older TypeScript input schema trims IDs before validation (`dist/chunk-5UXINOXG.mjs:5127,5199–5215`); the supplied wire schema requires nonempty strings without that normalization. Automation create/edit refinements remain pending authoritative source.

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

The baselined TypeScript contract is in `dist/chunk-5UXINOXG.mjs:4668–4813`. Its terminal dates use `z.coerce.date()`, accepting and normalizing inputs beyond the supplied string/date-time wire shape. Haskell keeps the exact timestamp spelling instead; this is not parity for the older coercive input model.

### Date-time validation boundary

`Rfc3339Timestamp` in `Schema/Primitives.hs` has a private constructor and preserves offsets, case and arbitrary fractional precision. `mkRfc3339Timestamp` checks ASCII syntax, Gregorian calendar validity and the UTC month-end position of positive leap seconds; `rfc3339TimestampText` returns the original text. Equality/ordering are textual, not instant comparisons. The `time` package supplies calendar validation rather than duplicated leap-year arithmetic.

This boundary follows [RFC 3339 sections 5.6–5.8](https://www.rfc-editor.org/rfc/rfc3339.txt) and [Draft-07 date-time format guidance](https://json-schema.org/draft-07/draft-handrews-json-schema-validation-01#rfc.section.7.3.1), with cases informed by the [JSON Schema test suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/optional/format/date-time.json). Aeson 2.2.4.1 was probed: its UTC decoder permits missing seconds/space separators but rejects lowercase separators and fractions beyond twelve digits. Consequently, it is not used as the wire-format validator.

Actual IERS leap-second occurrence dates and future negative-leap announcements are not validated. That remaining conformance work is tracked in `hsdk-w8y`; neither these timestamp tests nor the containing terminal codecs establish complete date-time format validation or full Draft-07 instance validation.

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

## Capability matrix

The matrix records implemented portions and target module boundaries, not completed SDK parity. Wire coverage comprises 94 shared, 185 local and 47 daemon named representations, plus inline types. Native process transport, scoped correlation/callbacks and 33 typed low-level operation bindings are implemented. `Factory.Droid` adds owned sessions, managed IDs, safe defaults, streams/results, validated inputs and scoped controls. Remaining rows are functional work rather than a request to exhaust the schema inventory.

Python `transport.py:82,325–365` supplies the lifecycle reference: configurable SIGTERM grace, followed by SIGKILL, including when close is cancelled. The Haskell channel makes grace an explicit microsecond argument rather than supplying the Python default of five seconds. It uses non-blocking exit polling to avoid cancelling a `waitForProcess` that may already have consumed the child status. Unlike Python's suppressed two-second post-kill timeout, final Haskell reaping defers asynchronous exceptions and depends on the kernel completing exit. Descendants and an absolute cleanup deadline remain outside this slice. See [development and verification](development.md) for the failure reproduction and stress evidence.

| Capability | Haskell destination | Verification destination |
| --- | --- | --- |
| Wire envelopes, methods, errors and metadata | `Factory.Droid.Schema.RPC`, `Factory.Droid.Schema.Metadata`, `Factory.Droid.Schema.Local`, `Factory.Droid.Client` (33 local operations implemented) | `test/RPCSpec.hs`, `test/EnvelopeSpec.hs`, `test/MetadataSpec.hs`, `test/ClientSpec.hs` |
| Optional exhaustive shared/local/daemon codec coverage | `Factory.Droid.Schema.*`; [deferred report](exhaustive-codec-backlog.md) | Separate missing-name/runtime-conformance ledger; not an active functional completion gate |
| JSONL process transport, ACP/stream-JSON-RPC process modes, trusted environment and custom transports | `Factory.Droid.Transport.Process` (channel and direct-child shutdown only) | `test/ProcessSpec.hs`: framing, grace/escalation, exception identity, cancellation and reaping; other transport capabilities pending |
| Daemon WebSocket, relay, binary tunnels and host message-channel transports | `Factory.Droid.Daemon.Transport` | Offline peers, authentication, byte framing and recovery tests |
| Request correlation, server requests and raw notifications | `Factory.Droid.Protocol` / `Protocol.Dispatch`; `requestReplyObserved` places selected reply observations on the existing queue without delaying correlation; `onDroidSessionEvent` supplies per-handle typed notifications outside turns | Channel/dispatcher tests cover first-acceptance ordering, cancellation, terminal drain, callback-issued requests and boundaries; native routing, unsubscribe, retired filtering and channel errors |
| One-shot execution and persistent local sessions | `Factory.Droid` (new/resumed scopes, one-shot helper and follow-up text prompts implemented) | `test/DroidSpec.hs`; historical live single-turn evidence in development notes |
| Session title and CLI working directory | `Factory.Droid.renameDroidSession` and `changeDroidWorkingDirectory` implemented | Native success/false/error, callback, resolved-path and unchanged-caller-directory checks |
| Complete/partial streaming, messages, usage and outcomes | `Factory.Droid.sendDroidEvents` provides complete/all-event modes, typed message/tool/hook/partial/status/usage payloads and raw unadapted inner notifications; shared result accumulation retains complete events and reported structured output. `sendDroidTurn` adapts to text; `sendPrompt` checks success | Native event order/filtering, snapshots/retractions, partial terminal text, raw/malformed payload, callback-failure and interruption tests; rich live interoperability remains unverified |
| Explicit local interruption | `Factory.Droid.interruptDroidSession` waits for submission and fences following turns; settled interruption preserves usability | Native idle/callback/retry/cancellation tests plus ordering-guard mutation checks; new live verification still required |
| Resume, fork, compaction, rewind and ownership transfer | `Factory.Droid` implements local resume, rewind-info and successor-producing controls with shared scoped connection ownership, retirement and rollback | Native lifecycle/gate/trace tests cover request draining, failures, cancellation, replay and independent sessions; live replacement and daemon ownership remain unverified |
| Settings, system prompts, spec mode, context statistics and detailed context breakdown | Typed updates/context queries, new-session prompts and `getDroidSettings` are implemented. Full initialize/load settings and `SettingsUpdatedEvent` observations feed one ordered connection view; no acknowledgement-derived state | Native startup/resume/override/reset, callback read, malformed-state recovery, successor/rollback and lifecycle checks; schema/nested validation, cancellation and duplicate/reset mutation evidence. Live settings verification remains pending |
| Worker termination and local bug reports with caller-supplied logs | `Factory.Droid.Client` | Operation, error and privacy fixtures |
| Model discovery and local saved-session discovery | `Factory.Droid.listDroidModels` for scoped queries; `Factory.Droid.Client.listModels` for low-level/sessionless use; saved-session listing remains unfinished | Native model options/availability/extension and error tests; saved-session filesystem fixtures still required |
| Images, documents and structured output | `Factory.Droid.Input` validates image/text/PDF values; `sendDroidInput*` submits them through the shared turn path. `sendDroidOutput*` requests object schemas; raw mode checks objects and typed mode uses `FromJSON`. Frame limits are explicit/configurable | Native source/size/signature/metadata/UTF-8/Base64, wire/aggregate-limit and callback tests; controlled actual-source GHCi FD/race probes. Canonical Base64 checks are stricter than Python aliases. Live attachment/output interoperability remains unverified; no full image/PDF parser, JSON Schema validator or automatic schema derivation is claimed |
| Permissions, typed actions and user questions | `Factory.Droid.Interaction`, refined `RequestPermissionResult` in `Schema.Interaction`, and new/resumed session handler entry points | Native/default/offered-choice/edit/question/failure/privacy tests; concurrent and nested-query callbacks, startup/load policy, pending-worker survival across cancellation/retirement, finalizers and reaping. Handlers are connection-scoped; live interaction verification remains pending |
| Native tool controls, skills, commands and hook events | `listDroidTools`, `listDroidCommands`, `listDroidSkills` and `setDroidSkillDisabled` provide scoped operations; tool policies use settings patches. Typed tool/hook events are exposed | Codec and operation fixtures; native hypothetical-query, independent allow flags, metadata-only delivery, callback/retired-handle and false-success checks |
| External MCP configuration, registry discovery, management and OAuth | `Factory.Droid.MCP` | MCP RPC fixtures |
| Haskell-defined MCP tools and session-owned HTTP servers | `Factory.Droid.MCP.Server` | Authenticated loopback integration tests |
| Attribution, safe logs, metrics and trace propagation | `Factory.Droid.Observability` | Privacy and metadata tests |
| Daemon session collections, search, archival and queued messages | `Factory.Droid.Daemon` | Daemon resource tests |
| Daemon workspace trust, files, terminals and default settings | `Factory.Droid.Daemon` | Daemon resource tests |
| Workspace-targeted file transfers and proxy-token resources | `Factory.Droid.Daemon` | Scope, authentication and transfer fixtures |
| Custom models, SSH keys, relay and update controls | `Factory.Droid.Daemon` | Daemon resource tests |
| Plugins, marketplaces and automations | `Factory.Droid.Daemon` | Daemon resource tests |
| Git, worktrees, semantic diffs and feedback | `Factory.Droid.Daemon` | Daemon resource tests |
| Experimental crons, missions and Software Factory resources | `Factory.Droid.Daemon` | Advanced-resource fixtures |
| Session/mission state stores, subscriptions and low-level controllers | `Factory.Droid.State` | State-transition and subscription tests |
| Optimistic submission, message-chain repair, queue review, terminal restoration and progressive message selection | `Factory.Droid.State` | State and reconstruction fixtures |
| Factory REST compute, template and session APIs | `Factory.Droid.REST` | HTTP request/response/error tests |
| Legacy query/stream capabilities, notification conversion, stream feeding and public compatibility exports | Native equivalents in the modules above | Explicit compatibility mapping and fixtures |
| Documentation, compiled examples and distribution | Cabal package and Haddock | Package/source-distribution checks |

Implementing an operation is not authorization to exercise its external effects during development. Live model calls, remote mutations, publication, and cost-bearing checks require explicit authorization.

The expanded rows follow Python `client.py:603–624,1027–1049,1197–1233,1325–1347,1524–1547`, its exported `RunStream` and legacy notification converter, and TypeScript `dist/index-D_SzTnFR.d.ts:106201–106406,108401–108537,111762–111922,112167–114985`. Low-level load coordination, external interaction dispatch, branch divergence and workspace-targeted transfer methods must be mapped independently of the smaller convenience-resource interfaces. Static inventory does not establish their runtime semantics.

## Material contract differences

### SDK language attribution: decision required

`shared.schema.json#/definitions/SdkClientMetadataSchema/properties/language` accepts only `"typescript"` and `"python"`. Python's `SdkClientMetadata` enforces the same restriction; `ClientRequestAttribution` requires this metadata when `client` is `"sdk"`. Consequently, honest `"haskell"` attribution cannot satisfy the supplied enum unchanged.

The older TypeScript package attaches a session tag containing its language and version. This does not establish support for Haskell in the newer request-attribution, subprocess-environment or daemon-authentication contracts. The local Haskell path omits unsupported optional identity fields and strips inherited upstream SDK labels rather than impersonating Python or TypeScript. This is implemented and tested locally; daemon-wide policy and interoperability remain unresolved under `hsdk-haskell-attribution-80m`.

### Runtime refinement reference: input required

The supplied schemas mark custom checks and preprocessing with `x-factory-runtime-only`, but do not supply their implementations. The older TypeScript package provides useful OAuth and managed-model checks (`dist/chunk-5UXINOXG.mjs:2657–2789`), yet its managed-model schema lacks the newer `authMode` field, and its `baseModelId` predicate depends on an older model registry. Required local system-prompt, settings and permission contracts now have explicit selected-CLI 1.201.1 implementations; they are not silently relabeled as exhaustive 1.205.0 refinement coverage. Remaining required-operation source gaps are tracked in `hsdk-runtime-refinements-tfo`; unused refinements remain optional backlog.

`RequestPermissionResultSchema` maps to private `RequestPermissionResult` plus validated construction/accessors in `Schema.Interaction`. Both older SDKs require `editedSpecContent` for `proceed_edit` and allow an empty string (TypeScript `dist/chunk-5UXINOXG.mjs:3273–3285`; Python `schemas/cli.py:1177–1199`); CLI 1.201.1 enforces the same condition near byte 204394882. The codec implements that selected runtime refinement and preserves other strict optional fields/extensions. The high-level adapter separately enforces offered-choice membership and safe fallback. This operational agreement does not assert exhaustive 1.205.0 runtime semantics.

`WorkerCompletedEntrySchema.commitId` and `.repoPath` also carry normalization markers. The older TypeScript preprocessor maps strings whose JavaScript `trim()` result is empty to absence, leaving nonblank strings unchanged (`dist/chunk-5UXINOXG.mjs:2392–2395,2505–2515`). This is not proof of the version-matched 1.205.0 contract; that progress entry and its dependent unions/snapshots remain pending the same source input.

Settings-update and list-tools autonomy/interaction fields carry invalid-value fallback markers. Automation create/edit inputs and `DaemonSaveWorktreeSetupProfileRequestParamsSchema` carry cross-field refinement markers. Those containing codecs remain pending version-matched contracts rather than inventing fallback values, resolving contradictory selections or weakening the refinements.

### Resource ownership and completion

Python closes supplied transports as well as transports it creates; injection does not imply borrowed ownership (`client.py:253–297`, `_high_level/_client.py:38–47`). Its high-level stream requires an explicit turn-completed event. The legacy response iterator also supports an idle-state fallback for older runtimes; the two contracts must not be conflated.

Node replacement operations load a successor and retire the source handle. Daemon replacement operations return successor identifiers and leave the source usable (published TypeScript reference, session replacement section). Shared Haskell machinery must retain this distinction rather than impose one runtime's ownership rules on the other.

### Validation and privacy

Python's raw structured-output mode checks for an object but does not perform complete local JSON Schema validation; typed output performs model validation (`_high_level/output.py:45–122`). Documentation must state the corresponding Haskell guarantees precisely.

The Python receive path admits JSON arrays before object dispatch, and one null-ID error-logging path assumes an object error payload (`transport.py:268–275`, `protocol.py:466–515`). These are defects, not compatibility requirements. Haskell boundary validation must reject malformed frames without crashing the dispatcher or exposing payloads in diagnostics.

## Evidence to date

- Both SDK checkout revisions and the published npm archive identity were verified.
- All four schema fingerprints and their byte-for-byte correspondence to the supplied files were verified.
- The reference audit and its five regression checks pass.
- The GHC 9.10.3 library and tests build with `-Werror`; all 2,680 Haskell checks pass. All 185 lifecycle/operation checks passed twenty repeated parallel runs.
- Ormolu, HLint and actionlint checks pass. Haddock covers the current public modules, with missing-link warnings; `cabal check` reports the absent source-repository metadata.
- The source distribution rebuilds independently and passes the Haskell and reference-audit tests. README expressions were checked in GHCi.
- macOS/Linux CI is configured for GHC 9.10.3, 9.12.4 and 9.14.1, but only local Apple Silicon macOS/GHC 9.10.3 execution has been verified. No live CLI/daemon test or completed SDK parity claim is present.
- The installed Obr exporter reintroduces a trailing blank line in `PLAN.org`, causing `git diff --check` to fail after export. The tooling issue is tracked as `hsdk-org-export-whitespace-x4c`; it is not suppressed by a whitespace-check exception.
