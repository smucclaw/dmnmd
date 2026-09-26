# DMN 1.6 fixtures

`test/dmn15/` covers what changes from 1.3 to 1.4 and 1.5.
These cover what changes from 1.5 to 1.6, which in the schema is almost nothing and in the specification text is one thing that matters.
All of them are hand-written, for the licence reason `test/dmn15/README.md` gives.

## The measurement that admitted 1.6

DMN 1.6 is formal as OMG `formal/25-12-02`.
Its schema was fetched on 2026-09-26 and compared against 1.5's; neither is vendored, per D-4.

| file | URL | bytes | sha256 |
|---|---|---|---|
| `DMN15.xsd` | `https://www.omg.org/spec/DMN/20230324/DMN15.xsd` | 25,973 | `3ffdbc068815b0d9…` |
| `DMN16.xsd` | `https://www.omg.org/spec/DMN/20240513/DMN16.xsd` | 26,009 | `861483990d3df4c7…` |

The whole-file check is one command, and anyone can re-run it:

```
diff DMN15.xsd DMN16.xsd     # four hunks, and nothing else
```

| hunk | what changes |
|---|---|
| lines 3 and 6 | the model namespace, in `xmlns` and `targetNamespace` |
| lines 57–58 | `tDefinitions`: the defaults of `expressionLanguage` and `typeLanguage` go from `…/20230324/FEEL/` to `…/20240513/FEEL/` |
| line 477 | `tFunctionKind` gains `<xsd:enumeration value="ONNX"/>` |

The per-type check extracted every top-level declaration of each file as source text — a line opening with one tab and `<xsd:complexType`, `<xsd:simpleType` or `<xsd:element` plus a `name`, through its closing tag at the same depth — and compared the blocks byte for byte.
There are 94 in each file (50 complex types, 5 simple types, 39 elements), none added and none removed, and 92 are identical.
The two that differ are the two above.

Every other type the reader models is among the 92: `tDecisionTable`, `tInputClause`, `tOutputClause`, `tDecisionRule`, `tUnaryTests`, `tLiteralExpression`, `tRuleAnnotationClause`, `tRuleAnnotation`, `tDecision`, `tInformationItem`, `tItemDefinition`, `tDMNElement`, `tNamedElement`, `tDRGElement`, `tExpression`, `tInputData`, `tKnowledgeSource`, `tDecisionService`, `tInvocable`, `tInformationRequirement`, `tKnowledgeRequirement`, `tAuthorityRequirement`, `tDMNElementReference`, `tImport`, `tHitPolicy`, `tBuiltinAggregator` and `tDecisionTableOrientation`, along with the artifact and business-context types the reader consumes and drops.
The exception is `tDefinitions`, whose only difference is two default values the reader consumes and drops, exactly as it did from 1.3 to 1.5.

Against the vendored `xsd/DMN13.xsd` the same extraction finds 82 declarations; DMN 1.6 has twelve more and removes none.
The twelve are the seven boxed-expression complex types and their five global elements, and all twelve are already in `DMN14.xsd` (fetched the same day: the same 94 names as 1.6).
Three blocks differ from 1.3: `tDefinitions` (FEEL URI defaults), `tItemDefinition` (1.5's `<typeConstraint>`) and `tFunctionKind` (1.6's `ONNX`).

### Namespaces

| | URI | source |
|---|---|---|
| model | `https://www.omg.org/spec/DMN/20240513/MODEL/` | `DMN16.xsd` `targetNamespace` |
| DMNDI | `https://www.omg.org/spec/DMN/20230324/DMNDI/` — **1.5's** | `DMN16.xsd` imports it from `DMNDI15.xsd`; no `DMNDI16.xsd` exists (404), and the OMG's DMN 1.6 page links `DMNDI15.xsd` |
| FEEL | `https://www.omg.org/spec/DMN/20240513/FEEL/` | `DMN16.xsd` defaults; DMN 1.6 §6.3.2 |
| B-FEEL | `https://www.omg.org/spec/DMN/20240513/B-FEEL/` | DMN 1.6 clause 11; in no schema |

The spec text is not self-consistent about FEEL: §6.3.2 gives `…/20240513/FEEL/` for `expressionLanguage` and, a page later, `…/20230324/FEEL/` for `typeLanguage`, while `DMN16.xsd` defaults both to `…/20240513/FEEL/`.
dmnmd drops both attributes, so the discrepancy cannot change what it reads.

## Accepted

| fixture | what it pins |
|---|---|
| `baseline16.dmn` | `test/dmn13/baseline.dmn` in DMN 1.6 and nothing else changed; it must convert to exactly what the 1.3 baseline converts to. **Its DMNDI namespace is 1.5's.** |

That makes 1.6 the second release, after 1.4, whose DMNDI date differs from its model date, and so the second one where a date-template design would be wrong.
Setting `dmn16`'s DMNDI URI to the guess `…/20240513/DMNDI/` turns exactly two things red: the hspec example over this fixture and `policy/xml-dmn16-accepted`, its corpus copy.
The failure is a named refusal, not the generic `xpCheckEmptyContents`: the right URI is also 1.5's, so the stray-namespace scan names the diagram `(DMN 1.5 DMNDI)`.
As with the 1.4 baseline, the `<dmndi:DMNDI>` element is what makes that visible, because the pickler is `xpOption`.

## Rejected by the reader, by name

| fixture | which refusal it exercises |
|---|---|
| `bad-mixed-dmndi.dmn` | a 1.6 document whose diagram is in `…/20191111/DMNDI/`, a DMNDI namespace dmnmd reads for 1.3 and 1.4 but not for 1.6 |
| `bad-bfeel.dmn` | `expressionLanguage` set to B-FEEL on `<definitions>` |
| `bad-bfeel-entry.dmn` | the same, on one `<inputEntry>` only, located by its `<decision>` |
| `bad-onnx-function.dmn` | a `<functionDefinition kind="ONNX">`, refused by the element that carries it (and by the `<context>` that is its body) |

**B-FEEL is the one thing 1.6 adds that dmnmd would otherwise have accepted, and no schema shows it.**
It keeps FEEL's grammar and changes its meaning: where FEEL answers null, B-FEEL answers false, 0 or `""`, so `"a" != 1` is true, `sum([])` is 0, and clause 11 itself notes that a `C+` table sums differently.
It is selected only by an `expressionLanguage` URI, which is `xsd:anyURI` in every release, so a document can declare it on `<definitions>` or on any single literal expression or unary test.
The reader never acts on that attribute, so the same document in the 1.5 namespace was measured reading at exit 0 with no word about the language.
`unmodelledExpressionLanguages` refuses it by URI in the same pre-flight as `unmodelledConstructs`, and `bad-bfeel-entry.dmn` is there because a check on the root alone would pass it.
It is a deny-list of one: `expressionLanguage="python"` on a 1.3 cell still reads as FEEL, and refusing that is a ruling for another day.

**ONNX needs no refusal of its own.**
`tFunctionKind` types only the `kind` attribute of a `tFunctionDefinition`, and a `tFunctionDefinition` appears in exactly two places: the global `<functionDefinition>`, which `unmodelledConstructs` already refuses, and a `<businessKnowledgeModel>`'s `<encapsulatedLogic>`, whose BKM `readerRefusal` already refuses by name (measured with a 1.6 BKM of kind `ONNX`).
So `unmodelledConstructs` gains no `DMN 1.6` entry, and `bad-onnx-function.dmn` pins that ONNX cannot arrive silently through the first of those.

**`bad-mixed-dmndi.dmn` is the 1.6 form of `test/dmn15/bad-mixed-namespace.dmn`.**
DMNDI namespaces are not one-to-one with releases: `…/20191111/DMNDI/` serves 1.3 and 1.4, and `…/20230324/DMNDI/` serves 1.5 and 1.6.
A 1.6 document still has exactly one legal DMNDI namespace, and the refusal names the release the stray one belongs to.

The same fixture could not be written for the date-template guess `…/20240513/DMNDI/` in a document: that URI belongs to no release, so the scan cannot name it, and such a document gets the generic unpickling failure.
That is the existing behaviour for any unrecognised namespace, not something 1.6 introduced.
