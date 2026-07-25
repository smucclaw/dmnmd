# DMN 1.3 fixtures

Every other `.dmn` / `.xml` file under `test/` is DMN 1.1 or 1.2, which is why
none of them ever exercised the DMN 1.3 reader. These do.

`baseline.dmn` is the known-good file: one `<decision>` holding one
`<decisionTable>`, plus the `<inputData>` nodes it refers to. Each of the others
is `baseline.dmn` plus exactly one feature that used to make the whole document
fail to unpickle, so a regression points at one thing:

| fixture | what it adds |
|---|---|
| `inputdata-variable.dmn` | `<inputData>` with `<description>` and `<variable>` children (B1) |
| `output-without-label.dmn` | `<input>`/`<output>` with no `label=`, `<output>` with no `typeRef=` (B2) |
| `default-output-entry.dmn` | `<output>` with a `<defaultOutputEntry>` (B3) |
| `output-values.dmn` | `<output>` with `<outputValues>`, `<input>` with `<inputValues>` (B3) |
| `minimal-namespaces.dmn` | declares only the DMN model namespace (B5) |
| `extra-namespace.dmn` | declares `xsi` and a vendor namespace as well, and carries foreign-namespace attributes (B5) |
| `feel-number-type.dmn` | `typeRef="number"`, the FEEL numeric type (B4) |
| `unknown-type.dmn` | `typeRef="tuple<number>"` — must warn and degrade, not crash (B4) |
| `annotations.dmn` | `<annotation>` columns and per-rule `<annotationEntry>` text |

`not-dmn13.dmn` is a DMN 1.2 file: it must be *rejected*, with a message naming
the version. Fixtures whose names start with `bad-` must be rejected too; they
exist to keep the reader from drifting into permissiveness.

Two kinds of refusal are distinguished, because they happen at different stages
and produce different messages.

*Rejected by the reader* — the document does not unpickle at all, so no table is
produced and nothing is emitted:

| fixture | why |
|---|---|
| `not-dmn13.dmn` | DMN 1.2 namespace |
| `bad-unknown-element.dmn` | an element the XSD does not allow there |
| `bad-unknown-attribute.dmn` | an attribute in DMN's own vocabulary that the XSD does not declare |
| `bad-misordered-child.dmn` | children out of schema order |
| `unsupported-drgelement.dmn` | a `<businessKnowledgeModel>`: legal DMN 1.3, but not modelled. The message must say *that*, not "this is not DMN 1.3" |

*Refused by the converter* — the document reads fine, but the table cannot be
represented faithfully, so that table is dropped with an `error` diagnostic and
`dmnmd` exits nonzero. Emitting a table that can never match, or one whose rules
have been silently widened, is worse than emitting none:

| fixture | why |
|---|---|
| `temporal-type.dmn` | `typeRef="date"`; there is no dmnmd temporal type, and reading it as a string turns every guard into a never-matching string comparison |
| `bad-rule-arity.dmn` | a rule with more `<inputEntry>` elements than the table has `<input>` columns |
| `bad-rule-no-output.dmn` | a rule with no `<outputEntry>` at all (the XSD requires at least one) |
