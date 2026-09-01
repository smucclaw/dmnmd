# DMN 1.3 fixtures

These exercise the DMN 1.3 reader one feature at a time. `test/dmn15/` does the
same for the 1.4 and 1.5 namespaces.

> An earlier version of this paragraph said "every other `.dmn` / `.xml` file
> under `test/` is DMN 1.1 or 1.2, which is why none of them ever exercised the
> DMN 1.3 reader". That was false when it was written, not drift: of the 14
> fixtures outside this directory, **12 are DMN 1.3** — `simple.dmn`,
> `safe2.dmn`, `simulation.dmn`, `simulation-collect-hit-policy.dmn` and all
> eight under `examples/`. Only `Traffic Violation.dmn` (1.2) and
> `dish-decision.dmn11.xml` (1.1) match the claim, and `test/simple.dmn` has
> exactly one commit touching it (`dbd1d73`), which already carried the 1.3
> namespace. What is true, and was presumably meant, is that none of those
> fixtures was written to exercise the reader *feature by feature*.

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
| `is-collection.dmn` | `isCollection="true"` on an `<itemDefinition>`, and a collection `typeRef` derived from another named type |
| `decision-variable-typeref.dmn` | a `<decision>` with a `<variable typeRef>` and a single `<output>` that carries **no** `typeRef` — §8.3.2's conformant spelling for a single output. The two cells discriminate: honour the variable and the outputs are the strings `"1"`/`"2"`, ignore it and inference calls the column Number and they become `1.0`/`2.0`. Both exit 0 |
| `any-typeref.dmn` | `typeRef="Any"`, FEEL's top type — a declaration that declares no restriction, so it must infer like an absent `typeRef` rather than hit the unknown-type refusal |
| `boxed-context.dmn` | a `<decision>` whose logic is a `<context>` — one of the five DMN 1.3 boxed expressions dmnmd has never modelled. Must be refused **by name**, naming the construct, the release and the owning decision, with neither the generic "could not read this" nor hxt's `xpCheckEmptyContents` |
| `decision-service.dmn` | a `<decisionService>`. Byte-identical to `baseline.dmn` otherwise, and that identity is asserted: the element must be dropped with a warning and change nothing else |

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
| `unsupported-drgelement.dmn` | a `<businessKnowledgeModel>`: legal DMN 1.3, but not modelled. The message must say *that*, not "this is not DMN 1.3". **Contrast `decision-service.dmn`**, which is accepted — a BKM carries `<encapsulatedLogic>`, so it *is* a definition, while a decision service is only a bundle of `href`s |

*Refused by the converter* — the document reads fine, but the table cannot be
represented faithfully, so that table is dropped with an `error` diagnostic and
`dmnmd` exits nonzero. Emitting a table that can never match, or one whose rules
have been silently widened, is worse than emitting none:

| fixture | why |
|---|---|
| `temporal-type.dmn` | `typeRef="date"`; there is no dmnmd temporal type, and reading it as a string turns every guard into a never-matching string comparison |
| `bad-rule-arity.dmn` | a rule with more `<inputEntry>` elements than the table has `<input>` columns |
| `bad-rule-no-output.dmn` | a rule with no `<outputEntry>` at all (the XSD requires at least one) |
| `bad-duplicate-unique-rules.dmn` | two rules with identical `<inputEntry>` text in a table with **no** `hitPolicy` attribute — which the XSD and `ParseDMN` both default to `UNIQUE`, so the second rule can never fire (D-13) |
| `no-typeref-inferred.dmn` | an `<inputExpression>` with **no** `typeRef`, which the XSD allows — so dmnmd infers the column's type, and D-2 refuses a column whose cells disagree. This is the fixture that pins the XML reader's coupling to `inferTypes`: the cells here disagree on purpose, so the inference *refusal* is what it exercises |

> This row used to end "The only fixture here that omits `typeRef`, and so the only
> one exercising the XML reader's coupling to `inferTypes`." Both halves were false
> when written, not drift: `output-without-label.dmn` has carried
> `<output id="Output_band" name="band"/>` — no `typeRef` — since `7741ce9`, its only
> commit, so it omits `typeRef` too and its output column is inferred as well. What
> is distinctive about `no-typeref-inferred.dmn` is that its cells *disagree*, so it
> reaches the refusal rather than a quiet inference. `decision-variable-typeref.dmn`
> and `any-typeref.dmn` now omit it as well.
