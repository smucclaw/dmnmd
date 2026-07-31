# DMN 1.4 and 1.5 fixtures

`test/dmn13/` covers the reader feature by feature in one release. These cover
what changes when the release does.

All of them are **hand-written**. The DMN TCK is the obvious source of real 1.5
models and is deliberately not used: its licence is genuinely unresolved (a root
`LICENSE-ASL-2.0.txt`, a README saying "creative commons
Share-Alike-With-Attribution", and GitHub's API reporting `license: null`), so
nothing is copied out of it into this repo.

## Accepted

The point of these two is that they are **not** special. Each is
`test/dmn13/baseline.dmn` with the namespaces changed and nothing else, and each
must emit output byte-identical to the 1.3 baseline on all three backends. That
identity is the assertion.

It holds because the delta is additive. `tDecisionTable`, `tInputClause`,
`tOutputClause`, `tDecisionRule`, `tUnaryTests` and `tLiteralExpression` are
byte-identical in `DMN13.xsd` and `DMN15.xsd`; nothing was removed anywhere; and
the one shared type that does differ, `tDefinitions`, differs only in the
*default values* of `expressionLanguage` and `typeLanguage`, both of which this
reader consumes and drops.

| fixture | what it pins |
|---|---|
| `baseline15.dmn` | the DMN 1.5 MODEL and DMNDI namespaces are read, and a decision table means the same thing in 1.5 as in 1.3 |
| `baseline14.dmn` | **DMN 1.4 pairs a 1.4 MODEL namespace with the 1.3 DMNDI namespace.** `DMN14.xsd` declares `xmlns=".../20211108/MODEL/"` and then imports `namespace=".../20191111/DMNDI/" schemaLocation="DMNDI13.xsd"`; the OMG ships no `DMNDI14.xsd`. So a release is two independent URIs, not a date substituted into a template — and both are `String`, so getting it wrong compiles, matches nothing, and makes the `<dmndi:DMNDI>` subtree resurface as a generic `xpCheckEmptyContents` |

Both baselines carry a real `<dmndi:DMNDI>` element, which is the only reason either
can catch a namespace mistake at all: the pickler is `xpOption`, so a fixture that
merely *declares* the prefix without using it passes whatever URI you give it. That
was true of these two as first written, and of `test/dmn13/baseline.dmn` still.

Two different mistakes are worth distinguishing, because they are caught by
different things and only one of them is 1.4-specific. Both were verified by
introducing them and watching what went red.

- Writing `relModelNS` where `relDmndiNS` was meant is caught more widely — three
  `policy/` cases fail, including the pre-existing `xml-no-decision-exits-zero`,
  because in every release `.../MODEL/` and `.../DMNDI/` differ.
- Writing `.../20211108/DMNDI/` for DMN 1.4 — the plausible guess, and the one a
  date-into-a-template design makes automatically — is caught by
  **`policy/xml-dmn14-accepted` and nothing else in the repo**. DMN 1.4 is the only
  release whose DMNDI date differs from its model date, so no other fixture can
  distinguish the two.

## Rejected by the reader, by name

Every one of these is refused **before unpickling starts**, by
`refuseUnmodelled`, naming the element, the release that introduced it, and the
`<decision>` or `<itemDefinition>` it sits under. That is the whole deliverable:
without it these documents would fail with `xpCheckEmptyContents: unprocessed
XML content`, a message that names the *parent* element and dumps eighty bytes
of the child — generic, on the very release that advertises the capability.

The five boxed expressions were added in **DMN 1.4**, not 1.5. `DMN14.xsd` and
`DMN15.xsd` have an identical set of 50 complex types; the entire 1.4 → 1.5
delta is the namespace bump plus `typeConstraint`. A 1.5 document inherits them,
and the refusal says 1.4 because that is true.

There are **five** of them, not seven. The 1.4 XSD adds seven complex types, but
`tIterator` and `tQuantified` are abstract bases and `tChildExpression` /
`tTypedChildExpression` are the types of the named children `in`, `return`,
`satisfies`, `if`, `then`, `else` and `match`. None of those four has a global
`<xsd:element>`, so `<iterator>` and `<quantified>` cannot be written in a
document at all and a fixture for either would test nothing.

| fixture | which refusal it exercises |
|---|---|
| `bad-conditional.dmn` | `<conditional>`, a boxed conditional (if / then / else), DMN 1.4 |
| `bad-for.dmn` | `<for>`, a boxed iterator (for / in / return), DMN 1.4 |
| `bad-some.dmn` | `<some>`, a boxed quantifier (some / in / satisfies), DMN 1.4 |
| `bad-every.dmn` | `<every>`, a boxed quantifier (every / in / satisfies), DMN 1.4 |
| `bad-filter.dmn` | `<filter>`, a boxed filter (in / match), DMN 1.4 |
| `bad-typeconstraint.dmn` | `<typeConstraint>` on an `<itemDefinition>` — the **only** structural change DMN 1.5 makes over 1.4 (`DMN15.xsd:239`) |
| `bad-mixed-namespace.dmn` | a 1.5 `<definitions>` whose `<decision>` subtree is in the 1.3 namespace |

Three of those rows carry more than a name.

**`bad-typeconstraint.dmn` is the sharpest case**, because the behaviour it
replaces was not a generic error but *silence*. `ItemDefinition`'s pickler
filters its children by name, and a name filter **deletes** what it does not
list, so `<typeConstraint>` never reached the empty-contents check. dmnmd
honoured `<allowedValues>` — which `domainErrors` enforces — and dropped its
narrower sibling without a word, admitting values the document forbids. That is
"accepting input and quietly giving a different answer", which `CLAUDE.md` calls
strictly worse than rejecting it. The old behaviour is recorded as it was, in
`test/corpus/cases/symptom/xml-typeconstraint-dropped-silently`. **No TCK model
uses `<typeConstraint>` at all**, so this hand-written fixture is the only
coverage that path has anywhere.

**`bad-mixed-namespace.dmn` is the one fixture that is not about a construct.**
It distinguishes "resolve the release once and thread it" from "accept any of a
list of namespaces at every position independently"; the second reads this file
happily, and is thereby inventing a dialect no DMN validator implements. Only
the outermost stray element is reported, because an `xmlns` is inherited and one
misplaced declaration would otherwise produce a dozen lines about one mistake.

**A 1.1 or 1.2 document is still rejected**, by namespace, with a message naming
the version — see `test/dmn13/not-dmn13.dmn` and
`test/corpus/cases/policy/xml-dmn1{1,2}-rejected`. Widening acceptance from one
release to three is not accepting everything, and the refusal message's second
line is now a list of the three readable releases rather than a single URI.
