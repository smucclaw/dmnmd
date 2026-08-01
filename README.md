# `dmnmd`: A Command-Line Interface to DMN Decision Tables In Plain Text

![Haskell](https://github.com/smucclaw/dmnmd/workflows/Haskell%20CI/badge.svg)

> Show me your flowchart and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won't usually need your flowchart; it'll be obvious." -- Fred Brooks, The Mythical Man Month (1975)

# Inspiration

Watch this real quick: https://youtu.be/Pe34U9QuhXA ... Wouldn't it be nice to be able to do this in your text editor?

## DMNMD embeds DMN in plain text source code or Markdown

The semantics are DMN.

The syntax is Markdown.

The input is plain-text.

The output is JS, TS, Python, L4, or DMN XML. (And, in future, English, LegalRuleML...)

The interface is CLI. No mouse needed!

## Installing from Source

At the moment, `dmnmd` is an executable program written in Haskell. In future it may switch to Python.

You need GHC and cabal — [ghcup](https://www.haskell.org/ghcup/) is the usual way to get both.
That is the whole list: dmnmd has **no system dependencies**. It used to need `pcre` via
`pkg-config`, and no longer does.

    git clone git@github.com:smucclaw/dmnmd.git
    cd dmnmd/languages/haskell
    cabal build
    cabal test
    cabal install exe:dmnmd --overwrite-policy=always \
      --install-method=copy --installdir=$HOME/.local/bin

That last line puts `dmnmd` on your `PATH`, which is what the transcripts below assume.
`languages/haskell/shell.nix` supplies the two native packages if you use nix.

dmnmd builds with **cabal only**; there is no `stack.yaml` and no hpack `package.yaml`.
`languages/haskell/dmnmd.cabal` is hand-maintained and is the single source of truth.

In future packaged binaries will be made available.

## Examples

This README contains decision tables formatted in plain text, in Markdown table syntax.

The DMN CLI (`dmnmd`) interpreter parses, evaluates, and translates them into alternative formats.

### Example 1: What's for dinner?

This example is taken from [Camunda's DMN Tutorial](https://camunda.com/dmn/).

| U | Season | Dish                         | # Annotation  |
|---|--------|------------------------------|---------------|
| 1 | Fall   | Spareribs                    |               |
| 2 | Winter | Roastbeef                    |               |
| 3 | Spring | Steak                        |               |
| 4 | Summer | Light Salad and a nice Steak | Hey, why not? |

A plain text version formatted for Markdown looks literally like this:

    | U | Season | Dish                         | # Annotation  |
    |---|--------|------------------------------|---------------|
    | 1 | Fall   | Spareribs                    |               |
    | 2 | Winter | Roastbeef                    |               |
    | 3 | Spring | Steak                        |               |
    | 4 | Summer | Light Salad and a nice Steak | Hey, why not? |

### Example 2: How many guests are coming?

| U | Season               | Guest Count | Dish (out)                   | # Annotation  |
|---|----------------------|-------------|------------------------------|---------------|
| 1 | Fall                 | <= 8        | Spareribs                    |               |
| 2 | Winter               | <= 8        | Roastbeef                    |               |
| 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]      | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8         | Stew                         |               |
| 6 | Summer               | -           | Light Salad and a nice Steak | Hey, why not? |

    | U | Season               | Guest Count | Dish (out)                   | # Annotation  |
    |---|----------------------|-------------|------------------------------|---------------|
    | 1 | Fall                 | <= 8        | Spareribs                    |               |
    | 2 | Winter               | <= 8        | Roastbeef                    |               |
    | 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       |               |
    | 4 | Spring               | [5..8]      | Steak                        |               |
    | 5 | Fall, Winter, Spring | > 8         | Stew                         |               |
    | 6 | Summer               | -           | Light Salad and a nice Steak | Hey, why not? |

Yeah, DMN allows spaces in variable names. What could possibly go wrong?

But if you think of them as properties in a dictionary, that's not so bad.

### XML source

The canonical DMN XML representation of this example is available at https://github.com/camunda/camunda-bpm-examples/blob/master/dmn-engine/dmn-engine-java-main-method/src/main/resources/org/camunda/bpm/example/dish-decision.dmn11.xml

## Background

Decision Model & Notation is [an XML-based standard from OMG](https://www.omg.org/spec/DMN/). One accessible tutorial is available [here](https://camunda.com/dmn/).

To help author DMN, a number of vendors provide graphical user interfaces as part of their [conforming implementations](https://dmn-tck.github.io/tck/). It is also possible to [import decision tables authored in a spreadsheet](https://github.com/camunda/camunda-dmn-xlsx).

What are decision tables? An ancient magic from an earlier age of computing, powerful but little known among developers today. See [Hillel Wayne's introduction](https://www.hillelwayne.com/post/decision-tables/). It may be making a comeback, though: a handful of [packages have appeared on npm](https://www.npmjs.com/search?q=dmn) in the last few years.

The [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) emphasizes the value of flat text files. While XML technically qualifies as text, many consider it "unwieldy": hence the popularity of [JSON](https://en.wikipedia.org/wiki/JSON) and [YAML](https://en.wikipedia.org/wiki/YAML).

Command-line utilities such as [json (on NPM)](https://www.npmjs.com/package/json) help manipulate JSON. `dmnmd` is intended to be the moral equivalent for manipulating DMN in Markdown.


## Imports

### from Markdown

Supported. This is the native format for `dmnmd`.

ASCII has its limitations. In graphical decision tables, output columns are separated from input columns by a double bar; most GUI implementations use colour and other formatting to distinguish input, output, and annotation columns. In `dmnmd` syntax, output columns are optionally labeled with an `(out)`; annotation columns are prefixed with a `#`. By default, if the columns are unlabeled, the rightmost column will be taken to be the output, and columns to the left will be taken to be inputs. (Leaving out annotation columns.)

You can also prefix output columns with a `>` character.

Columns are optionally typed using a colon. You will see `Column Name : String`, `Column Name : Number`, and `Column Name : Boolean`. If you omit the type definition, `dmnmd` will attempt to infer the type.

Inside a string column, a cell wrapped in double quotes is a string **literal**: `"Fall"` means
the four characters `Fall`, and the quotes are not part of the value. This is how DMN XML writes
strings, so a table imported from XML reads the same as one written by hand. A cell that is not
a well-formed literal is kept exactly as written, so `Non-Participating` and `5' 10"` are
unaffected.

#### Collection columns

A column can hold a **collection**, written `Column Name : [String]`. This is DMN's
`isCollection="true"`, so a `<itemDefinition isCollection="true">` imported from XML becomes one
of these.

In a collection column a cell means **membership** — does the collection contain this value:

| F | roles : [String] | grant (out) |
|---|------------------|-------------|
| 1 | admin            | full        |
| 2 | clerk, teller    | read        |
| 3 | -                | none        |

Rule 1 fires when `roles` contains `admin`. The comma keeps its usual meaning of "any of these",
at the element level, so rule 2 fires when `roles` contains either `clerk` or `teller`. The
wildcard `-` matches any collection, including the empty one, and the empty collection contains
nothing, so it matches no membership rule.

An **output** cell in a collection column is a value rather than a test: `fries, slaw` is the
two-element collection, and `-` is the empty one.

A cell that is **not** a plain member — a comparison like `> 3`, a range, an arithmetic
expression — is **refused**, with a message naming the row and the column. This is deliberate.
"Some element is over 3" and "every element is over 3" are different rules, nothing in the table
says which, and DMN gives the comparison no meaning at all. Rather than guess, `dmnmd` asks you to
aggregate the collection to a scalar before the table, or split the column. FEEL constructs that
`dmnmd` does not implement — `not(...)`, `list contains(...)`, a `[a, b]` literal — are refused
for the same reason.

At the `-q` prompt a collection is written the way FEEL writes one, `[admin, clerk]`, with `[]`
for the empty collection.

In some decision tables, the input and outputs are enumerated in a sort of sub-header row. The order matters.

This implementation only supports vertical layout. Horizontal and crosstab layouts may appear in a future version if there is demand.

The above is perhaps best explained by an example; see figure 8.19 of the DMN 1.3 spec.

#### Example 3: Routing Rules

For hit policy "O", the order of results in the output is determined by the order of the column enums.

The column enums are giving in a subhead row between the top row and body data row "1".

The subhead row goes **below** the `|---|` rule, not above it. Markdown requires the `|---|`
immediately after the header row, so a table with the enums above it does not render as a table
at all.

Those enums are a **declared domain**: they say what the column is allowed to hold, and `dmnmd`
checks them. A plain value that is not one of the listed values is an error and the table is
refused — a misspelled `HIHG` in a column declared `LOW, MEDIUM, HIGH` is a typo, not a fourth
risk category. Cells holding a *test* rather than a value (`< 18`, `[18..65]`, `-`, or an
arithmetic expression) are not checked against the list, because a test does not name a member
of the domain, it selects a subset of it. Where the domain is itself a numeric range
(`[0..150]`), plain numbers are checked to lie inside it.

The same applies to tables read from DMN XML, where the domain is written as `<inputValues>`
and `<outputValues>`.

| O | Age | Risk Category     | Debt Review :Boolean | > Routing              | > Review level         | Reason (out)                |
|---|-----|-------------------|----------------------|------------------------|------------------------|-----------------------------|
|   |     | LOW, MEDIUM, HIGH |                      | DECLINE, REFER, ACCEPT | LEVEL 2, LEVEL 1, NONE |                             |
| 1 | -   | -                 | -                    | ACCEPT                 | NONE                   | Acceptable                  |
| 2 | <18 |                   |                      | DECLINE                | NONE                   | Applicant too young         |
| 3 |     | HIGH              |                      | REFER                  | LEVEL 1                | High risk application       |
| 4 |     |                   | True                 | REFER                  | LEVEL 2                | Applicant under debt review |

This example comes from the [DMM 1.3 specification](https://www.omg.org/spec/DMN/1.3/PDF), page 96.

Note that advanced hit policies are not yet implemented for code generation, only for evaluation.

### In the IDE

JetBrains MPS is a language workbench and an IDE from the future. Look how decision tables live right in the IDE: https://www.youtube.com/watch?v=Pe34U9QuhXA

### from XML

On the roadmap.

    $ dmnmd --from=example1.dmn --to=example1.md

## Exports

Interactive evaluation is intended for quick testing in development.
For real-world use, you probably want *code generation* to an
operational language like Python or Javascript. Or *extraction* to SQL
or JSON (suitable for NoSQL) or to XML (as OMG intended). Or to
natural language!

### to Typescript

    $ dmnmd README.md --to=ts

By default, generates Typescript.

You can output Javascript instead by saying `--to=js`

Options:

**--props** Normally, functions expect as many parameters as there are input columns. with `--props`, functions expect input in a single `props` object; a "Props" type is generated.

    % dmnmd README.md --to=ts --pick="Example 2" -r
    type Props_Example_2 = {
        "Season" : string;
        "Guest Count" : number;
    }
    type Return_Example_2 = {
        "Dish" : string;
    }
    export function Example_2 ( props : Props_Example_2 ) : Return_Example_2 {
      if (props["Season"] === "Fall" && props["Guest Count"] <= 8.0) { // 1
        return {"Dish":"Spareribs"};
      }
      else if (props["Season"] === "Winter" && props["Guest Count"] <= 8.0) { // 2
        return {"Dish":"Roastbeef"};
      }
      else if (props["Season"] === "Spring" && props["Guest Count"] <= 4.0) { // 3
        return {"Dish":"Dry Aged Gourmet Steak"};
      }
      else if (props["Season"] === "Spring" && (5.0 <= props["Guest Count"] && props["Guest Count"] <= 8.0)) { // 4
        return {"Dish":"Steak"};
      }
      else if ((props["Season"] === "Fall" || props["Season"] === "Winter" || props["Season"] === "Spring") && props["Guest Count"] > 8.0) { // 5
        return {"Dish":"Stew"};
      }
      else if (props["Season"] === "Summer") { // 6
        return {"Dish":"Light Salad and a nice Steak"};
        // Hey, why not?
      }
    }

We use "props" here as a synonym for the more proper term "context".

### to Javascript

This works today, modulo full support for hit policies.

    % dmnmd README.md --pick="Example 2" --to=js
    export function Example_2 ( Season, Guest_Count ) {
      if (Season === "Fall" && Guest_Count <= 8.0) { // 1
        return {"Dish":"Spareribs"};
      }
      else if (Season === "Winter" && Guest_Count <= 8.0) { // 2
        return {"Dish":"Roastbeef"};
      }
      else if (Season === "Spring" && Guest_Count <= 4.0) { // 3
        return {"Dish":"Dry Aged Gourmet Steak"};
      }
      else if (Season === "Spring" && (5.0 <= Guest_Count && Guest_Count <= 8.0)) { // 4
        return {"Dish":"Steak"};
      }
      else if ((Season === "Fall" || Season === "Winter" || Season === "Spring") && Guest_Count > 8.0) { // 5
        return {"Dish":"Stew"};
      }
      else if (Season === "Summer") { // 6
        return {"Dish":"Light Salad and a nice Steak"};
        // Hey, why not?
      }
    }

On the roadmap: a fully native version which allows direct evaluation of decison tables as functions. Should be about a week's worth of work, accelerated by the availability of the [js-feel](https://github.com/EdgeVerve/feel) package.

The vision: after you `npm i --save dmnmd`, you can define a function `dinner` by saying:

    const dinner = dmnmd(`
    | U | Season | Dish                         | # Annotation  |
    |---|--------|------------------------------|---------------|
    | 1 | Fall   | Spareribs                    |               |
    | 2 | Winter | Roastbeef                    |               |
    | 3 | Spring | Steak                        |               |
    | 4 | Summer | Light Salad and a nice Steak | Hey, why not? |
    `)

You should then be able to call `dinner({Season:"Fall"})` and get back `{Dish:"Spareribs"}`.

### to L4

Works today. [L4](https://github.com/legalese/l4-ide) is a language for law; this backend emits a
`BRANCH` expression with column-aligned **ditto** (`^`), where each `^` repeats the guard token
directly above it. Reading down a column is how a lawyer checks that a condition really is the
same across several rules.

    % dmnmd README.md --pick="Example 2" --to=l4
    GIVEN Season        IS A STRING
          `Guest Count` IS A NUMBER
    GIVETH A STRING
    `Example 2` Season `Guest Count` MEANS
      BRANCH
        IF Season                                                                     EQUALS "Fall"   AND `Guest Count`                               <= 8 THEN "Spareribs"
        IF ^                                                                          ^      "Winter" ^   `Guest Count`                               ^  ^ THEN "Roastbeef"
        IF ^                                                                          ^      "Spring" ^   `Guest Count`                               ^  4 THEN "Dry Aged Gourmet Steak"
        IF ^                                                                          ^      ^        ^   (`Guest Count` >= 5 AND `Guest Count` <= 8)      THEN "Steak"
        IF (Season EQUALS "Fall" OR Season EQUALS "Winter" OR Season EQUALS "Spring")                 ^   `Guest Count`                               >  8 THEN "Stew"
        IF Season                                                                     EQUALS "Summer"                                                      THEN "Light Salad and a nice Steak"  -- Hey, why not?
        OTHERWISE ""

The alignment is not cosmetic: L4 resolves `^` by absolute source column, so the emitter measures
every token's display width — including East Asian wide characters — against the same table the L4
lexer uses. A one-column drift would make a caret silently copy the wrong token.

Unlike the other backends, this one **refuses** the list-valued hit policies (`C`, `A`, `O`, `R`)
rather than approximating them: a scalar first-match `BRANCH` would return one row and quietly drop
the rest.

### to Haxe

Perhaps [Haxe])(https://www.haxe.org/) can help us achieve a longer list of transpilation targets.

### to XML

    $ dmnmd README.md --to=xml

Exports to XML conforming to the DMN 1.3 specification: one `<definitions>` carrying one
`<decision>` per table, plus the `<inputData>` and `<informationRequirement>` wiring that makes the
file open in a DMN tool. The emitted documents in this repository are checked with
`xmllint --noout --schema languages/haskell/xsd/DMN13.xsd`.

DMN 1.3 specifically, even though `--from=xml` also reads 1.4 and 1.5: 1.3 is the only release this
repository ships a schema for, so it is the only one whose output can be validated here.

The gate on this backend is that dmnmd can read its own output. For every markdown fixture in the
tree, `F --to=xml | --from=xml --to=ts` must equal `F --to=ts` byte for byte — see
`languages/haskell/test/roundtrip/`. Where a construct dmnmd accepts has no DMN spelling, dmnmd
either translates it faithfully and warns, or refuses; it never emits a document that means
something else in silence. Three such constructs exist today:

  * a **wildcard output cell** (`-` in an output column) becomes an empty `<text/>`, with a warning:
    `-` is unary-test syntax and a DMN output entry is a literal expression, so a conformant engine
    reads that rule as producing null.
  * a **table with no output column**, and a **row with fewer cells than columns**, are refused.
    DMN requires at least one `<output>` and exactly one entry per column, and padding a short row
    with `-` would silently widen the rule.
  * **authored rule numbers** that are not `1..n` are renumbered, with a warning. DMN identifies a
    rule by position and has no field for the number written in the leftmost cell.

The suffix comparison below (`5 <=`) is emitted in its mirrored prefix form, `>= 5`. That is
correct but not isomorphic, and dmnmd cannot warn about it: the mirror happens in the parser, so by
the time any backend runs the two spellings are the same value. See `DECISIONS.md` D-11.

### to Flora-2

On the roadmap.

    $ dmnmd README.md --to=flora2

#### Example 4: Grocery Delivery Boxes

This learning exercise is detailed at [ex-20200527-grocery](../doc/ex-20200527-grocery/).

### to other Prologs
### to XLSX

On the roadmap.

    $ dmnmd README.md --to=xlsx

Exports to an Excel spreadsheet.

### to SQL

On the roadmap.

    $ dmnmd README.md --to=sql

`dmnmd` outputs DDL, DML, and DQL statements suitable for SQLite, Postgres, and others:

    CREATE TABLE example_1 (row_id primary key, season text, dish text, annotation text);
    INSERT INTO  example_1 (row_id, season, dish, annotation) VALUES
        (1, "Fall",   "Spareribs", NULL),
        (2, "Winter", "Roastbeef", NULL),
        (3, "Spring", "Steak",     NULL),
        (4, "Summer", "Light Salad and a nice Steak", "Hey, why not?");

    -- Query
    SELECT row_id, dish, annotation FROM example_1 WHERE Season = ?;

What to do about rows that contain FEEL expressions? For lack of a better place, that logic goes into the query.

### to Python

Works today, modulo full support for hit policies — same caveat as the Javascript backend.
Note the flag is `--to=py`; `--to=python` is rejected.

    % dmnmd README.md --pick="Example 2" --to=py
    def Example_2 ( Season, Guest_Count ) :
      if (Season == "Fall" and Guest_Count <= 8.0): # 1
        return {"Dish":"Spareribs"};

      elif (Season == "Winter" and Guest_Count <= 8.0): # 2
        return {"Dish":"Roastbeef"};

      elif (Season == "Spring" and Guest_Count <= 4.0): # 3
        return {"Dish":"Dry Aged Gourmet Steak"};

      elif (Season == "Spring" and (5.0 <= Guest_Count and Guest_Count <= 8.0)): # 4
        return {"Dish":"Steak"};

      elif ((Season == "Fall" or Season == "Winter" or Season == "Spring") and Guest_Count > 8.0): # 5
        return {"Dish":"Stew"};

      elif (Season == "Summer"): # 6
        return {"Dish":"Light Salad and a nice Steak"};
        # Hey, why not?

### to Natural Language

On the roadmap.

    $ dmnmd README.md --to=english --dialect=Horn

    The dish is Spareribs when the Season is Fall.

    The dish is Roastbeef when the Season is Winter.

    The dish is Steak when the Season is Spring.

    The dish is Light Salad and a nice Steak when the Season is Summer. (Hey, why not?)

Brevity is a parameter which makes the output more concise.

    $ dmnmd README.md --to=english --dialect=Horn --brevity=2

    The dish is Spareribs when the Season is Fall, Roastbeef in Winter, Steak in Spring, and (Hey, why not?) Light Salad and a nice Steak in Summer.

More brevity requires more tacit knowledge. This is safer when we have an accompanying ontology to refer to.

    $ dmnmd README.md --to=english --dialect=Horn --brevity=3

    Spareribs in the Fall; Roastbeef in Winter; Steak in Spring; and (Hey, why not?) Light Salad and a nice Steak otherwise.

It is characteristic of natural language that utterances omit "common sense" world knowledge, and employ other linguistic shorthand which is obvious to native speakers and often challenging to others.

The above dialect is `Horn`, which uses an "output if input" ordering . Omitting that option, we get an "input then output" ordering:

    $ dmnmd README.md --to=english --brevity=3

    In the Fall, Spareribs; in Winter, Roastbeef; in Spring, Steak; and in Summer, Light Salad and a nice Steak (Hey, why not?).

By default, brevity is 1.

    $ dmnmd README.md --to=english

    When the Season is Fall, the Dish is Spareribs.
    
    When the Season is Winter, the Dish is Roastbeef.
    
    When the Season is Spring, the Dish is Steak.
    
    When the Season is Summer, the Dish is Light Salad and a nice Steak (Hey, why not?).

Some linguistic magic happens behind the scenes. Different parameters take different determiners.

### to LegalRuleML

On the roadmap.

    $ dmnmd README.md --to=legalruleml --brevity=4

    ... <lrml:...> ...

### to Prolog

On the roadmap.

    $ dmnmd README.md --to=prolog --brevity=4

    dish("Spareribs") :- season("Fall").
    dish("Roastbeef") :- season("Winter").
    dish("Steak")     :- season("Spring").
    dish("Light salad and a nice Steak") :- season("Summer").

## Co-requisites

Your IDE may need a plugin to work with Markdown tables.
- VS Code: ["markdown table" extensions](https://marketplace.visualstudio.com/search?term=%22markdown%20table%22&target=VSCode&category=All%20categories&sortBy=Relevance)
- Atom: ["markdown table" packages](https://atom.io/packages/search?q=markdown+table)
- Vim: [vim extension markdown tables](https://www.google.com/search?q=vim+extension+markdown+tables)
- Emacs: You're all set. `M-x markdown-mode` and hit TAB after starting your table.

## Evaluation

A decision table is basically a function. Let's run it.

> **This transcript does not work today.** `dmnmd -q` crashes on the first successful query with
> `Non-exhaustive patterns in function showToJSON` (`app/Main.hs`). It is a known defect, recorded
> as `languages/haskell/test/corpus/cases/symptom/cli-showtojson-unknown-format`, and the transcript
> below is what it is *supposed* to print. The evaluator itself is fine — `evalTable` is the
> semantic oracle the whole test suite is written against; it is the printing of the answer that
> falls over.

Interactively, on the command line:

    $ dmnmd -q README.md --pick "Example 1"
    Example 1> Fall
    Example 1: "Dish":"Spareribs"

    Example 1> Winter
    Example 1: "Dish":"Roastbeef"

    Example 1> Spring
    Example 1: "Dish":"Steak"

    Example 1> Summer
    Example 1: "Dish":"Light Salad and a nice Steak"

    Example 1> ^D
    
    
    
    $ dmnmd -q README.md --pick "Example 2"
    Example 2> Fall, 7
    Example 2: "Dish":"Spareribs"

    Example 2> Fall, 9
    Example 2: "Dish":"Stew"

    Example 2> Summer, 10
    Example 2: "Dish":"Light Salad and a nice Steak"


Coming soon: Batch-mode.

    $ echo "Winter" | dmnmd README.md --dt="Example 1"
    Roastbeef

Coming soon: JSON in, JSON out.

    $ echo '{ "Season": "Winter" }' | dmnmd README.md --pick "Example 1" -j
    { "Season": "Winter", "Dish": "Roastbeef" }

## Extensions

This implementation aims to extend DMN with higher-order functional programming capabilities. Input cells already can be what a functional programmer would call a "function section" -- a partially applied binary function curried to expect a single argument. Strictly speaking, DMN 1.3 output columns need to be "plain" values: strings, Booleans, and numbers. This implementation proposes to allow the same expressive range for output columns as input columns, so you could return a range, such as `[20..40]`, if you wanted.

Paper: https://t.co/Oap8NMywyJ?amp=1 "Adding Constraint Tables to the DMN Standard: Preliminary Results"

## Automated Reasoning

https://www.researchgate.net/publication/301836662_Semantics_and_Analysis_of_DMN_Decision_Tables
