# dmnmd in Haskell

This will evolve into a Haskell-native dmnmd toolsuite.

Wouldn't it be cool if we could, using Template Haskell QuasiQuoting, write code like this?

The "lambda" output column is *not* part of the DMN specification. But we like higher-order functions!

``` haskell
let whatdish = [dmnmd|
| U | Season               | Guest Count | Dish (out)                   | Lambda (out) | # Annotation  |
|---+----------------------+-------------+------------------------------+--------------+---------------|
| 1 | Fall                 | <= 8        | Spareribs                    | 2            |               |
| 2 | Winter               | <= 8        | Roastbeef                    | <2           |               |
| 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       | <=4          |               |
| 4 | Spring               | [5..8]      | Steak                        | >2           |               |
| 5 | Fall, Winter, Spring | > 8         | Stew                         | [4..20]      |               |
| 6 | Summer               | -           | Light Salad and a nice Steak | ==20         | Hey, why not? |

|]

whatdish "Fall" 4
-- returns "Spareribs"
```

## Type Inference

The DMNMD parser is loosely typed, the way Python and Javascript are.

You can add explicit type annotations to the column headers.

Or you can leave them out, in which case the parser will try to guess.

The parser guesses by looking at the contents of the column.

Numeric columns usually contain some sort of comparison or range operator, or just plain numbers.

Boolean columns usually contain True or False.

Otherwise, the parser guesses String.

Note that there's a tricky detail:

input columns and output columns have slightly different types.

Input columns contain these familiar types:
- boolean :: `True`, `False`
- number :: `10`, `[20..40]`, `>= 42`
- string :: bare strings like `foo` are allowed, as are quoted strings like `"foo"`.

In addition to those types, output columns can also contain
- variable references like `age`
- function calls, like `max(0, age)`
- lambda functions, which is a fancy name for `[1..10]` and `< 10` ordinarily seen in input columns

Beyond those, we say, if you have something more sophisticated to return, then just precompute it and pass it in as an (unconsulted) input field, and refer to it in the output.

Types. How does dmnmd infer types, when a column header doesn't specify them?

Here's the call graph:

| source file           | function      | description                                                                      |
|-----------------------|---------------|----------------------------------------------------------------------------------|
| app/Main              | main          | top-level                                                                        |
| app/Main              | parseTables   | first order of business is to parse the input                                    |
| app/ParseMarkdown     | parseMarkdown | if it's a markdown input file                                                    |
| app/parseMarkdown     | fileChunks    | extract a list of unparsed decision tables from the input                        |
| app/parseMarkdown     | parseChunk    | parses each decision table, calls parseTable                                     |
| src/DMN/ParseTable    | parseTable    | parses a decision table                                                          |
| src/DMN/ParseTable    | parseTable    | FIRST, if there are explicit type annotations,                                   |
|                       |               | they get recorded into columnSignatures; if not, they're Nothing                 |
| src/DMN/ParseTable    | parseDataRows | initial parse of data rows given initial `columnSignatures`                      |
| src/DMN/ParseTable    | parseDataRow  | initial parse of individual data row, dealing with pipe separators and so on     |
| src/DMN/ParseTable    | mkFEELCol     |                                                                                  |
| src/DMN/ParseTable    | mkDataCol     | for input columns                                                                |
|                       | mkOutCol      | for output columns                                                               |
| src/DMN/ParseTable    | mkFsIn / Out  | convert a body cell to a list of FEEL expressions, based on input or output role |
| src/DMN/ParseFEEL     | parseDataCell | if we're given a Nothing or a VS column type, we honour that,                    |
|                       | (for inputs)  | and just return an FNullary VS.                                                  |
|                       |               | at this point, the untyped columns have carry column signatures of Nothing,      |
|                       |               | and the cells have been uninterpreted as VS strings.                             |
|                       |               | Cells may contain multiple comma-separated FEEL expressions.                     |
| src/DMN/ParseFEEL     | parseFEELexp  | parse a single FEEL expression.                                                  |
| src/DMN/ParseFEEL     | parseOutCell  | we call a slightly different function to parse output cells, because             |
|                       |               | those cells allow a wider range of expression -- functions and variable names.   |
|                       |               |                                                                                  |
|                       | INTERLUDE     | it's time to infer types and reprocess! We couldn't have done it earlier         |
|                       |               | because we didn't have all the data cells loaded yet,                            |
|                       |               | and type inference requires that we consider all the data cells                  |
|                       |               | in a given column at once, to see if they match up with each other               |
|                       |               |                                                                                  |
| src/DMN/ParseTable    | parseTable    | SECOND, calls mkDTable to construct the actual DecisionTable value               |
| src/DMN/DecisionTable | mkDTable      | runs inferTypes to propose revised columnheaders `newchs` and `typedchs`         |
| src/DMN/DecisionTable | inferTypes    |                                                                                  |
| src/DMN/DecisionTable | inferType     |                                                                                  |
| src/DMN/ParseFEEL     | parseDMNType  | guesses what type a given string is                                              |
| src/DMN/DecisionTable | reprocessRows | calls ???                                                                        |
|                       |               |                                                                                  |
| src/DMN/DecisionTable | mkDTable      | returns the new table with types properly inferred                               |

If the type annotation is provided, the column signature contains a Just DMNType value, and the type inference is skipped: `mkDataCol` is given a `Just DMNType` as its first argument, which is propagated to the `mkFs` function.

But if the type annotation is missing, `mkDataCol` runs with a `Nothing` argument.

- ParseTable.mkFs :: converts a data column to a list of FEELexps
- ParseFEEL.mkF :: parses a single FEELexp as part of a mkFs
- ParseFEEL.parseFEELexp :: when running with Nothing, needs to guess the type.
- ParseFEEL.parseDMNType :: without preconceptions, tries to guess the type of a given string

parseDMNVal tries to guess the type of the cell. If all the cells in the column match, then we re-run parseFEELexp to set it up as though it had been annotated.

Note that we support more syntax in the output columns than the input columns.

Output columns can include expressions like `min(0, age)` which input columns cannot. So they get a slightly different datatype.

