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

