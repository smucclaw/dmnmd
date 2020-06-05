# dmnmd in Haskell

This will evolve into a Haskell-native dmnmd toolsuite.

Using Template Haskell QuasiQuoting,

``` haskell
let whatdish = [dmnmd|
| U | Season               | Guest Count | Dish (out)                   | # Annotation  |
|---|----------------------|-------------|------------------------------|---------------|
| 1 | Fall                 | <= 8        | Spareribs                    |               |
| 2 | Winter               | <= 8        | Roastbeef                    |               |
| 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]      | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8         | Stew                         |               |
| 6 | Summer               | -           | Light Salad and a nice Steak | Hey, why not? |
|]

whatdish "Fall" 4
-- returns "Spareribs"
```

