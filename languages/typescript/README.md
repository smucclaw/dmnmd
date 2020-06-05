# dmnmd in Typescript

This will evolve into a Typescript-native dmnmd toolsuite.

Possible dependencies:
- https://www.npmjs.com/package/js-feel

```
var whatdish = dmnmd(`
| U | Season               | Guest Count | Dish (out)                   | # Annotation  |
|---|----------------------|-------------|------------------------------|---------------|
| 1 | Fall                 | <= 8        | Spareribs                    |               |
| 2 | Winter               | <= 8        | Roastbeef                    |               |
| 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]      | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8         | Stew                         |               |
| 6 | Summer               | -           | Light Salad and a nice Steak | Hey, why not? |
`);

whatdish("Fall",4) // returns "Spareribs"

```
