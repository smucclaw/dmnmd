# dmnmd in Python

This will evolve into a Python-native dmnmd toolsuite.

Possible dependencies:
- https://github.com/russellmcdonell/pyDMNrules
- https://github.com/russellmcdonell/pySFeel

See also https://python-forum.io/Thread-DMN-FEEL-in-Python?pid=73512&highlight=dmn%2Ffeel#pid73512

```
var whatdish = dmnmd("""
| U | Season               | Guest Count | Dish (out)                   | # Annotation  |
|---|----------------------|-------------|------------------------------|---------------|
| 1 | Fall                 | <= 8        | Spareribs                    |               |
| 2 | Winter               | <= 8        | Roastbeef                    |               |
| 3 | Spring               | <= 4        | Dry Aged Gourmet Steak       |               |
| 4 | Spring               | [5..8]      | Steak                        |               |
| 5 | Fall, Winter, Spring | > 8         | Stew                         |               |
| 6 | Summer               | -           | Light Salad and a nice Steak | Hey, why not? |
""");

whatdish("Fall",4) # returns "Spareribs"

```
