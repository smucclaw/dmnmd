## `Band` — hit policy `F`

| F | Age : Number | Risk Category     | Routing (out)          |
|---|--------------|-------------------|------------------------|
|   | [0..150]     | LOW, MEDIUM, HIGH | DECLINE, REFER, ACCEPT |
| 1 | < 18         | -                 | DECLINE                |
| 2 | [18..65]     | HIGH              | REFER                  |
| 3 | > 65         | LOW               | ACCEPT                 |
