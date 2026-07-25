# Miles-Card Routing — DMN decision tables (dmnmd format)

Golden **test input** for the `dmnmd --to=l4` transpiler. The expected output is the
hand-written `docs/miles-card.l4`. Round-trip test: `dmnmd --to=l4 miles-card-dmn.md`
should equal `miles-card.l4`.

Conventions (per [smucclaw/dmnmd](https://github.com/smucclaw/dmnmd)): hit-policy letter
in the top-left cell with numbered rules; `(out)` suffix marks output columns; `#` prefix
marks annotation columns; `: Type` on inputs; enums are unquoted, comma-listed; `-` is the
wildcard. Merchant-vs-MCC matches are split into separate OR-rows (same row = AND).

## `Categorize` — hit policy `F`

| F | merchant : String | mcc : Number | channel : String | currency : String | Category (out) : String |
|---|---|---|---|---|---|
| 1 | - | - | - | MYR | MYR |
| 2 | Cold Storage, CS Fresh, Jasons, Giant | - | inPerson | - | GroceriesPhysical |
| 3 | foodpanda | - | - | - | foodpanda |
| 4 | Charge+ | - | - | - | TeslaCharging |
| 5 | SimplyGo | - | - | - | SimplyGo |
| 6 | - | 4111, 4112 | - | - | SimplyGo |
| 7 | Netflix, Disney+, Spotify, Viu | - | - | - | Streaming |
| 8 | - | 4899, 5815 | online | - | Streaming |
| 9 | McDonald's, KFC, Starbucks, Ya Kun | - | - | - | FastFood |
| 10 | - | 5814 | - | - | FastFood |
| 11 | - | 5812, 5813 | - | - | Dining |
| 12 | - | 4121 | - | - | RideHailing |
| 13 | RedMart | - | - | - | GroceriesOnline |
| 14 | - | 5411 | online | - | GroceriesOnline |
| 15 | Shopee | - | - | - | Shopee |
| 16 | StarHub | - | - | - | StarHub |
| 17 | - | - | online | - | OtherOnline |
| 18 | - | 5311, 5411, 5611, 5651 | inPerson | - | ContactlessTap |
| 19 | - | - | - | - | Other |

## `CardToUse` — hit policy `F`

| F | Category : String | PaymentMethod : String | yuuRemaining : Number | passionRemaining : Number | fourMpdRemaining : Number | Card (out) : String | mpd (out) : String | # Sensing |
|---|---|---|---|---|---|---|---|---|
| 1 | GroceriesPhysical, foodpanda, SimplyGo, TeslaCharging | - | > 0 | - | - | DBS yuu | 10 | shared |
| 2 | GroceriesPhysical, foodpanda, SimplyGo, TeslaCharging | - | <= 0 | > 0 | - | PAssion Debit | 5 | self |
| 3 | GroceriesPhysical, foodpanda | Online, MobileWallet | <= 0 | <= 0 | > 0 | Woman's World | 4 | shared |
| 4 | SimplyGo | - | <= 0 | <= 0 | - | SC Smart | 6% cb | self |
| 5 | GroceriesPhysical, foodpanda, TeslaCharging | - | <= 0 | <= 0 | - | PRVI | 1.4 | — |
| 6 | GroceriesOnline, Shopee, OtherOnline, StarHub | - | - | - | > 0 | Woman's World | 4 | shared |
| 7 | Dining, RideHailing | - | - | - | > 0 | HSBC Revolution | 4 | shared |
| 8 | ContactlessTap | MobileWallet | - | - | > 0 | UOB Preferred | 4 | shared |
| 9 | ContactlessTap | PhysicalContactless | - | - | > 0 | HSBC Revolution | 4 | shared |
| 10 | ContactlessTap | Amaze | - | - | > 0 | Citi Rewards | 4 | shared |
| 11 | Streaming, FastFood | - | - | - | - | SC Smart | 6% cb | self |
| 12 | MYR | - | - | > 0 | - | PAssion Debit | ~4.5 | self |
| 13 | MYR | - | - | <= 0 | - | PRVI | 2.4 | — |
| 14 | Other | - | - | - | - | PRVI | 1.4 / 2.4 | — |
| 15 | - | - | - | - | - | PRVI | 1.4 | — |
