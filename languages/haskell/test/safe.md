# SAFE in Markdown

This is a markdown implementation of the Simple Agreement for Future Equity
using dmnmd.

## Safe Price

| Safe Price                                                    |
| ------------------------------------------------------------- |
| decimal(Post-Money Valuation Cap / Company Capitalization, 4) |

## Discount Price
| Discount Price                                    |
| ------------------------------------------------- |
| decimal(Preferred Share Price * Discount Rate, 4) |

## Liquidity Event
| F   | Event Type                                                       | Liquidity Event (out) |
| --- | ---------------------------------------------------------------- | --------------------- |
| 1   | "Change of Control", "Direct Listing", "Initial Public Offering" | true                  |
| 2   | -                                                                | false                 |

## Liquidity Price
| Liquidity Price                                                  |
| ---------------------------------------------------------------- |
| decimal( Post-Money Valuation Cap / Liquidity Capitalization, 4) |

## Liquidity Capitalization
| Liquidity Capitalization                                                                                         |
| ---------------------------------------------------------------------------------------------------------------- |
| floor((Capital Stock Issued + Issued Options + Promised Options Receiving Proceeds) / (1-Converting Securities)) |

## Dissolution Event
| F   | Event Type                                                                                                                            | Liquidity Event | Dissolution Event (out) |
| --- | ------------------------------------------------------------------------------------------------------------------------------------- | --------------- | ----------------------- |
| 1   | "Voluntary Termination of Operations", "Other Liquidation, Dissolution, or Winding-Up", "General Assignment for Benefit of Creditors" | false           | true                    |
| 2   | -                                                                                                                                     | -               | false                   |

## Event Prior to Termination
| U   | Safe Terminated | Event Date < Safe Termination Date | Event Prior to Termination |
| --- | --------------- | ---------------------------------- | -------------------------- |
| 1   | false           | -                                  | true                       |
| 2   | true            | true                               | true                       |
| 3   | true            | false                              | false                      |

## Conversion Price
| Conversion Price                                                     |
| -------------------------------------------------------------------- |
| if (Safe Price < Discount Price) then Safe Price else Discount Price |

## Safe Event Type
| F   | Dissolution Event | Liquidity Event | Event Type              | SAFE Event Type    |
| --- | ----------------- | --------------- | ----------------------- | ------------------ |
| 1   | true              | false           | -                       | "Dissolution"      |
| 2   | false             | true            | -                       | "Liquidity"        |
| 3   | -                 | -               | "Bona Fide Transaction" | "Equity Financing" |
| 4   | -                 | -               | -                       | "None"             |

## Conversion Amount
| Conversion Amount                          |
| ------------------------------------------ |
| floor( Purchase Amount / Liquidity Price ) |

## Company Capitalization
| Company Capitalization                                                                                                                   |
| ---------------------------------------------------------------------------------------------------------------------------------------- |
| floor(Unissued Option Pool + Promised Options Receiving Proceeds + Issued Options + Capital Stock Issued ) / (1 - Converting Securities) |

## Cash Out Amount
| Cash Out Amount |
| --------------- |
| Purchase Amount |

## Result of Termination
| U   | Event Prior to Termination | SAFE Event Type    | Security Holders Given Choice of Form and Amount | Change of Control Intended to Qualify as Tax Free | Investor Provides Documents (out ) | Safe Preferred Stock Due (out)            | Cash Amount Due (out)                    | Investor Choice of Form and Amount (out) | Board May Reduce Cash Portion of Proceeds Payable (out) | SAFE Operaters Like Standard Non-Participating Preferred Stock (out) | Terminating Event (out)     |
| --- | -------------------------- | ------------------ | ------------------------------------------------ | ------------------------------------------------- | ---------------------------------- | ----------------------------------------- | ---------------------------------------- | ---------------------------------------- | ------------------------------------------------------- | -------------------------------------------------------------------- | --------------------------- |
| 1   | true                       | "Equity Financing" | -                                                | -                                                 | true                               | floor(Purchase Amount / Conversion Price) | 0                                        | false                                    | false                                                   | false                                                                | "Issuance of Capital Stock" |
| 2   | true                       | "Dissolution"      | -                                                | -                                                 | false                              | 0                                         | Cash Out Amount                          | false                                    | false                                                   | true                                                                 | "Payment or Setting Aside"  |
| 3   | true                       | "Liquidity"        | false                                            | false                                             | false                              | 0                                         | max( Cash Out Amount, Conversion Amount) | false                                    | false                                                   | true                                                                 | "Payment or Setting Aside"  |
| 4   | true                       | "Liquidity"        | true                                             | false                                             | false                              | 0                                         | max( Cash Out Amount, Conversion Amount) | true                                     | false                                                   | true                                                                 | "Payment or Setting Aside"  |
| 5   | true                       | "Liquidity"        | false                                            | true                                              | false                              | 0                                         | max( Cash Out Amount, Conversion Amount) | false                                    | true                                                    | true                                                                 | "Payment or Setting Aside"  |
| 6   | true                       | "Liquidity"        | true                                             | true                                              | false                              | 0                                         | max( Cash Out Amount, Conversion Amount) | true                                     | true                                                    | true                                                                 | "Payment or Setting Aside"  |
| 7   | false                      | -                  | -                                                | -                                                 | false                              | 0                                         | 0                                        | false                                    | false                                                   | false                                                                | "n/a"                       |