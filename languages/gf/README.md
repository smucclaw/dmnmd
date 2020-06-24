# Natural Language Generation of DMN/FEEL Expressions using GF

To run this, you will need to install [GF](http://www.grammaticalframework.org/).

    $ ./demo.sh
    DMN
    ---
    Table (ConsDTRow (Row 1 (Single (TimeSeason "Season" (FNullary (VS "Winter")))) (Single (Attribute "Dish" (FNullary (VS "Kidney bean stew")))) NoComment) (ConsDTRow (Row 2 (Single (TimeSeason "Season" (FNullary (VS "Spring")))) (Single (Attribute "Dish" (FNullary (VS "Smoked tofu salad")))) NoComment) (ConsDTRow (Row 3 (Single (TimeSeason "Season" (FNullary (VS "Summer")))) (Single (Attribute "Dish" (FNullary (VS "Roasted potatoes and a nice steak")))) NoComment) (BaseDTRow (Row 4 (Many (BaseFCell (TimeSeason "Season" (FNullary (VS "Autumn"))) (AmountCount "Guest" "count" (FInRangeInt 5 8)))) (Single (Attribute "Dish" (FNullary (VS "Instant noodles")))) NoComment) (Row 5 (Many (BaseFCell (TimeSeason "Season" FAnything) (AmountCount "Guest" "count" FAnything))) (Single (Attribute "Dish" (FNullary (VS "Pea soup")))) (CommentString "I give up"))))))

    DMNEngBrev3
    ---
    in Winter , Kidney bean stew ; in Spring , Smoked tofu salad ; in Summer , Roasted potatoes and a nice steak ; in Autumn and with 5 - 8 Guests , Instant noodles ; in any Season and with any number of Guests , Pea soup ( I give up )

    DMNEngHornBrev1
    ---
    the Dish is Kidney bean stew in Winter (#1)
    the Dish is Smoked tofu salad in Spring (#2)
    the Dish is Roasted potatoes and a nice steak in Summer (#3)
    the Dish is Instant noodles in Autumn and with between 5 and 8 Guests (#4)
    the Dish is Pea soup in any Season and with any number of Guests ( I give up ) (#5)
