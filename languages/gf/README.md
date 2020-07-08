# Natural Language Generation of DMN/FEEL Expressions using GF

To run this, you will need to install [GF](http://www.grammaticalframework.org/). You'll also need the [resource grammar library](https://github.com/GrammaticalFramework/gf-rgl) and the [gf-wordnet](https://github.com/GrammaticalFramework/gf-wordnet).

    $ ./demo.sh

    DMN
    Table (ConsDTRow (Row 1 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Winter")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Kidney bean stew")))) NoComment) (ConsDTRow (Row 2 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Spring")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Smoked tofu salad")))) NoComment) (ConsDTRow (Row 3 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Summer")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Roasted potatoes and a nice steak")))) NoComment) (BaseDTRow (Row 4 (Many (BaseFCell (TimeSeason (ncn season_2_N) (FNullary (VS "Autumn"))) (AmountCount (ncn number_1_N) (ncn guest_1_N) (FInRangeInt 5 8)))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Instant noodles")))) NoComment) (Row 5 (Many (BaseFCell (TimeSeason (ncn season_2_N) FAnything) (AmountCount (ncn number_1_N) (ncn guest_1_N) FAnything))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Pea soup")))) (CommentString "I give up"))))))


    DMNEngBrev3
    ---
    in Winter , Kidney bean stew ; in Spring , Smoked tofu salad ; in Summer , Roasted potatoes and a nice steak ; in Autumn for 5 - 8 guests , Instant noodles ; in any season for any number of guests , Pea soup ( I give up )

    DMNEngHornBrev1
    ---
    #1 the dish is Kidney bean stew in Winter
    #2 the dish is Smoked tofu salad in Spring
    #3 the dish is Roasted potatoes and a nice steak in Summer
    #4 the dish is Instant noodles in Autumn for between 5 and 8 guests
    #5 the dish is Pea soup in any season for any number of guests ( I give up )


### TODO

* Connect this to a format with more metadata
* Connect the WordNet lexicon into other resources. Try e.g. [this](https://hackage.haskell.org/package/WordNet-1.1.0/docs/NLP-WordNet.html)
