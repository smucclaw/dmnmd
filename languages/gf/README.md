# Natural Language Generation of DMN/FEEL Expressions using GF

To run this, you will need to install [GF](http://www.grammaticalframework.org/).

    $ ./demo.sh
    DMN: Table (ConsDTRow (Row 1 (Single (ColHd "Season" (FNullary (VS "Winter")))) (Single (ColHd "Dish" (FNullary (VS "Stew")))) NoComment) (ConsDTRow (Row 2 (Single (ColHd "Season" (FNullary (VS "Spring")))) (Single (ColHd "Dish" (FNullary (VS "Stew")))) NoComment) (ConsDTRow (Row 3 (Single (ColHd "Season" (FNullary (VS "Summer")))) (Single (ColHd "Dish" (FNullary (VS "Salad")))) NoComment) (BaseDTRow (Row 4 (Many (BaseFCell (ColHd "Season" (FNullary (VS "Autumn"))) (ColHd "Guest count" (FInRange 5.0 8.0)))) (Single (ColHd "Dish" (FNullary (VS "Steak")))) NoComment) (Row 5 (Many (BaseFCell (ColHd "Season" FAnything) (ColHd "Guest count" FAnything))) (Single (ColHd "Dish" (FNullary (VS "Pea soup")))) (CommentString "I give up"))))))
    DMNEngBrev3: when the Season is Winter the Dish is Stew , when the Season is Spring the Dish is Stew , when the Season is Summer the Dish is Salad , when the Season is Autumn and Guest count is between 5.0 and 8.0 the Dish is Steak , when the Season is anything and Guest count is anything the Dish is Pea soup ( I give up )
    DMNEngHornBrev1: #1 the Dish is Stew when the Season is Winter
    #2 the Dish is Stew when the Season is Spring
    #3 the Dish is Salad when the Season is Summer
    #4 the Dish is Steak when the Season is Autumn and Guest count is between 5.0 and 8.0
    #5 the Dish is Pea soup when the Season is anything and Guest count is anything ( I give up )
