#/usr/bin/bash

winter='Row 1 (Single (ColHd "Season" (FNullary (VS "Winter")))) (Single (ColHd "Dish" (FNullary (VS "Stew")))) NoComment'
spring='Row 2 (Single (ColHd "Season" (FNullary (VS "Spring")))) (Single (ColHd "Dish" (FNullary (VS "Stew")))) NoComment'
summer='Row 3 (Single (ColHd "Season" (FNullary (VS "Summer")))) (Single (ColHd "Dish" (FNullary (VS "Salad")))) NoComment'
autumn='Row 4 (Many (BaseFCell (ColHd "Season" (FNullary (VS "Autumn"))) (ColHd "Guest count" (FInRange 5.0 8.0)))) (Single (ColHd "Dish" (FNullary (VS "Steak")))) NoComment'
other='Row 5 (Many (BaseFCell (ColHd "Season" FAnything) (ColHd "Guest count" FAnything))) (Single (ColHd "Dish" (FNullary (VS "Pea soup")))) (CommentString "I give up")'


table="l -bind -treebank Table (ConsDTRow ($winter) (ConsDTRow ($spring) (ConsDTRow ($summer) (BaseDTRow ($autumn) ($other)))))"

echo $table | gf --run DMNEng*.gf | sed -E 's/ \\ /\\/g' | tr '\' '\n'
