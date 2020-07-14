#/usr/bin/bash

winter='Row 1 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Winter")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Kidney bean stew")))) NoComment'
spring='Row 2 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Spring")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Smoked tofu salad")))) NoComment'
summer='Row 3 (Single (TimeSeason (ncn season_2_N) (FNullary (VS "Summer")))) (Single (Attribute (ncn dish_2_N) (FNullary (VS  "Roasted potatoes and a nice steak")))) NoComment'
autumn='Row 4 (Many (BaseFCell (TimeSeason (ncn season_2_N) (FNullary (VS "Autumn"))) (AmountCount (ncn number_1_N) (ncn guest_1_N)  (FInRangeInt 5 8)))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Instant noodles")))) NoComment'
other='Row 5 (Many (BaseFCell (TimeSeason (ncn season_2_N) FAnything) (AmountCount (ncn number_1_N) (ncn guest_1_N) FAnything))) (Single (Attribute (ncn dish_2_N) (FNullary (VS "Pea soup")))) (CommentString "I give up")'


table="l -bind -treebank Table (ConsDTRow ($winter) (ConsDTRow ($spring) (ConsDTRow ($summer) (BaseDTRow ($autumn) ($other)))))"

echo "gr -cat=DTRow -number=10 | l -treebank -bind" | gf --run DMN.pgf | sed -E 's/ \\ /\\/g ; s/: /\\---\\/g ; s/DMN/\\DMN/g' | tr '\' '\n'

echo $table | gf --run DMN.pgf | sed -E 's/ \\ /\\/g ; s/: /\\---\\/g ; s/DMN/\\DMN/g' | tr '\' '\n'

