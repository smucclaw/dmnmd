concrete WordnetNPsEng of WordnetNPs = WordnetNPsI
  with
   (Syntax = SyntaxEng),
   (Symbolic = SymbolicEng),
   (WordNet = WordNetEng) ** open ParadigmsEng in {

  lin
    npcn np = let s : Str = (mkUtt np).s in mkCN (mkN s s s s) ;

}
