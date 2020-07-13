instance LexDMNEng of LexDMN = DMNParams, SyntaxEng ** open ParadigmsEng in {

oper
  anything_NP = mkNP (mkN "anything") ;
  emptyNP = anything_NP ** {s = \\_ => []} ;
  upon_Prep = mkPrep "upon" ;
  at_Prep = mkPrep "at" ;

  np2s np = lin S (mkUtt np) ;

  feq = table { -- Omitted in most cases: "X is Y" instead of "X is as much as Y"
      HWeight => "as heavy as" ;
      HHeight => "as tall as" ;
      HLength => "as long as" ;
      HAmountCount => "as many as" ;
      HAmountMass => "as much as" ;
      HSize => "as large as" ;
      HSpeed => "as fast as" ;
      HTime => "exactly" ;
      _ => "equal to"} ;

  flt = table {
      HWeight => "lighter than" ;
      HHeight => "shorter than" ;
      HLength => "shorter than" ;
      HAmountCount => "fewer than" ;
      HAmountMass => "less than" ;
      HSize => "smaller than" ;
      HSpeed => "slower than" ;
      HTime => "less than" ; -- ?
      _ => "less than"} ;

  fgt = table {
      HWeight => "heavier than" ;
      HHeight => "taller than" ;
      HLength => "longer than" ;
      HAmountCount => "more than" ;
      HAmountMass => "more than" ;
      HSize => "larger than" ;
      HSpeed => "faster than" ;
      HTime => "longer than" ; -- ?
      _ => "greater than"} ;

  flte = or flt feq ;
  fgte = or fgt feq ;

  or : (HeaderType=>Str) -> (HeaderType=>Str) -> (HeaderType=>Str) = \lt,eq ->
    \\h => lt ! h ++ "or" ++ eq ! h ;
}
