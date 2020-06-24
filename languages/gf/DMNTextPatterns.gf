resource DMNTextPatterns = open Prelude, Coordination in {
-- Resource module for mixing and matching text patterns

  param
    ExpType = Anything | Value ;
    Brevity = B1 | B2 | B3 ;

  oper
    Exp : Type = {
      s : Str ;
      e : ExpType
      } ;
    ListExp = ListX ** {
      e : ExpType
      } ;

    BinOp : Type = {s : Brevity => Str} ;

    -- TODO: support "more/fewer than", "bigger/smaller than"
    binop : Str -> Str -> BinOp = \lt,less_than -> {
      s = table {
            B3 => lt ;
            _ => less_than }
    } ;
    or : BinOp -> BinOp -> BinOp = \lt,eq -> {
      s = table {
            B3 => lt.s ! B3 + eq.s ! B3 ;
            x  => lt.s ! x ++ "or" ++ eq.s ! x }
    } ;

    conjStr : Str -> ListX -> SS = \and -> conjunctSS (ss and) ;

--------------------------
-- Building expressions --
--------------------------
  rowHorn,
  rowIfThen : Brevity -> (rownum : SS) -> (input,output : SS) -> (comment : SS) -> SS ;

  rowHorn brev = case brev of {
    B1 => rowHornBrev1 ;
    B2 => rowHornBrev2 ;
    B3 => rowHornBrev3} ;

  rowIfThen brev = case brev of {
    B1 => rowBrev1 ;
    B2 => rowBrev2 ;
    B3 => rowBrev3} ;

  rowBrev1,
  rowBrev2,
  rowBrev3,
  rowHornBrev1,
  rowHornBrev2,
  rowHornBrev3 : (rownum : SS) -> (input,output : SS) -> (comment : SS) -> SS ;

  rowBrev1 num inputs outputs comments = {
    s = "when the" ++ inputs.s ++ "the" ++ outputs.s ++ comments.s
      ++ "(#" ++ BIND ++ num.s ++ BIND ++ ")" ;
    } ;

  rowBrev2 num inputs outputs comments = {
    s = "when the" ++ inputs.s ++ "the" ++ outputs.s ++ comments.s ;
    } ;

  rowBrev3 num inputs outputs comments = {
    s = "when the" ++ inputs.s ++ "the" ++ outputs.s ++ comments.s ;
    } ;


  rowHornBrev1 num inputs outputs comments = {
    s = "#" ++ BIND ++ num.s 
      ++ "the" ++ outputs.s ++ "when the" ++ inputs.s ++ comments.s ;
    } ;

  rowHornBrev2 num inputs outputs comments = {
    s = "the" ++ outputs.s ++ "when the" ++ inputs.s ++ comments.s ;
    } ;

  rowHornBrev3 num inputs outputs comments = {
    s = "the" ++ outputs.s ++ "when the" ++ inputs.s ++ comments.s ;
    } ;

}
