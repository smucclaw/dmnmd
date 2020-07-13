resource DMNParams = open Prelude, Coordination in {
-- Resource module for mixing and matching text patterns

  param
    -- Naming convention: param value prefixed with the first letter of its type
    ExpType = EAnything | EValue ValType | ERange | ESection | EList ;
    ValType = VTrue | VFalse | VString | VNum Number ;
    Number = Singular | Plural ;
    HeaderType = HAttribute -- catch-all case
      | HLocation | HTime | HDuration | HSpeed
      | HAmountCount | HAmountMass
      | HSize | HWeight | HHeight | HLength
      
      -- not actual headers, but needed in FCell
      | HFalse | HTrue 
      | HAnything
      ;
    Role = Input | Output ;
    Brevity = B1 | B2 | B3 ;
    Order = IfThen | ThenIf ;
    ConjType = UseConj | NoConj ;

  oper
    Val : Type = {
      s : Str ;
      t : ValType
      } ;

    Exp : Type = {
      s : Brevity => HeaderType => Str ;
      t : ExpType
      } ;

    ListExp = ListTable2 Brevity HeaderType ** {
      t : ExpType
      } ;

    -- Cell : Type = {
    --   s :  Brevity => {s : S ; adv : Adv} ;
    --   } ;

    Row : Type = {
      s : Order => Str
      } ;

    ListRow : Type = ListTable Order ;

    Tuple : Type -> Type = \A -> {p1, p2 : A} ;

    BinOp : Type = Brevity => HeaderType => Tuple Str ; -- Discontinuous: <"more", "than">

    binop : Str -> (HeaderType=>Str) -> BinOp = \lt,hStrTable ->
      table { B3 => table {_ => split <lt:Str>} ;               -- The shortest brevity: just use symbols like <, =
              _  => table {x => split (hStrTable ! x)}    -- Different expressions depending on HeaderType
      } where {
          split : Str -> Tuple Str = \str -> case str of {
            much + " " + more + " " + than => <much ++ more, than> ;
            less + " " + than              => <less, than> ;
            x                              => <x, "">
            } ;
        } ;

    conjTable : Order -> Str -> ListRow -> SS = \o,sep,rows ->
      ss ((conjunctTable Order (ss sep) rows).s ! o) ;

    headerType : HeaderType -> HeaderType -> HeaderType =
      \ht1,ht2 -> case <ht1,ht2> of {
        <HTrue,HTrue> => HTrue ;
        <HFalse,HFalse> => HFalse ;
        <HAnything,y> => y ;
        <x,y> => x } ;
    
    eqHeaderType : HeaderType -> HeaderType -> Bool =
      \ht1,ht2 -> case <ht1,ht2> of {
        <HAttribute,HAttribute>|
        <HLocation,HLocation>|
        <HTime,HTime>|
        <HDuration,HDuration>|
        <HSpeed,HSpeed>|
        <HAmountCount,HAmountCount>|
        <HAmountMass,HAmountMass>|
        <HSize,HSize>|
        <HWeight,HWeight>|
        <HHeight,HHeight>|
        <HLength,HLength>|
        <HTrue,HTrue>|
        <HFalse,HFalse>|
        <HAnything,HAnything> => True ;
        _                     => False } ;

}
