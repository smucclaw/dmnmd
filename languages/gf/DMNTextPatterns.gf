resource DMNTextPatterns = open Prelude, Coordination in {
-- Resource module for mixing and matching text patterns

  param
    -- Naming convention: param value prefixed with the first letter of its type
    ExpType = EAnything | EValue ValType | ERange | ESection | EList ;
    ValType = VTrue | VFalse | VValue ;
    HeaderType = HAttribute -- catch-all case
               | HLocation | HTime | HDuration | HSpeed
               | HAmountCount | HAmountMass
               | HSize | HWeight | HHeight | HLength
               ;
    Role = Input | Output ;
    Brevity = B1 | B2 | B3 ;
    Order = IfThen | ThenIf ;

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

    Cell : Type = {
      s : Role => Brevity => Str
      } ;

    ListCell : Type = ListTable2 Role Brevity ;

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

    or : BinOp -> BinOp -> BinOp = \lt,eq -> \\b,h =>
      case <b,h> of {
        <B3,x> => <(lt ! B3 ! x).p1 + (eq ! B3 ! x).p1, []> ;
        _ => <(lt ! b ! h).p1 ++ "or" ++ (eq ! b ! h).p1, (lt ! b ! h).p2>
      } ;

    and_Conj : SS = {s="and"} ;
    or_Conj : SS = {s="or"} ;
    conjTable : Order -> Str -> ListRow -> SS = \o,sep,rows ->
      ss ((conjunctTable Order (ss sep) rows).s ! o) ;

--------------------------
-- Building expressions --
--------------------------

  -- All cell types have the same output
  output : Brevity -> (header : SS) -> Exp -> Str = \b,hdr,exp -> case exp.t of {
    EAnything => "for any" ++ hdr.s ; -- nonExist ; -- In standard DMN, wildcard can't be output
    _ => table {
          --B3 => exp.s ! B3 ! HAttribute ;
          _ => "the" ++ hdr.s ++ "is" ++ exp.s ! b ! HAttribute
          } ! b
    } ;


}
