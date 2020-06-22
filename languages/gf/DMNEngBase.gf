incomplete concrete DMNEngBase of DMN = open
  Prelude,
  Coordination,
  DMNTextPatterns in {

  lincat
    -- Other lincats are {s : Str}, compiler inserts automatically
    [DTRow],
    [FCell] = ListX ; -- Defined in Coordination
    -- Rest defined in DMNTextPatterns
    [FEELexp] = ListExp ;
    FEELexp = Exp ;
    FBinOp = BinOp ;

  lin
    -- Binary operations
    Feq = binop "=" "equal to" ;
    Flt = binop "<" "less than" ;
    Fgt = binop ">" "greater than" ;
    Flte = or Flt Feq ;
    Fgte = or Fgt Feq ;

    -- Booleans. We only need strings, that's why we hide Prelude.Bool.
    -- This is a bit silly. :-P
    True = ss "True" ;
    False = ss "False" ;

    -- : String -> DMNVal ;
    VS str = str ;
    -- : Float -> DMNVal ;
    VN flt = flt ;
    -- : Bool -> DMNVal ;
    VB bool = bool ;

    NoComment = ss "" ;       -- : Comment
    CommentString = parenss ; -- : String -> Comment

    -- FEEL expressions
    FAnything = {s = "anything" ; e = Anything} ;  -- : FEELexp ;
    FNullary val = val ** {      -- : DMNVal -> FEELexp ;
      e = Value ; -- Record extension: use the s field of val, and add e field
      } ;
    FInRange bg end = {          -- : Float  -> Float  -> FEELexp ;
      s = "between" ++ bg.s ++ "and" ++ end.s ;
      e = Value ;
      } ;
    FSection op val = {  -- : FBinOp -> DNMVal -> FEELexp ;
      s = op.s ! B1 ++ val.s ;
      e = Value ;
    } ;

---------------------
-- List of FEELexp --
---------------------
    BaseFEELexp e1 e2 =
     case <e1.e, e2.e> of {
       <Value,Value> => twoSS e1 e2 ** {e = Value} ;
       -- If any subexpression is Anything, the whole expression is Anything.
       _ => twoSS e1 e2 ** {e = Anything} -- Replaced with just one FAnything later.
     } ;

    ConsFEELexp e es =
      case <e.e, es.e> of {
        <Value,Value> => consrSS comma e es ** {e = Value} ;
        _ => consrSS comma e es ** {e = Anything}
      } ;

    Disj es = case es.e of {
      Anything => FAnything ;
      Value => conjStr "or" es ** {e = Value}
      } ;

    -- FEELexp to FCell
    ColHd = infixSS "is" ;

-------------------
-- List of cells --
-------------------
    BaseFCell = twoSS ;
    ConsFCell = consrSS comma ;
    -- : [FCell] -> FCells ;
    Many = conjStr "and" ;
    -- : FCell -> FCells ;
    Single cell = cell ;

------------------
-- List of rows --
------------------

    BaseDTRow = twoSS ;
    ConsDTRow = consrSS tablesep ; -- tablesep defined in each concrete
    -- : [DTRow] -> DTable ;
    Table = conjStr tablesep ;

}
