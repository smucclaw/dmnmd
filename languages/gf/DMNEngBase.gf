incomplete concrete DMNEngBase of DMN = open
  Prelude,
  Coordination,
  DMNTextPatterns in {

  lincat
    -- Other lincats are {s : Str}, compiler inserts automatically
    -- Rest defined in DMNTextPatterns
    DTRow = Row ;
    [DTRow] = ListRow ;
    FCell,
    FCells = Cell ;
    [FCell] = ListCell ;
    [FEELexp] = ListExp ;
    FEELexp = Exp ;
    Bool,
    DMNVal = Val ;
    FBinOp = BinOp ;

  lin
    -- Binary operations
    Feq = binop "=" (table { -- Omitted in most cases: "X is Y" instead of "X is as much as Y"
            HWeight => "as heavy as" ;
            HHeight => "as tall as" ;
            HLength => "as long as" ;
            HAmountCount => "as many as" ;
            HAmountMass => "as much as" ;
            HSize => "as large as" ;
            HSpeed => "as fast as" ;
            HTime => "exactly" ;
            _ => "equal to"}) ;
    Flt = binop "<" (table {
            HWeight => "lighter than" ;
            HHeight => "shorter than" ;
            HLength => "shorter than" ;
            HAmountCount => "fewer than" ;
            HAmountMass => "less than" ;
            HSize => "smaller than" ;
            HSpeed => "slower than" ;
            HTime => "less than" ; -- ?
            _ => "less than"}) ;
    Fgt = binop ">" (table {
            HWeight => "heavier than" ;
            HHeight => "taller than" ;
            HLength => "longer than" ;
            HAmountCount => "more than" ;
            HAmountMass => "more than" ;
            HSize => "larger than" ;
            HSpeed => "faster than" ;
            HTime => "longer than" ; -- ?
            _ => "greater than"}) ;
    -- Flte = or Flt Feq ;
    -- Fgte = or Fgt Feq ;

    -- Booleans
    True  = {s = "True" ; t = VTrue} ;
    False = {s = "False" ; t = VFalse} ;

    -- : String -> DMNVal ;
    VS str = str ** {t = VValue} ;
    -- : Float -> DMNVal ;
    VN flt = flt ** {t = VValue} ;
    -- : Bool -> DMNVal ;
    VB bool = bool ;

    NoComment = ss "" ;       -- : Comment
    CommentString = parenss ; -- : String -> Comment

    -- FEEL expressions
    FAnything = {                -- : FEELexp ;
      s = \\_,_ => "anything" ;
      t = EAnything
      } ;
    FNullary val = {             -- : DMNVal -> FEELexp ;
      s = \\_,_ => val.s ;
      t = EValue val.t
      } ;
    FInRange,
    FInRangeInt = \bg,end -> {          -- : Float  -> Float  -> FEELexp ;
      s = table {
        B3 => \\_ => bg.s ++ "-" ++ end.s ; -- postprocess or use BIND to remove spaces
        _  => \\_ => "between" ++ bg.s ++ "and" ++ end.s } ; -- TODO: depend on headertype
      t = ERange ;
      } ;
    FSection op val = {  -- : FBinOp -> DNMVal -> FEELexp ;
      s = \\b,h =>
            let opS : Tuple Str = op ! b ! h
             in opS.p1 ++ val.s ++ opS.p2 ; -- TODO depend on headertype
      t = ESection ;
    } ;

---------------------
-- List of FEELexp --
---------------------
    BaseFEELexp e1 e2 =
     case <e1.t, e2.t> of {
       -- If any subexpression is Anything, the whole expression is Anything.
       <_,EAnything>|
       <EAnything,_> => twoTable2 Brevity HeaderType e1 e2 ** {t = EAnything} ; -- Replaced with just one FAnything later.
       <_,_> => twoTable2 Brevity HeaderType e1 e2 ** {t = EList}
     } ;

    ConsFEELexp e es =
      case <e.t, es.t> of {
       <_,EAnything>|
       <EAnything,_> => consTable2 Brevity HeaderType comma es e ** {t = EAnything} ;
        _ => consTable2 Brevity HeaderType comma es e ** {t = EList}
      } ;

    Disj es = case es.t of {
      EAnything => FAnything ;
      x => conjunctTable2 Brevity HeaderType or_Conj es ** {t = x}
      } ;

    -- : String -> FEELexp -> FCell ;
    Attribute hdr exp = {  -- Fallback: Header is FEELexp "Dish is Stew"
      s = \\r,b => case r of {
        Input => case exp.t of {
                    EAnything => "with any" ++ hdr.s ;
                    _ => "when the" ++ hdr.s ++ "is" ++ exp.s ! b ! HAttribute } ;
        Output => output b hdr exp }
      } ;

    Event hdr exp = {
      s = \\r,b => case r of {
        Input => case exp.t of {
                    EAnything => "upon any" ++ hdr.s ;
                    _ => "upon" ++ exp.s ! b ! HAttribute } ;
        Output => output b hdr exp }

      } ;

    Location,                   -- {City,Paris} ~ "In Paris"
    TimeSeason = \hdr,exp -> {  -- {Month,May} ~ "In May"
      s = \\r,b => case r of {
        Input  => case exp.t of {
                    EAnything => "in any" ++ hdr.s ; -- in any city/month
                    _ => "in" ++ exp.s ! b ! HLocation } ;
        Output => output b  hdr exp
        }
      } ;

    TimeClock hdr exp = { -- {Time,14:00} ~ "At 2 PM", "At any Time"
      s = \\r,b => case r of {
        Input  => case exp.t of {
                    EAnything => "at any" ++ hdr.s ; -- in any month
                    _ => "at" ++ exp.s ! b ! HLocation } ;
        Output => output b hdr exp
        }
      } ;

    AmountCount hdr1 hdr2 exp = { -- {XCount,=<10} ~ "With 10 or fewer Xs"
      s = \\r,b => case r of {
        Input => case exp.t of {
                   EAnything => "for any number of" ++ (glue hdr1.s "s") ;
                   _ => "for" ++ exp.s ! b ! HAmountCount ++ (glue hdr1.s "s") } ; -- TODO actual inflection
        Output => output b (cc2 hdr1 hdr2) exp
        }
      } ;
    -- Duration,   -- {Weeks,[3..5]} ~ "Between 3 and 5 Weeks"
    -- Weight,     -- {XWeight,3 kg} ~ "X weighs 3 kg"
    -- Length,     -- {XLength,3 m} ~ "X is 3 m long"
    -- Height,     -- {XHeight,3 m} ~ "X is 3 m tall"
    -- AmountMass, -- {CupsOfX,5} ~ "With 5 Cups of X". NB. the header needs to contain the unit and the material.

-------------------
-- List of cells --
-------------------
lin
    BaseFCell = twoTable2 Role Brevity ;
    ConsFCell = consrTable2 Role Brevity comma ;
    -- : [FCell] -> FCells ;
    Many = conjunctTable2 Role Brevity and_Conj ;
    -- : FCell -> FCells ;
    Single cell = cell ;

------------------
-- List of rows --
------------------
   -- : Int -> FCells -> FCells -> Comment -> DTRow ;
   Row rownum inputs outputs comments =
    let num : Str = table {B1 => "#" ++ BIND ++ rownum.s ; _ => []} ! brev ;
        input : Str = inputs.s ! Input ! brev ;
        output : Str = outputs.s ! Output ! brev ;
    in {s = table {
            ThenIf => num ++ output ++ input ++ comments.s ;
            IfThen => num ++ input ++ output ++ comments.s }
       } ;

    BaseDTRow = twoTable Order ;
    ConsDTRow = consrTable Order tablesep ; -- tablesep defined in each concrete

    -- : [DTRow] -> DTable ;
    Table = conjTable order tablesep ;

    -- This is for cases like
    -- "(output) The contract is terminated, (input) when X, Y and Z."
    -- "(input) When the contract is terminated, (output) the Company's obligations are A, B and C."
    Consequence rows = {
       s = rows.s2 ! ThenIf ++ tablesep ++ rows.s1 ! IfThen
     } ;

}
