incomplete concrete DMNBase of DMN =
  WordnetNPsI
--  , Numeral
   ** open
  Prelude,
  Predef,
  Coordination,
  Syntax,
  Sentence,
  Extend,
  Symbolic,
  LexDMN,
  DMNParams in {

  lincat
    -- Other lincats are {s : Str}, compiler inserts automatically
    -- Rest defined in DMNParams
    DTRow = DMNParams.Row ;
    [DTRow] = ListRow ;
    FCell  = Cell ;
    FCells = Cells ;
    [FCell] = ListCell ;
    [FEELexp] = ListExp ;
    FEELexp = Exp ;
    Bool,
    DMNVal = Val ;
    FBinOp = BinOp ;
  oper
    HEA : Type = {hdr, exp : NP ; adv : Adv} ;
    ListHESA : Type = {hdr, exp : [NP] ; s : [S] ; adv : [Adv]} ;
    Cells : Type = {
      s : S ;
      adv : Adv
      } ;
    Cell : Type = {
      s : HEA ;
      h : HeaderType ; -- for coordination
      } ;
    ListCell : Type = {
      s : ListHESA ;
      conjType : ConjType ; -- TODO: more fine-grained later
      } ;

  lin
    -- Binary operations
    Feq = binop "=" LexDMN.feq ;
    Flt = binop "<" LexDMN.flt ;
    Fgt = binop ">" LexDMN.fgt ;
    Flte = binop "<=" LexDMN.flte ;
    Fgte = binop ">=" LexDMN.fgte ;

    -- Booleans
--    True  = {s = "True" ; t = VTrue} ;
--    False = {s = "False" ; t = VFalse} ;

    -- : DMNVal
    VNOne = {s = "1" ; t = VNum Singular} ; -- for linguistic accuracy
    -- : String -> DMNVal ;
    VS str = str ** {t = VString} ;
    -- : Float -> DMNVal ;
    VN flt = flt ** {t = VNum Plural} ;
    -- : Bool -> DMNVal ;
   -- VB bool = bool ;

    NoComment = ss "" ;       -- : Comment
    CommentString = parenss ; -- : String -> Comment

    -- FEEL expressions
    FAnything = {                -- : FEELexp ;
      s = \\_ => [] ;
      t = EAnything
      } ;
    FNullary val = {             -- : DMNVal -> FEELexp ;
      s = \\_ => val.s ;
      t = EValue val.t
      } ;
    FInRange,
    FInRangeInt = \bg,end -> {          -- : Float  -> Float  -> FEELexp ;
      s = case brev of {
        B3 => \\_ => bg.s ++ "-" ++ end.s ; -- postprocess or use BIND to remove spaces
        _  => \\_ => between bg end} ; -- TODO: depend on headertype
      t = ERange ;
      } ;
    FSection op val = {  -- : FBinOp -> DNMVal -> FEELexp ;
      s = \\h => -- TODO: depend on headertype
        let opS : Tuple Str = op ! brev ! h
        in opS.p1 ++ opS.p2 ++ val.s ; -- do we need discontinuity?
      t = case val.t of {
        VNum _ => EValue (VNum Plural) ; -- "one or more/fewer things"
        _ => EValue val.t }
    } ;

oper
  between : SS -> SS -> Str = \a,b ->
    let aNP : NP = symb a ;
        bNP : NP = symb b ;
     in (mkAdv between_Prep (mkNP and_Conj (mkListNP aNP bNP))).s ;


---------------------
-- List of FEELexp --
---------------------
lin
    BaseFEELexp e1 e2 =
     case <e1.t, e2.t> of {
       -- If any subexpression is Anything, the whole expression is Anything.
       <_,EAnything>|<EAnything,_> -- Replaced with just one FAnything later.
         => twoTable HeaderType e1 e2 ** {t = EAnything};
       _ => twoTable HeaderType e1 e2 ** {t = EList}
     } ;

    ConsFEELexp e es = lin ListFEELexp (
      case <e.t, es.t> of {
       <_,EAnything>|<EAnything,_>
          => consTable HeaderType comma es e ** {t = EAnything} ;
        _ => consTable HeaderType comma es e ** {t = EList}
      }) ;

    Disj es = case es.t of {
      EAnything => FAnything ;
      x => conjunctDistrTable HeaderType or_Conj es ** {t = x}
      } ;

    -- : CN -> FEELexp -> FCell ;
    Attribute =  -- Fallback: Header is FEELexp "Dish is Stew"
      headerWhen HAttribute with_Prep ;

    Event = header HAttribute upon_Prep ;

    IsTrue = headerBool HTrue ;
    IsFalse = headerBool HFalse ;

    Location,     -- {City,Paris} ~ "In Paris"
    TimeSeason =  -- {Month,May} ~ "In May"
      header HLocation in_Prep ;

    TimeClock = -- {Time,14:00} ~ "At 2 PM", "At any Time"
      header HLocation at_Prep ;

    AmountCount hdr1 hdr2 exp =  -- {XCount,=<10} ~ "With 10 or fewer Xs"
      headerCount HAmountCount for_Prep hdr1 hdr2 exp ;

    -- Duration,   -- {Weeks,[3..5]} ~ "Between 3 and 5 Weeks"
    -- Weight,     -- {XWeight,3 kg} ~ "X weighs 3 kg"
    -- Length,     -- {XLength,3 m} ~ "X is 3 m long"
    -- Height,     -- {XHeight,3 m} ~ "X is 3 m tall"
    -- AmountMass, -- {CupsOfX,5} ~ "With 5 Cups of X". NB. the header needs to contain the unit and the material.

oper

  headerWhen : HeaderType -> Prep -> CN -> FEELexp -> Cell =
    \h,prep,hdrCN,expr ->
    let cell : Cell = header h prep hdrCN expr in cell ** {
      s = cell.s ** {
        adv = mkAdv when_Subj (cell2s cell) }
    } ;


  header : HeaderType -> Prep -> CN -> FEELexp -> Cell =
    \h,prep,hdrCN,expr -> {
      s = case expr.t of {
        EAnything
          => any prep hdrCN ;
        _ => let expNP : NP = symbNP h expr in
          {hdr = det expr hdrCN ;
           exp = expNP ;
           adv = mkAdv prep expNP} } ;
      h = h
    } ;

  headerBool : HeaderType -> CN -> Cell = \h,hdrCN -> {
    s = let expNP : NP = mass hdrCN in -- hdr is informative, exp is just T/F
      {hdr = emptyNP ;
       exp = expNP ;
       adv = mkAdv when_Subj (mkS (mkCl (mkVP expNP)))} ;
    h = h
    } ;

  headerCount : HeaderType -> Prep -> (number,apple : CN) -> FEELexp -> Cell =
    \h,prep,nbr,guest,expr ->
    let number_of_guests : CN = partCN nbr guest ;
        five_to_eight : Det =
          case expr.t of {
            EList|ERange|EValue (VNum Plural)
              => aPl_Det ; -- Override for different langs
            _ => a_Det } ** {s = expr.s ! h} ;
    in {s = case expr.t of {
              EAnything
                => any prep number_of_guests ;
              _ => let expNP : NP = mkNP five_to_eight guest in
                   {hdr = emptyNP ; -- will use existential, no hdr
                    exp = expNP ; -- there are 5-8 guests
                    adv = mkAdv prep expNP }} ; -- with 5-8 guests
        h = h ;
    } ;

    det : FEELexp -> CN -> NP = \expr ->
      case expr.t of {
        EList|ERange|EValue (VNum Plural)
          => thePl ;
        _ => theSg } ;

    any : Prep -> (header : CN) -> HEA = \upon,hdrCN ->
      {hdr = mkNP hdrCN ; --nonExist ; -- In standard DMN, wildcard can't be output
       exp = anything_NP ;
       adv = Syntax.mkAdv upon (anyNP hdrCN)} ;

    anyNP : CN -> NP = mkNP anySg_1_Det ;

    symbNP : HeaderType -> Exp -> NP = \h,expr ->
      Symbolic.symb (expr.s ! h) ;

    partCN : CN -> CN -> CN = \number,apple ->
      mkCN number (mkAdv part_Prep (mkNP aPl_Det apple)) ;

    cell2s : Cell -> S = \c ->
      case <c.h,brev> of {
        <HFalse,_> => mkS negativePol (mkCl (mkVP c.s.exp)) ;
        <_,    B3> => np2s c.s.exp ;
        <HTrue, _> => mkS (mkCl (mkVP c.s.exp)) ;
        _          => mkS (mkCl c.s.hdr c.s.exp) -- Hdr is Exp
      } ;

-------------------
-- List of cells --
-------------------
lin
    BaseFCell c1 c2 = {
      s = {hdr = mkListNP c1.s.hdr c2.s.hdr ;
           exp = mkListNP c1.s.exp c2.s.exp ;
           s = mkListS (cell2s c1) (cell2s c2) ;
           adv = mkListAdv c1.s.adv c2.s.adv} ;
      conjType = case eqHeaderType c1.h c2.h of {
        True => UseConj ;
        False => NoConj } ;
      h = headerType c1.h c2.h ;
      } ;

    ConsFCell c cs =
      case c.h of {
        HAnything => cs ; -- don't add to list items that are Anything
        _ => {s = {hdr = mkListNP c.s.hdr cs.s.hdr ;
                   exp = mkListNP c.s.exp cs.s.exp ;
                   s = mkListS (cell2s c) cs.s.s;
                   adv = mkListAdv c.s.adv cs.s.adv} ;
              conjType = UseConj ; -- always use conj for longer lists than 2
              h = headerType c.h cs.h}
          } ;

    -- : [FCell] -> FCells ;
    Many cs =
      let conj : Conj = case cs.conjType of {
            UseConj => and_Conj ;
            NoConj => and_Conj ** {s1,s2 = []} } ; -- Override for other langs
      in {s = mkS and_Conj cs.s.s ;
          adv = mkAdv conj cs.s.adv} ;

    -- : FCell -> FCells ;
    Single cell = {
      s = cell2s cell ;
      adv = cell.s.adv
      } ;

------------------
-- List of rows --
------------------
   -- : Int -> FCells -> FCells -> Comment -> DTRow ;
   Row rownum inputs outputs comments =
    let num : Str = table {B1 => "#" ++ BIND ++ rownum.s ; _ => []} ! brev ;
        input : Adv = inputs.adv ;
        output : S = outputs.s ;
        inputOutput : S = Sentence.ExtAdvS input output ;
    in {s = table {
            ThenIf => num ++ (mkUtt output).s ++ (mkUtt input).s ++ comments.s ;
            IfThen => num ++ (mkUtt inputOutput).s ++ comments.s }
       } ;

    BaseDTRow = twoTable Order ;
    ConsDTRow = consrTable Order tablesep ; -- tablesep defined in each concrete


    -- TODO aggregation, e.g.:
    -- the dish is:
--     #1 Kidney bean stew in Winter
--     #2 Smoked tofu salad in Spring
--     #3 Roasted potatoes and a nice steak in Summer
--     #4 Instant noodles in Autumn for between 5 and 8 guests
--     #5 Pea soup in any season for any number of guests ( I give up )
    -- : [DTRow] -> DTable ;
    Table = conjTable order tablesep ;

    -- This is for cases like
    -- "(output) The contract is terminated, (input) when X, Y and Z."
    -- "(input) When the contract is terminated, (output) the Company's obligations are A, B and C."
    Consequence rows = {
       s = rows.s2 ! ThenIf ++ tablesep ++ rows.s1 ! IfThen
     } ;

}
