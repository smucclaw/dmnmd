incomplete concrete DMNBase of DMN =
  WordnetNPsI
--  , Numeral
   ** open
  Prelude,
  Coordination,
  Syntax,
  Grammar,
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
    Cells : Type = {
      s : Brevity => {s : S ; adv : Adv} ;
      } ;
    Cell : Type = Cells ** {
      h : HeaderType ; -- for coordination
      } ;
    ListCell : Type = {
      s : Brevity => {s : [S] ; adv : [Adv]} ;
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
    True  = {s = "True" ; t = VTrue} ;
    False = {s = "False" ; t = VFalse} ;

    -- : DMNVal
    VOne = {s = "1" ; t = VNum Singular} ; -- for linguistic accuracy
    -- : String -> DMNVal ;
    VS str = str ** {t = VString} ;
    -- : Float -> DMNVal ;
    VN flt = flt ** {t = VNum Plural} ;
    -- : Bool -> DMNVal ;
    VB bool = bool ;

    NoComment = ss "" ;       -- : Comment
    CommentString = parenss ; -- : String -> Comment

    -- FEEL expressions
    FAnything = {                -- : FEELexp ;
      s = \\_,_ => [] ;
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
        _  => \\_ => between bg end} ; -- TODO: depend on headertype
      t = ERange ;
      } ;
    FSection op val = {  -- : FBinOp -> DNMVal -> FEELexp ;
      s = \\b,h =>
            let opS : Tuple Str = op ! b ! h
             in opS.p1 ++ val.s ++ opS.p2 ; -- TODO depend on headertype
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
         => twoTable2 Brevity HeaderType e1 e2 ** {t = EAnything};
       _ => twoTable2 Brevity HeaderType e1 e2 ** {t = EList}
     } ;

    ConsFEELexp e es = lin ListFEELexp (
      case <e.t, es.t> of {
       <_,EAnything>|<EAnything,_>
          => consTable2 Brevity HeaderType comma es e ** {t = EAnything} ;
        _ => consTable2 Brevity HeaderType comma es e ** {t = EList}
      }) ;

    Disj es = case es.t of {
      EAnything => FAnything ;
      x => conjunctDistrTable2 Brevity HeaderType or_Conj es ** {t = x}
      } ;

    -- : CN -> FEELexp -> FCell ;
    Attribute =  -- Fallback: Header is FEELexp "Dish is Stew"
      headerWhen HAttribute with_Prep ;

    Event = header HAttribute upon_Prep ;

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
    \h,prep,hdr,exp -> let cell : Cell = header h prep hdr exp in
    cell ** {
      s = \\b => cell.s ! b ** {adv = mkAdv when_Subj ((cell.s ! b).s)}
    } ;


  header : HeaderType -> Prep -> CN -> FEELexp -> Cell =
    \h,prep,hdr,exp ->
    let f : CN -> NP = case exp.t of {
          EList|ERange|EValue (VNum Plural) => thePl ;
          _ => theSg } ;
    in {s = \\b => case exp.t of {
              EAnything
                => any prep hdr ;
              _ =>
                let expNP : NP = symbNP b h exp ;
                    s : S = mkS (mkCl (f hdr) expNP) ;
                    s' : S = case b of {
                      B3 => np2s expNP ;
                      _ => s} ;
                in {s = s' ; adv = mkAdv prep expNP} } ;
        h = h
        } ;

  headerCount : HeaderType -> Prep -> (number,apple : CN) -> FEELexp -> Cell =
    \h,prep,nbr,guest,exp ->
    let number_of_guests : CN = partCN nbr guest ;
        five_to_eight : Brevity=>Det = \\b =>
          case exp.t of {
            EList|
            ERange|
            EValue (VNum Plural) => aPl_Det ; -- Override for different langs
            _                    => a_Det } ** {s = exp.s ! b ! h} ;
    in {s = \\b => case exp.t of {
              EAnything
                => any prep number_of_guests ;
              _ => let s : S = mkS (mkCl (mkNP (five_to_eight ! b) guest)) ;
                   in {s = s ; adv = mkAdv prep (mkNP (five_to_eight ! b) guest) }} ;
        h = h ;
    } ;


    any : Prep -> (header : CN) -> {s : S ; adv : Adv} = \upon,hdr ->
      {s = mkS (mkCl (mkNP hdr) anything_NP) ; --nonExist ; -- In standard DMN, wildcard can't be output
       adv = Syntax.mkAdv upon (anyNP hdr)} ;

    anyNP : CN -> NP = mkNP anySg_1_Det ;

    symbNP : Brevity -> HeaderType -> Exp -> NP = \b,h,exp ->
      Symbolic.symb (exp.s ! b ! h) ;

    partCN : CN -> CN -> CN = \number,apple ->
      mkCN number (mkAdv part_Prep (mkNP aPl_Det apple)) ;

-------------------
-- List of cells --
-------------------
lin
    BaseFCell c1 c2 = {
      s = \\b =>
        let c1s : {s : S ; adv : Adv} = c1.s ! b ;
            c2s : {s : S ; adv : Adv} = c2.s ! b ;
         in {s = mkListS c1s.s c2s.s ;
             adv = mkListAdv c1s.adv c2s.adv} ;
      conjType = case eqHeaderType c1.h c2.h of {
        True => UseConj ;
        False => NoConj } ;
      } ;

    ConsFCell c cs = {
      s = \\b =>
        let c1s : {s : S ; adv : Adv} = c.s ! b ;
            c2s : {s : [S] ; adv : [Adv]} = cs.s ! b ;
         in {s = mkListS c1s.s c2s.s ;
             adv = mkListAdv c1s.adv c2s.adv} ;
      conjType = UseConj ; -- always use conj for longer lists than 2
      } ;

    -- : [FCell] -> FCells ;
    Many cells =
      let conj : Conj = case cells.conjType of {
            UseConj => and_Conj ;
            NoConj => and_Conj ** {s1,s2 = []} } ; -- Override for other langs
      in {
        s = \\b => let cs : {s : [S] ; adv : [Adv]} = cells.s ! b in {
          s = mkS conj cs.s ;
          adv = mkAdv conj cs.adv}
      } ;

    -- : FCell -> FCells ;
    Single cell = cell ;

------------------
-- List of rows --
------------------
   -- : Int -> FCells -> FCells -> Comment -> DTRow ;
   Row rownum inputs outputs comments =
    let num : Str = table {B1 => "#" ++ BIND ++ rownum.s ; _ => []} ! brev ;
        input : Adv = (inputs.s ! brev).adv ;
        output : S = (outputs.s ! brev).s ;
        inputOutput : S = Grammar.ExtAdvS input output ;
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
