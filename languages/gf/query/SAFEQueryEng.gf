concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open
  Prelude,
  (R=ResEng),
  (E=ExtendEng),
  (C=ConjunctionEng),
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  VerbEng,
  AdjectiveEng in {

  lincat
    Action = LinAction ; -- Negations affect more than standard RGL negation does
    'Action/Dir' = SlashDir ;
    'Action/Indir' = SlashIndir ;
    'Action/Dir/Indir' = SlashDirIndir ;
    [Action] = ListLinAction ;
    ['Action/Dir'] = ListSlashDir ;
    ['Action/Indir'] = ListSlashIndir ;
    ['Action/Dir/Indir'] = ListSlashDirIndir ;

    Temporality = Tense ;
    Polarity = Pol ;

  linref
    Action = linAction ;

  lin
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise = mkDir raise_V2 ;
    Issue = mkDir issue_V2 ;
    Sell  = mkDir sell_V2 ;

    -- Indirect object
    IssueAt = mkDirIndir issue_at_V3 whether_at_Prep ;
    SellAt = mkDirIndir sell_at_V3 whether_at_Prep ;

    -- Complements
    -- : 'Action/Dir' -> Term -> Action ;
    AComplDir = complDir ;
    -- : 'Action/Indir' -> Term -> Action ;
    AComplIndir = complIndir ;
    -- : 'Action/Dir/Indir' -> Term -> 'Action/Indir' ; -- sell stock (at fixed valuation)
    ASlashDir = slashDir ;
    -- : 'Action/Dir/Indir' -> Term -> 'Action/Dir' ;   -- sell (stock) at fixed valuation
    ASlashIndir = slashIndir ;

    -- Conjunctions
    BaseAction a1 a2 = {
      s = \\t,p => E.BaseVPS (a1.s ! t ! R.CPos) (a2.s ! t ! R.CPos) ; -- "doesn't sell and issue"
      gerund = \\p => mkListAdv (a1.gerund ! R.CPos) (a2.gerund ! R.CPos)
      } ;
    ConsAction a as = {
      s = \\t,p => E.ConsVPS (a.s ! t ! R.CPos) (as.s ! t ! R.CPos) ;
      gerund = \\p => mkListAdv (a.gerund ! R.CPos) (as.gerund ! R.CPos)
      } ;
    ConjAction co as = {
      s = \\t,p =>  let conj : Conj = co ! cpol2pol p in E.ConjVPS conj (as.s ! t ! p) ;
      gerund = \\p => let conj : Conj = co ! cpol2pol p in
        SyntaxEng.mkAdv conj (as.gerund ! p)
      } ;

    'BaseAction/Dir' a1 a2 =
      let a1' : LinAction = complDir a1 emptyNP ;
          a2' : LinAction = complDir a2 emptyNP ;
      in BaseAction a1' a2' ** {
        dir = a1.dir ; -- : PrepPol
        indir = \\p => emptyAdv ; -- the existing indir has been incorporated in a1' and a2'
      } ;
    'ConsAction/Dir' a as =
      let a' : LinAction = complDir a emptyNP ;
      in ConsAction a' <as:ListLinAction> ** {
        dir = as.dir ; -- : PrepPol
        indir = \\p => emptyAdv
        } ;
    ConjSlashDir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    'BaseAction/Indir' a1 a2 =
      let a1' : LinAction = complIndir a1 emptyNP ;
          a2' : LinAction = complIndir a2 emptyNP ;
      in BaseAction a1' a2' ** {
        indir = a1.indir ; -- : PrepPol
       dir = \\p => emptyAdv ; -- the existing dir has been incorporated in a1' and a2'
      } ;
    'ConsAction/Indir' a as =
      let a' : LinAction = complIndir a emptyNP ;
      in ConsAction a' <as:ListLinAction> ** {
        indir = as.indir ; -- : PrepPol
        dir = \\p => emptyAdv
        } ;

    ConjSlashIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    'BaseAction/Dir/Indir' a1 a2 = BaseAction a1 a2 ** {
      dir = a2.dir ;
      indir = a2.indir
      } ;
    'ConsAction/Dir/Indir' a as = ConsAction a as ** {
      dir = as.dir ;
      indir = as.indir
      } ;
    ConjSlashDirIndir co as = ConjAction co as ** {
      dir = as.dir ;
      indir = as.indir
      } ;

    -- : Term -> Action -> Move ; -- the company raises capital
    MAction temp pol t a = mkText (mkUtt (cl temp pol t a)) fullStopPunct ;

    TPresent = presentTense ;
    TPast = pastTense ;
    PPositive = positivePol ;
    PNegative = negativePol ;

  oper
    LinAction : Type = {
      s : R.Tense => R.CPolarity => E.VPS ;
      gerund : R.CPolarity => Adv ;
      } ;

    ListLinAction : Type = {
      s : R.Tense => R.CPolarity => E.ListVPS ;
      gerund : R.CPolarity => [Adv] ;
      } ;

    linAction : LinAction -> Str = \l ->
      (mkUtt (cl presentTense positivePol emptyNP l)).s ;

    mkVPS : R.Tense -> R.CPolarity -> VP -> E.VPS = \t,p ->
      let tense : Tense = lin Tense {s=[] ; t=t} ;
          pol : Pol = lin Pol {s=[] ; p=p} ;
       in E.MkVPS (mkTemp tense simultaneousAnt) pol ;

    ----------------------
    -- Slash categories --
    ----------------------

    mkGerS : V2 -> LinAction = \v2 -> {
      s = \\t,p => mkVPS t p (mkVP <v2:V2> emptyNP) ;
      gerund = \\p => E.GerundAdv (mkVP <v2:V2> emptyNP) ; -- TODO proper treatment of polarity
      } ;

    -- /Dir
    SlashDir : Type = LinAction ** {
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol
      } ;
    mkDir : V2 -> SlashDir = \v2 -> mkGerS v2 ** {
      dir = prepPol v2.c2 ;
      indir = \\_ => emptyAdv ;
      } ;
    slashDir : SlashDirIndir -> LinTerm -> SlashIndir = \vps,do -> vps ** {
      dir = applyPrepPol vps.dir (np do)
      } ;
    complDir : SlashDir -> LinTerm -> LinAction = \vps,do -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (applyPrepPol vps.dir (np do) ! p)
                          (vps.indir ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (applyPrepPol vps.dir (np do) ! p)
                            (vps.indir ! p) ;
      } ;

    -- /Indir
    SlashIndir : Type = LinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/won't sell) some/any stock
      indir : PrepPol ;
      } ;
    mkIndir : V2 -> SlashIndir = \v2 -> mkGerS v2 ** {
      dir = \\_ => emptyAdv ;
      indir = prepPol v2.c2 ;
      } ;
    slashIndir : SlashDirIndir -> LinTerm -> SlashDir = \vps,io -> vps ** {
      indir = applyPrepPol vps.indir (np io)
      } ;
    complIndir : SlashIndir -> LinTerm -> LinAction = \vps,io -> vps ** {
      s = \\t,p => complS (vps.s ! t ! p)
                          (vps.dir ! p)
                          (applyPrepPol vps.indir (np io) ! p) ;
      gerund = \\p => complGer (vps.gerund ! p)
                            (vps.dir ! p)
                            (applyPrepPol vps.indir (np io) ! p)
      } ;


    -- /Dir/Indir
    SlashDirIndir : Type = LinAction ** {
      dir,
      indir : PrepPol ;
      } ;
    mkDirIndir = overload {
      mkDirIndir : V3 -> SlashDirIndir = \v3 -> mkGerS v3 ** {
        dir = prepPol v3.c2 ;
        indir = prepPol v3.c3
        } ;
      mkDirIndir : V3 -> PrepPol -> SlashDirIndir = \v3,indir -> mkGerS v3 ** {
        indir = indir ;
        dir = prepPol v3.c2
        }
      } ;
    -- PrepPol is more powerful than Prep: prepared for multilayer negations
    PrepPol : Type = R.CPolarity => PrepPlus ;
    PrepPlus : Type = {  -- Positive version  / Negative version
      s : Str ;      -- at (fixed valuation) / whether at (fixed valuation)
      post : Str ;   -- ∅                    / or without
      redupl : Bool  -- False                / True       (fixed valuation)
      } ;

    prepPol = overload {
      prepPol : Str -> PrepPol = \p -> \\pol => {
        s = p ;
        post = [] ;
        redupl = False
        } ;
      prepPol : (p,n : PrepPlus) -> PrepPol = \pos,neg -> table {
        R.CPos   => pos ;
        R.CNeg _ => neg
        }
      } ;

    prepPlus : (s,post : Str) -> (redupl : Bool) -> PrepPlus = \s,post,r -> {
      s = s ;
      post = post ;
      redupl = r
      } ;

    applyPrepPol : PrepPol -> NP -> (R.CPolarity=>Adv) = \pp,np -> \\pol =>
      let npacc : Str = np.s ! R.NPAcc ;
          prep : PrepPlus = pp ! pol
      in lin Adv {
        s = prep.s ++ npacc ++ prep.post ++ case prep.redupl of {
                                                True => npacc ;
                                                False => [] }
      } ;

    -- helpers for complDir and complIndir
    complS : E.VPS -> Adv -> Adv -> E.VPS = \vps,dir,indir -> lin VPS {
      s = \\a => vps.s ! a ++ dir.s ++ indir.s
      } ;
    complGer : (a,b,c : Adv) -> Adv = \ger,indir,dir -> lin Adv {
      s = ger.s ++ dir.s ++ indir.s
      } ;

    -------------------
    -- List versions --
    -------------------
    ListSlashDir : Type = ListLinAction ** {
      indir : R.CPolarity => Adv ; -- at fixed valuation / whether at fv nor without fv
      dir : PrepPol ;
      } ;

    ListSlashIndir : Type = ListLinAction ** {
      dir : R.CPolarity => Adv ; -- (Acme will/won't sell) some/any stock
      indir : PrepPol ;
      } ;

    ListSlashDirIndir : Type = ListLinAction ** {
      dir,
      indir : PrepPol ;
      } ;

    ---------------------
    -- Generic helpers --
    ---------------------
    cl : Tense -> Polarity -> LinTerm -> LinAction -> S = \t,p,subj,pred ->
      let s : S = E.PredVPS (np subj) (pred.s ! t.t ! p.p)
       in s ** {s = s.s ++ t.s ++ p.s} ;
    -- This is silly, but I need to do it this way, because instead of VP, which is variable in
    -- tense and polarity, I'm storing /fully formed VPS/s in a table with R.Tense and R.CPolarity as LHS.
    -- (Why do I store VPS instead of VP? To be able to coordinate them.)
    -- When an abstract syntax value like TPresent or PPositive is used to choose the correct VPS,
    -- I need to use the s fields of those values, so that every argument contributes to the linearization.
    -- See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#metavariables-or-those-question-marks-that-appear-when-parsing

    gerund : LinAction -> NP = \pred ->
      let s : Str = (pred.gerund ! R.CPos).s in mkNP (mkN s s s s) ;

    cpol2pol : R.CPolarity -> R.Polarity = \p -> case p of {
      R.CPos => R.Pos ;
      R.CNeg _ => R.Neg
      } ;

  lin

    ----------------
    -- Properties --
    ----------------
    Fixed = prop "fixed" ;
    PreMoney = prop "pre-money" ;
    PostMoney = prop "post-money" ;
    BonaFide = prop "bona fide" ;
    Voluntary = prop "voluntary" ;
    Involuntary = prop "involuntary" ;

    -- : Term -> Property ; -- for the benefit of the Company's creditors
    ForBenefit t =
      adv2ap (adv for_Prep (mkNP the_Det (mkCN benefit_N2 (np t)))) ;

    -- : Action -> Property ; -- with the purpose of raising capital
    WithPurpose action =
      adv2ap (adv with_Prep
                  (mkNP the_Det
                        (mkCN purpose_N2 (gerund action))
                  )
             ) ;

    -----------
    -- Kinds --
    -----------
    Event = kind "event" ;
    Capital = kind "capital" ;

    DissolutionEvent = adjkind "dissolution" "event" ;
    Termination = ofkind "termination" "operations" ;
    GeneralAssignment = adjkind "general" "assignment" ;

    LiquidityEvent = adjkind "liquidity" "event" | kind "liquidity" ;
    ChangeOfControl = ofkind "change" "control" ;
    DirectListing = adjkind "direct" "listing" ;
    InitialPublicOffering = linkind (mkCN (prop "initial") (adjkind "public" "offering").cn) ;

    EquityFinancing = adjkind "equity" "financing" ;
    Transaction = kind "transaction" ;
    PreferredStock = adjkind "preferred" "stock" ;
    Valuation = kind "valuation" ;

  lincat
    'Kind/Term' = CN ;
    'ListKind/Term' = C.ListCN ;

  lin
    Liquidation = mkCN (mkN "liquidation") ;
    Dissolution = mkCN (mkN "dissolution") ;
    WindingUp = mkCN (mkN "winding up" "windings up") ;

    -- Complement goes to cn field, not to adv field.
    ComplKind cn t = linkind (mkCN cn (adv part_Prep (np t))) ;

    -- : [Property] -> Kind -> Kind
    KWhetherOr props kind =
      let prop : Adv = ap2adv (mkAP whether_or_Conj props) ;
      in kind ** {
        adv = cc2 kind.adv prop } ;

    'BaseKind/Term' = C.BaseCN ;
    'ConsKind/Term' = C.ConsCN ;
    ConjSlashTerm co = C.ConjCN (co ! R.Pos) ;

    -----------
    -- Terms --
    -----------
    -- : Term
    Company = mkNP theSg_Det (mkN "Company") ;

    -- : Term -> Term ;
    Creditors t =          -- the company's creditors
      mkNP (mkDet (ExtendEng.GenNP (np t)) pluralNum) creditor_N ;

    -- : Determiner -> Kind -> Term -> Term ;
    TExcluding the valuation t =
      let exclAdv : Adv = parenss (adv excluding_Prep (np t)) ; -- The adv "excluding post-money"
          valuation_excl : Kind = valuation ** {
            cn = AdvCN valuation.cn exclAdv  -- first layer: "valuation excluding post-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_excl ;

    -- : Determiner -> Kind -> Term -> Term ;
    TIncluding the valuation t = -- fixed valuation, including a pre-money or post-money valuation
      let inclAdv : Adv = adv including_Prep (np t) ; -- The adv "including pre-money"
          valuation_incl : Kind = valuation ** {
            cn = ExtAdvCN valuation.cn inclAdv  -- first layer: "valuation including pre-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the valuation_incl ;

    AnyOther = any_other_Det ;
    Series = series_Det ;

  oper
    -------------
    -- Lexicon --
    -------------
    any_other_Det : Det = a_Det ** {s = "any other"} ;
    series_Det : Det = aPl_Det ** {s = "a series of"} ;

    raise_V2 : V2 = mkV2 (mkV "raise") ;
    sell_V2 : V2 = mkV2 (mkV "sell") ;
    issue_V2 : V2 = mkV2 (mkV "issue") ;
    sell_at_V3 : V3 = mkV3 (mkV "sell") noPrep at_Prep ;
    issue_at_V3 : V3 = mkV3 (mkV "issue") noPrep at_Prep ;

    benefit_N2 : N2 = mkN2 (mkN "benefit") ;
    purpose_N2 : N2 = mkN2 (mkN ("purpose"|"principal purpose")) ;
    series_N2 : N2 = mkN2 (mkN "series" "series") ;
    creditor_N : N = mkN "creditor" ;

    whether_or_Conj : Conj = or_Conj ** {s1 = ", whether"} ;

    whether_at_Prep : PrepPol =
     prepPol
      {s = "at" ; post = [] ; redupl = False}
      {s = ", whether at" ; post = "or without" ; redupl = True} ;

    at_Prep : Prep = mkPrep "at" ;
    excluding_Prep : Prep = mkPrep "excluding" ;
    including_Prep : Prep =  -- endComma: disappears in front of other punctuation
      mkPrep ("including" ++ strOpt ("but not limited to" ++ Prelude.endComma)) ;


    ----------
    -- Misc --
    ----------
    linkind : CN -> LinKind = \cn -> {cn = cn ; adv = emptyAdv} ;

    adv : Prep -> NP -> Adv = SyntaxEng.mkAdv ; -- shorthand: mkAdv is imported from two modules, so it has to be qualified
    prop : Str -> AP = \a -> mkAP (mkA a) ;
    kind : Str -> LinKind = \n -> linkind (mkCN (mkN n)) ;
    adjkind : Str -> Str -> LinKind =
      \a,n -> linkind (mkCN (prop a) (mkN n)) ;
    ofkind : Str -> Str -> LinKind =
      \n,p -> linkind (mkCN (mkN n) (adv part_Prep (mkNP (mkN p)))) ;

    ExtAdvCN : CN -> Adv -> CN = \cn,ad -> cn ** {   -- RGL fun AdvCN doesn't put comma
      s = \\n,c => cn.s ! n ! c ++ "," ++ ad.s
      } ;

 }
