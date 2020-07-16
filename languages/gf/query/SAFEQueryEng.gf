concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open
  Prelude,
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  VerbEng,
  AdjectiveEng,
  ConjunctionEng,
  ExtendEng in {

  lincat
    Action = LinAction ;
    Determiner = Det ;

  linref
    Action = linAction ;

  lin

    -------------
    -- Actions --
    -------------
    -- : Term -> Action ; -- raising capital
    Raise = action2 raise_V2 ;
    Sell = action2 sell_V2 ;
    Issue = action2 issue_V2 ;
    SellAt t s = action3 sell_at_V3 s t ;
    IssueAt t s = action3 issue_at_V3 s t ;

    -- : Term -> Action -> Move ; -- the company raises capital
    MAction t a = mkText (mkUtt (cl t a)) fullStopPunct ;

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

    Liquidation t = kind "liquidation" ** {adv = adv part_Prep (np t)} ;
    Dissolution t = kind "dissolution" **  {adv = adv part_Prep (np t)} ;
    WindingUp t = linkind (mkCN (mkN "winding up" "windings up")) ** {adv = adv part_Prep (np t)} ;

    -- : [Property] -> Kind -> Kind
    KWhetherOr props kind =
      let prop : Adv = ap2adv (mkAP whether_or_Conj props) ;
      in kind ** {
        adv = cc2 kind.adv prop } ;
    --cn = mkCN prop kind.cn} ;

    -----------
    -- Terms --
    -----------
    -- : Term
    Company = {
      cn = mkCN (mkN "Company") ;
      det = theSg_Det
      } ;

    -- : Term -> Term ;
    Creditors t = t ** {         -- the company's creditors
      det = mkDet (ExtendEng.GenNP (np t)) pluralNum ;
      cn = mkCN creditor_N
      } ;

    -- : Kind -> Term ;
    TAnyOther = term any_other_Det ;
    TSeries kind = TPlIndef (kind ** {cn = mkCN series_N2 (mkNP aPl_Det kind.cn)}) ;

    -- : Determiner -> Kind -> Term -> Term ;
    TExcluding the valuation t =
      let exclAdv : Adv = parenss (adv excluding_Prep (np t)) ; -- The adv "excluding post-money"
          valuation_excl : Kind = valuation ** {
            cn = AdvCN valuation.cn exclAdv  -- first layer: "valuation excluding post-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the.det valuation_excl ;

    -- : Determiner -> Kind -> Term -> Term ;
    TIncluding the valuation t = -- fixed valuation, including a pre-money or post-money valuation
      let inclAdv : Adv = adv including_Prep (np t) ; -- The adv "including pre-money"
          valuation_incl : Kind = valuation ** {
            cn = ExtAdvCN valuation.cn inclAdv  -- first layer: "valuation including pre-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the.det valuation_incl ;

    AnyOther = any_other_Det ;

  lincat
    ListTerm2Kind = ListCN ;
    ListTerm2Action = {
      vps : ListVPS2 ; -- from ExtendEng
      gers : ListNP
      } ;
  lin
    -- Kind is discontinuous in order to aggregate Term->Kind functions.
    -- Linearization of one Term->Kind is a record like {cn = dissolution ; adv = of the company}
    -- We put the cn parts in a list, and the adv part is only added once.
    -- For linearizing higher-order abstract syntax, see
    -- https://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc122
    BaseTK f g = BaseCN f.cn g.cn ;
    ConsTK f fs = ConsCN f.cn fs ;
    TKOr = composeTK or_Conj ;
    TKAnd = composeTK and_Conj ;

    BaseTA f g = {
      vps = BaseVPS2 f.vps g.vps ;
      gers = mkListNP f.ger g.ger
      } ;
    ConsTA f fs = {
      vps = ConsVPS2 f.vps fs.vps ;
      gers = mkListNP f.ger fs.gers
      } ;
    TAOr = composeTA or_Conj ;
    TAAnd = composeTA and_Conj ;


  oper
    -------------
    -- Lexicon --

  -------------
    any_other_Det : Det = a_Det ** {s = "any other"} ;

    raise_V2 : V2 = mkV2 (mkV "raise") ;
    sell_V2 : V2 = mkV2 (mkV "sell") ;
    issue_V2 : V2 = mkV2 (mkV "issue") ;
    sell_at_V3 : V3 = mkV3 (mkV "sell") at_Prep noPrep ;
    issue_at_V3 : V3 = mkV3 (mkV "issue") at_Prep noPrep ;

    benefit_N2 : N2 = mkN2 (mkN "benefit") ;
    purpose_N2 : N2 = mkN2 (mkN ("purpose"|"principal purpose")) ;
    series_N2 : N2 = mkN2 (mkN "series" "series") ;
    creditor_N : N = mkN "creditor" ;

    whether_or_Conj : Conj = or_Conj ** {s1 = ", whether"} ;

    at_Prep : Prep = mkPrep "at" ;
    excluding_Prep : Prep = mkPrep "excluding" ;
    including_Prep : Prep =  -- endComma: disappears in front of other punctuation
      mkPrep ("including" ++ strOpt ("but not limited to" ++ Prelude.endComma)) ;


    ----------
    -- Misc --
    ----------

    mkVPS2 : VPSlash -> VPS2 = MkVPS2 (mkTemp presentTense simultaneousAnt) positivePol ;

    LinAction : Type = {
      vps : VPS2 ;
      obj : NP ;
      ger : NP
      } ;

    linAction : LinAction -> Str = \l -> (PredVPS emptyNP (vp l)).s ;

    action2 : V2 -> LinTerm -> LinAction = \v2,t -> {
      vps = mkVPS2 (mkVPSlash v2) ;
      obj = np t ;
      ger = GerundNP (mkVP v2 emptyNP) ;
      } ;
    action3 : V3 -> LinTerm -> LinTerm -> LinAction = \v3,indir,dir -> {
      vps = mkVPS2 (Slash2V3 v3 (np indir)) ;
      obj = np dir ;
      ger = GerundNP (mkVP v3 emptyNP (np indir)) ;
      } ;

    cl : LinTerm -> LinAction -> S = \subj,pred -> PredVPS (np subj) (vp pred) ;
    vp : LinAction -> VPS = \pred -> ComplVPS2 pred.vps pred.obj ;
    gerund : LinAction -> NP = \pred -> pred.ger ** {
      s = \\c => pred.ger.s ! c ++ pred.obj.s ! c
      } ;


    LinKind : Type = {cn : CN ; adv : Adv} ;
    LinTerm : Type = {cn : CN ; det : Det} ;

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

    ---------------------------
    -- Function compositions --
    ---------------------------

    -- We kept the CN and Adv parts of Kind separate until now,
    -- but at this point we can put them back together, both into the cn field.
    -- This necessary for adding postmod APs. -- TODO check if still true
    composeTK : Conj -> ListCN -> LinTerm -> LinKind = \co,fs,x ->
      let cns : CN = ConjCN co fs ;
       in linkind (mkCN cns (adv part_Prep (np x))) ;
      --{cn = cns ; adv = adv part_Prep x} ; -- alternative: keep separate

    composeTA : Conj -> {vps : ListVPS2 ; gers : ListNP} -> LinTerm -> LinAction = \co,fs,x -> {
      vps = ConjVPS2 co fs.vps ;
      obj = np x ;
      ger = mkNP co fs.gers ;
      } ;

 }
