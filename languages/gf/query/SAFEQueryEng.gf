concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open
  Prelude,
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  AdjectiveEng,
  ConjunctionEng,
  ExtendEng in {

  lincat
    Action = VP ;
    Determiner = Det ;
  lin

    -------------
    -- Actions --
    -------------
    -- : Term -> Action ; -- raising capital
    Raise t = mkVP raise_V2 t.np ;
    Sell t = mkVP sell_V2 t.np ;
    Issue t = mkVP issue_V2 t.np ;
    SellAt stock valuation = mkVP sell_at_V3 stock.np valuation.np ;
    IssueAt stock valuation = mkVP issue_at_V3 stock.np valuation.np ;

    -- : Term -> Action -> Move ; -- the company raises capital
    MAction term action = mkText (mkUtt (mkCl term.np action)) fullStopPunct ;

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
    ForBenefit term =
      adv2ap (adv for_Prep (mkNP the_Det (mkCN benefit_N2 term.np))) ;

    -- -- : Kind -> Term -> Kind ; -- for the benefit of the Company's creditors
    -- ForBenefit term kind = kind ** {
    --   adv = cc2 kind.adv
    --            (adv for_Prep (mkNP the_Det (mkCN benefit_N2 term.np)))
    --   } ;

    -- : Action -> Property ; -- with the purpose of raising capital
    WithPurpose action =
      adv2ap (adv with_Prep
                  (mkNP the_Det
                        (mkCN purpose_N2 (GerundNP action))
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

    Liquidation t = kind "liquidation" ** {adv = adv part_Prep t.np} ;
    Dissolution t = kind "dissolution" **  {adv = adv part_Prep t.np} ;
    WindingUp t = linkind (mkCN (mkN "winding up" "windings up")) ** {adv = adv part_Prep t.np} ;

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
      np = mkNP theSg_Det (mkN "Company") ;
      det = the_Det
      } ;

    -- : Term -> Term ;
    Creditors company = company ** {         -- the company's creditors
      np = mkNP (mkDet (ExtendEng.GenNP company.np) pluralNum) creditor_N
      } ;

    -- : Kind -> Term ;
    TAnyOther = term any_other_Det ;
    TSeries kind = TPlIndef (kind ** {cn = mkCN series_N2 (mkNP aPl_Det kind.cn)}) ;

    -- : Determiner -> Kind -> Term -> Term ;
    TExcluding the valuation postmoney =
      let exclAdv : Adv = parenss (adv excluding_Prep postmoney.np) ; -- The adv "excluding post-money"
          valuation_excl : Kind = valuation ** {
            cn = AdvCN valuation.cn exclAdv  -- first layer: "valuation excluding post-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the.det valuation_excl ;

    -- : Determiner -> Kind -> Term -> Term ;
    TIncluding the valuation premoney = -- fixed valuation, including a pre-money or post-money valuation
      let inclAdv : Adv = adv including_Prep premoney.np ; -- The adv "including pre-money"
          valuation_incl : Kind = valuation ** {
            cn = ExtAdvCN valuation.cn inclAdv  -- first layer: "valuation including pre-money"
            } ; -- Potential postmodifier is in valuation's adv field
      in term the.det valuation_incl ;

    AnyOther = any_other_Det ;

  lincat
    ListTerm2Kind = ListCN ;
  lin
    -- Kind is discontinuous in order to aggregate Term->Kind functions.
    -- Linearization of one Term->Kind is a record like {cn = dissolution ; adv = of the company}
    -- We put the cn parts in a list, and the adv part is only added once.
    -- For linearizing higher-order abstract syntax, see
    -- https://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc122
    BaseTK f g = BaseCN f.cn g.cn ;
    ConsTK f fs = ConsCN f.cn fs ;
    TKOr = compose or_Conj ;
    TKAnd = compose and_Conj ;

  oper
    -------------
    -- Lexicon --
    -------------
    any_other_Det : Det = a_Det ** {s = "any other"} ;

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

    at_Prep : Prep = mkPrep "at" ;
    excluding_Prep : Prep = mkPrep "excluding" ;
    including_Prep : Prep =  -- endComma: disappears in front of other punctuation
      mkPrep ("including" ++ strOpt ("but not limited to" ++ Prelude.endComma)) ;


    ----------
    -- Misc --
    ----------

    LinKind : Type = {cn : CN ; adv : Adv} ;
    LinProp : Type = {ap : AP ; adv : Adv} ;
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
    -- This necessary for adding postmod APs. (If you add only one Term->Kind, then postmod APs break.)
    -- TODO: tweak the lincat of Property to fix this.
    compose : Conj -> ListCN -> {np : NP} -> LinKind = \co,fs,x ->
      let fsCN : CN = ConjCN co fs ;
       in linkind (mkCN fsCN (adv part_Prep x.np)) ;
      --{cn = cns ; adv = adv part_Prep x} ; -- alternative: keep separate

 }
