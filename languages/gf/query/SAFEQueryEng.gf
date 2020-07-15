concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open
  Prelude,
  SyntaxEng,
  ParadigmsEng,
  NounEng,
  AdjectiveEng,
  ConjunctionEng,
  ExtendEng in {

  lin
    ----------------
    -- Properties --
    ----------------
    Fixed = prop "fixed" ;
    PreMoney = prop "pre-money" ;
    PostMoney = prop "post-money" ;
    Voluntary = prop "voluntary" ;
    Involuntary = prop "involuntary" ;

    -- : Term -> Property ; -- for the benefit of the Company's creditors
    Benefit np = AdvAP (prop []) -- empty AP, just to attach an Adv to it
                       (adv for_Prep (mkNP the_Det (mkCN benefit_N2 np))) ;

    -----------
    -- Kinds --
    -----------
    Event = kind "event" ;

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

    Liquidation t = kind "liquidation" ** {adv = adv part_Prep t} ;
    Dissolution t = kind "dissolution" **  {adv = adv part_Prep t} ;
    WindingUp t = linkind (mkCN (mkN "winding up" "windings up")) ** {adv = adv part_Prep t} ;

    -- : [Property] -> Kind -> Kind
    KWhetherOr props kind =
      let prop : AP = <mkAP whether_or_Conj props : AP> ** {isPre=False} ;
       in kind ** {cn = mkCN prop kind.cn} ;

    -- : Kind -> Term -> Kind
    KExcluding main excl = main ** {
      cn = AdvCN main.cn (parenss (adv excluding_Prep excl))
      } ;
    KIncluding main incl = main ** {
      cn = ExtAdvCN main.cn (adv including_Prep incl)
      } ;


    -----------
    -- Terms --
    -----------
    Company = mkNP theSg_Det (mkN "Company") ;

    -- : Term -> Term ;
    Creditors company = -- the company's creditors
      mkNP (mkDet (ExtendEng.GenNP company) pluralNum) (mkCN creditor_N) ;

    -- : Kind -> Term ;
    TAnyOther kind = TAny (kind ** {cn = mkCN other_A kind.cn}) ;

  lincat
    ListTerm2Kind = ListCN ;
  lin
    BaseTK f g = BaseCN f.cn g.cn ;
    ConsTK f fs = ConsCN f.cn fs ;
    TKOr = compose or_Conj ;
    TKAnd = compose and_Conj ;

  oper
    -------------
    -- Lexicon --
    -------------

    benefit_N2 : N2 = mkN2 (mkN "benefit") ;
    creditor_N : N = mkN "creditor" ;
    other_A : A = mkA "other" ;
    whether_or_Conj : Conj = or_Conj ** {s1 = ", whether"} ;
    including_Prep : Prep = mkPrep "including" ;
    excluding_Prep : Prep = mkPrep "excluding" ;

    ----------
    -- Misc --
    ----------

    LinKind : Type = {cn : CN ; adv : Adv} ;
    emptyAdv : Adv = mkAdv [] ;
    linkind : CN -> LinKind = \cn -> {cn = cn ; adv = emptyAdv} ;

    adv : Prep -> NP -> Adv = SyntaxEng.mkAdv ; -- shorthand: mkAdv is imported from two modules, so it has to be qualified
    prop : Str -> AP = \a -> mkAP (mkA a) ;
    kind : Str -> LinKind = \n -> linkind (mkCN (mkN n)) ;
    adjkind : Str -> Str -> LinKind = \a,n -> linkind (mkCN (prop a) (mkN n)) ;
    ofkind : Str -> Str -> LinKind = \n,p -> linkind (mkCN (mkN n) (adv part_Prep (mkNP (mkN p)))) ;
    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ; -- hack; RGL has no AP->Adv fun
    ExtAdvCN : CN -> Adv -> CN = \cn,ad -> cn ** { -- RGL fun doesn't put comma
      s = \\n,c => cn.s ! n ! c ++ "," ++ ad.s
      } ;

    ---------------------------
    -- Function compositions --
    ---------------------------

    -- a bit silly, given that the whole split of Kind was to keep the things separate
    -- but postmod APs mess things up, so will make this continuous now
    compose : Conj -> ListCN -> NP -> LinKind = \co,fs,x ->
      let fsCN : CN = ConjCN co fs ;
       in linkind (mkCN fsCN (adv part_Prep x)) ;
      --{cn = cns ; adv = adv part_Prep x} ; -- alternative: keep separate

 }
