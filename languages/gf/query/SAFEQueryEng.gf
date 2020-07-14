concrete SAFEQueryEng of SAFEQuery = QueryEng **
  open SyntaxEng, ParadigmsEng in {

  lin
    ----------------
    -- Properties --
    ----------------
    Fixed = prop "fixed" ;
    PreMoney = prop "pre-money" ;
    PostMoney = prop "post-money" ;
    Voluntary = prop "voluntary" ;

    -----------
    -- Kinds --
  -----------
    Event = kind "event" ;

    Termination = ofkind "termination" "operations" ;
    GeneralAssignment = adjkind "general" "assignment" ;

    LiquidityEvent = adjkind "liquidity" "event" ;
    ChangeOfControl = ofkind "change" "control" ;
    DirectListing = adjkind "direct" "listing" ;
    InitialPublicOffering = mkCN (prop "initial") (adjkind "public" "offering") ;

    EquityFinancing = adjkind "equity" "financing" ;
    Transaction = kind "transaction" ;
    PreferredStock = adjkind "preferred" "stock" ;
    Valuation = kind "valuation" ;

    -----------
    -- Terms --
    -----------
    Company = mkNP theSg_Det (kind "company") ;

    ---------------
    -- Functions --
    ---------------

    -- : Kind -> Kind ;
    Other = mkCN (prop "other") ;

    --Benefit : Term -> Property ; -- for the benefit of the Company's creditors

  oper
    prop : Str -> AP = \a -> mkAP (mkA a) ;
    kind : Str -> CN = \n -> mkCN (mkN n) ;
    adjkind : Str -> Str -> CN = \a,n -> mkCN (prop a) (mkN n) ;
    ofkind : Str -> Str -> CN = \n,p -> mkCN (mkN n) (SyntaxEng.mkAdv part_Prep (mkNP (mkN p))) ;
}
