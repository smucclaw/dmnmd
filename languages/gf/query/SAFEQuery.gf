abstract SAFEQuery = Query ** {
  cat
    Action ;
    Determiner ; -- Works together with Including and Excluding
    -- Event ; -- TODO: figure out semantics
  flags startcat=Move ;
  fun

    -------------
    -- Actions --
    -------------
    Raise,                             -- raising capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : Term -> Action ;

    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : Term -> Term -> Action ;

    MAction : Term -> Action -> Move ; -- the company raises capital

    ----------------
    -- Properties --
    ----------------
    Fixed,
    PreMoney,
    PostMoney,
    BonaFide,
    Voluntary,
    Involuntary : Property ;


    ForBenefit   -- general assignment for the benefit of the Company's creditors
      : Term -> Property ;

    WithPurpose   -- transaction with the purpose of raising capital
      : Action -> Property ;
    -----------
    -- Kinds --
    -----------
    Event,
    Capital,

    DissolutionEvent,
    Termination,
    GeneralAssignment,

    LiquidityEvent,
    ChangeOfControl,
    DirectListing,
    InitialPublicOffering,

    EquityFinancing,
    Transaction,
    PreferredStock,
    Valuation : Kind ;

    -- These take a term
    Liquidation,
    Dissolution,
    WindingUp : Term -> Kind ;

    KWhetherOr  -- dissolution event, whether voluntary or involuntary
      : [Property] -> Kind -> Kind ;

    -----------
    -- Terms --
    -----------
    Company : Term ;
    Creditors : Term -> Term ; -- the Company's creditors

    TAnyOther : Kind -> Term ; -- any other liquidation, dissolution or winding up
    TSeries : Kind -> Term ;   -- a series of transactions

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      :-- Determiner ->
        (Kind -> Term) ->
        Kind -> Term ->
        Term ;
    AnyOther : Determiner ;
    ------------------------
    -- Lists of functions --
    ------------------------

    -- Liquidation, Dissolution and WindingUp are functions. They take arguments.
    -- This is in order to aggregate them: "liquidation and dissolution of the company"
    -- instead of "liquidation of the company, dissolution of the company and …"
  cat
    ListTerm2Kind ;
    ListTerm2Action ;
  fun
    BaseTK : (Term -> Kind) -> (Term -> Kind) -> ListTerm2Kind ;
    ConsTK : (Term -> Kind) -> ListTerm2Kind -> ListTerm2Kind ;
    TKAnd,
    TKOr : ListTerm2Kind -> Term -> Kind ;  -- liquidation, dissolution or winding up of the company

    BaseTA : (Term -> Action) -> (Term -> Action) -> ListTerm2Action ;
    ConsTA : (Term -> Action) -> ListTerm2Action -> ListTerm2Action ;
    TAAnd,
    TAOr : ListTerm2Action -> Term -> Action ; -- issues and sells stock at a fixed valuation

}
