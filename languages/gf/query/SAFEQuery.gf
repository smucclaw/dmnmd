abstract SAFEQuery = Query ** {
  flags startcat=Move ;

  cat
    Action ;
    'Action/Dir' ;
    'Action/Indir' ;
    'Action/Dir/Indir' ;
    [Action]{2} ;              -- sells stock to Acme and raises capital
    ['Action/Dir']{2} ;        -- sells today and issues (stock) at fixed valuation
    ['Action/Indir']{2} ;      -- sells widgets and issues stock (at fixed valuation)
    ['Action/Dir/Indir']{2} ;  -- sells and issues (stock) (at fixed valuation)

  fun
    -------------
    -- Actions --
    -------------
    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : 'Action/Dir' ;

    -- Indirect object
    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : 'Action/Dir/Indir' ;

    -- Complements
    AComplDir   : 'Action/Dir' -> Term -> Action ;
    AComplIndir : 'Action/Indir' -> Term -> Action ;
    ASlashDir   : 'Action/Dir/Indir' -> Term -> 'Action/Indir' ; -- sell stock (at fixed valuation)
    ASlashIndir : 'Action/Dir/Indir' -> Term -> 'Action/Dir' ;   -- sell (stock) at fixed valuation

    -- Conjunctions

    ConjAction : Conjunction -> [Action] -> Action ;
    ConjSlashDir : Conjunction -> ['Action/Dir'] -> 'Action/Dir' ;
    ConjSlashIndir : Conjunction -> ['Action/Indir'] -> 'Action/Indir' ;
    ConjSlashDirIndir : Conjunction -> ['Action/Dir/Indir'] -> 'Action/Dir/Indir' ;

  cat
    -- Event ; -- TODO: figure out semantics
    Temporality ;
    Polarity ;

  fun
    TPresent  : Temporality ;
    TPast     : Temporality ;

    PPositive : Polarity ;
    PNegative : Polarity ;

    MAction : Temporality -> Polarity ->
      Term -> Action -> Move ; -- the company raises/raised/doesn't raise capital

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

    KWhetherOr  -- dissolution event, whether voluntary or involuntary
      : [Property] -> Kind -> Kind ;

    --------------------------
    -- Kinds with arguments --
    --------------------------
  cat
    'Kind/Term' ;
    ['Kind/Term']{2} ;

  fun
    Liquidation,
    Dissolution,
    WindingUp
      : 'Kind/Term' ;

    ComplKind : 'Kind/Term' -> Term -> Kind ; -- liquidation of the company

    ConjSlashTerm -- "liquidation and dissolution of the company"
     : Conjunction -> ['Kind/Term'] -> 'Kind/Term' ;

    -----------
    -- Terms --
    -----------
  fun
    Company : Term ;
    Creditors : Term -> Term ; -- the Company's creditors

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Determiner ->
      Kind -> Term ->
      Term ;

    Series,   -- a series of transactions
    AnyOther  -- any other liquidation, dissolution or winding up
      : Determiner ;

}
