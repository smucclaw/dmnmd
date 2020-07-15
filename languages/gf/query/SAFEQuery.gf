abstract SAFEQuery = Query ** {
  {- Later: more fine-grained categories
  cat
    Event ; -}
  flags startcat=Move ;
  fun
    ----------------
    -- Properties --
    ----------------
    Fixed,
    PreMoney,
    PostMoney,
    Voluntary,
    Involuntary : Property ;

    Benefit : Term -> Property ; -- for the benefit of the Company's creditors

  -----------
    -- Kinds --
    -----------
    Event,

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

    KExcluding, -- liquidation of the Company, excluding a Liquidity Event
    KIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Kind -> Term -> Kind ;    -- TODO: is this a good type?

    -----------
    -- Terms --
    -----------
    Company : Term ;
    Creditors : Term -> Term ; -- the Company's creditors

    TAnyOther : Kind -> Term ; -- any other liquidation, dissolution or winding up

    ------------------------
    -- Lists of functions --
    ------------------------

    -- Liquidation, Dissolution and WindingUp are functions. They take arguments.
    -- This is in order to aggregate them: "liquidation and dissolution of the company"
    -- instead of "liquidation of the company, dissolution of the company and …"
  cat
    ListTerm2Kind ;
  fun
    BaseTK : (Term -> Kind) -> (Term -> Kind) -> ListTerm2Kind ;
    ConsTK : (Term -> Kind) -> ListTerm2Kind -> ListTerm2Kind ;
    TKAnd,
    TKOr : ListTerm2Kind -> Term -> Kind ;  -- liquidation, dissolution or winding up of the company
      
}
