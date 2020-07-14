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
    Voluntary : Property ;

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

    -- We'll switch to a more fine-grained grammar later,
    -- until then these things are also Kinds.
    Liquidation,
    Dissolution,
    WindingUp : Kind ;

    -----------
    -- Terms --
    -----------
    Company : Term ;

    ---------------
    -- Functions --
    ---------------
    Other : Kind -> Kind ;     -- (any) other liquidation, dissolution or winding up

    Including                  -- fixed valuation, including
      : Term -> Term -> Term ; -- a pre-money or post-money valuation

    Benefit : Term -> Property ; -- for the benefit of the Company's creditors

}
