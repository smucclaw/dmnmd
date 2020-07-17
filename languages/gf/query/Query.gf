-- Modified from https://github.com/GrammaticalFramework/gf-contrib/tree/master/YAQL/mini
abstract Query = {
  flags startcat = Move ;
  cat
    Move ; -- Common category for queries and answers
    Query ;
    Kind ;
    [Kind]{2} ;
    Property ;
    [Property]{2} ;
    Term ;
    [Term]{2} ;

    Determiner ;
    Conjunction ;

  fun
    MQuery : Query -> Move ;  -- Coercion function: Query to start category Move

    -- Queries
    QWhichProp : Kind -> Property -> Query ;  -- which events are voluntary
    QWhichTerm : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhetherProp : Term -> Property -> Query ;-- is change of control voluntary
    QWhetherTerm : Term -> Term -> Query ;    -- is change of control a liquidity event

    -- Answers to queries
    MYes,MNo : Move ;
    MDefProp : Term -> Property -> Move ;     -- liquidity event is voluntary
    MDefTerm : Term -> Term -> Move ;         -- liquidity event means A, B or C

    -- Determiners
    ASg,                                     -- a post-money valuation
    APl,                                     -- creditors
    TheSg,                                   -- the company
    ThePl,                                   -- the companies
    All,                                     -- all dissolution events
    Any : Determiner ;                       -- any liquidation event

    TDet : Determiner -> Kind -> Term ;

    -- Kinds, Terms and Properties
    PNot : Property -> Property ;             -- not fixed
    KProperty : Property -> Kind -> Kind ;    -- voluntary termination

    -- Conjunctions
    Or,
    And : Conjunction ;

    ConjTerm                                  -- change of control or direct listing
      : Conjunction -> [Term] -> Term ;
    ConjProp                                  -- pre-money or post-money
      : [Property] -> Property ;
    -- ConjKind
    --   : Conjunction -> [Kind] -> Kind ;

    -----------------------------------------------------------------
    -- Later: maybe switch to the full YAQL "Yet Another Query Language"
    -- https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/

}
