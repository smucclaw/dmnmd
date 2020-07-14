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
  fun
    MQuery   : Query -> Move ;  -- Coercion function: Query to start category Move

    -- Queries
    QWhichProp : Kind -> Property -> Query ;  -- which events are voluntary
    QWhichTerm : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhetherProp : Term -> Property -> Query ;-- is change of control voluntary
    QWhetherTerm : Term -> Term -> Query ;    -- is change of control a liquidity event

    -- Answers to queries
    MYes,MNo : Move ;
    MDefProp : Term -> Property -> Move ;     -- liquidity event is voluntary
    MDefTerm : Term -> Term -> Move ;         -- liquidity event means A, B or C

    -- Kinds, Terms and Properties
    TSgIndef,                                 -- post-money valuation
    TPlIndef,                                 -- creditors
    TSgDef,
    TPlDef,
    TMass : Kind -> Term ;
    TAll,                                     -- all dissolution events
    TAny : Kind -> Term ;                     -- any liquidation event

    TOr,                                      -- change of control or direct listing
    TAnd : [Term] -> Term ;
    POr,                                      -- pre-money or post-money
    PAnd : [Property] -> Property ;
    KOr,
    KAnd : [Kind] -> Kind ;
    PNot : Property -> Property ;             -- excluding liquidity
    KProperty : Property -> Kind -> Kind ;    -- voluntary termination
    KTerm : Term -> Kind -> Kind ; -- the Company's creditors

    -----------------------------------------------------------------
    -- Later: switch to the full YAQL "Yet Another Query Language"
    -- https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/

}
