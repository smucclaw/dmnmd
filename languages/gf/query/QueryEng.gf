concrete QueryEng of Query = open SyntaxEng, ParadigmsEng in {

  lincat
    -- All the categories on the RHS (Utt, QS etc.) come from SyntaxEng
    Move = Utt ;
    Query = QS ;
    Kind = {cn : CN ; adv : Adv} ; -- To aggregate "liquidation, dissolution or winding up of the company"
    --    [Kind] = ListCN ;

    Term = NP ;
    [Term] = ListNP ;
    Property = AP ; -- Simplification: later use https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/YAQLFunctor.gf#L19
    [Property] = ListAP ;
  linref
    Kind = \x -> (mkUtt (merge x)).s ;
  lin
    -- : Query -> Move ;  -- Coercion function: Query to start category Move
    MQuery = mkUtt ;

    -- : Kind -> Property -> Query ;    -- which events are dissolution events
    QWhichProp kind prop = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) prop) ;

    -- : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhichTerm kind term = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) term) ;

    -- : Term -> Property -> Query ;    -- is change of control voluntary
    QWhetherProp term prop = mkQS (mkQCl (mkCl term prop)) ;

    -- : Term -> Term -> Query ;    -- is change of control a liquidity event
    QWhetherTerm term1 term2 = mkQS (mkQCl (mkCl term1 term2)) ;

    MYes = yes_Utt ;
    MNo = no_Utt ;

    -- : Term -> Property -> Move ;     -- liquidity event is voluntary
    MDefProp term prop = mkUtt (mkCl term prop) ;

    -- : Term -> Term -> Move ;         -- liquidity event means A, B or C
    MDefTerm term1 term2 = mkUtt (mkCl term1 (mkV2 (mkV "mean")) term2) ;


    -- Kinds, Terms and Properties
    TSgIndef = np aSg_Det ; -- using our oper 'np', defined at the end of file
    TPlIndef = np aPl_Det ;
    TSgDef = np theSg_Det ;
    TPlDef = np thePl_Det ;
    TAny = np any_Det ;

    TMass kind = mkNP (mkCN kind.cn kind.adv) ; -- using opers from RGL API
    TAll kind = mkNP all_Predet (np aPl_Det kind) ;


    -- See the two instances of ListNP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListNP
    BaseTerm = mkListNP ; -- : NP -> NP -> ListNP
    ConsTerm = mkListNP ; -- : NP -> ListNP -> ListNP
    TOr = mkNP or_Conj ;  -- Conjunction with or: "change of control or direct listing"
    TAnd = mkNP and_Conj ;

    -- See the two instances of ListAP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListAP
    BaseProperty = mkListAP ; -- : AP -> AP -> ListAP
    ConsProperty = mkListAP ; -- : AP -> ListAP -> ListAP
    POr = mkAP or_Conj ;      -- Conjunction with or: "pre-money or post-money"
    PAnd = mkAP and_Conj ;

    --PNot : Property -> Property ;

    -- : Property -> Kind -> Kind ;    -- voluntary termination
    KProperty prop kind = kind ** {cn = mkCN prop kind.cn} ;

    -----------------------------------------------------------------

  oper
    -- Resource Grammar Library doesn't have any_Det, so we make it ourselves.
    -- Determiners are supposed to be closed class, so the constructor isn't
    -- exported in the API. (Silly, if you ask me.)
    -- The options are: open a low-level module and use the hidden constructor, or do this hack.
    any_Det : Det = a_Det ** { -- Extend a_Det: keyword ** is record extension
      s = "any"                -- Keep other fields from a_Det, but replace s with the string "any"
      } ;


    -- Wrapper to make NPs out of our record types
    np : Det -> {cn : CN ; adv : Adv} -> NP = \det,kind ->
      mkNP det (merge kind) ;

    -- Merge the discontinuous CN
    merge : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;


}
