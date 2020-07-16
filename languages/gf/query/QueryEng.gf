concrete QueryEng of Query = open
  Prelude,
  AdjectiveEng,
  SyntaxEng,
  ParadigmsEng in {

  lincat
    -- All the categories on the RHS (Utt, QS etc.) come from SyntaxEng
    Move = Text ;
    Query = QS ;
    Kind = {    -- Discontinuous: head and postmodifier.
      cn : CN ; -- Some determiners can come between head and postmod.
      adv : Adv
      } ;
    Term = { -- Determiner included because we want to retain it in special cases
      np : NP ;
      det : Det
      } ;
    [Term] = ListNP ;
    Property = AP ;  -- Simplification: later use https://github.com/GrammaticalFramework/gf-contrib/blob/master/YAQL/YAQLFunctor.gf#L19
    [Property] = ListAP ;

  linref
    -- To make discontinuous categories show properly in the shell
    Kind = \x -> (mkUtt (merge x)).s ;
    Term = \x -> (mkUtt x.np).s ;

  lin
    -- : Query -> Move ;  -- Coercion function: Query to start category Move
    MQuery q = mkText (mkUtt q) questMarkPunct ;

    -- : Kind -> Property -> Query ;    -- which events are dissolution events
    QWhichProp kind prop = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) prop) ;

    -- : Kind -> Term -> Query ;      -- which events are dissolution events
    QWhichTerm kind term = mkQS (mkQCl (mkIP whichPl_IDet (merge kind)) term.np) ;

    -- : Term -> Property -> Query ;    -- is change of control voluntary
    QWhetherProp term prop = mkQS (mkQCl (mkCl term.np prop)) ;

    -- : Term -> Term -> Query ;    -- is change of control a liquidity event
    QWhetherTerm term1 term2 = mkQS (mkQCl (mkCl term1.np term2.np)) ;

    MYes = yes_Utt ;
    MNo = no_Utt ;

    -- : Term -> Property -> Move ;     -- liquidity event is voluntary
    MDefProp term prop = mkText (mkUtt (mkCl term.np prop)) fullStopPunct ;

    -- : Term -> Term -> Move ;         -- liquidity event means A, B or C
    MDefTerm term1 term2 = mkText (mkUtt (mkCl term1.np (mkV2 (mkV "mean")) term2.np)) fullStopPunct ;


    -- Kinds, Terms and Properties
    TSgIndef = term aSg_Det ; -- using our oper 'term', defined at the end of file
    TPlIndef = term aPl_Det ;
    TSgDef = term theSg_Det ;
    TPlDef = term thePl_Det ;
    TAny = term any_Det ;
    TAll = term all_Det ;
    TMass = term emptyDet ;


    -- See the two instances of ListNP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListNP
    BaseTerm t1 t2 = mkListNP t1.np t2.np ; -- : NP -> NP -> ListNP
    ConsTerm t1 ts = mkListNP t1.np ts;     -- : NP -> ListNP -> ListNP
    TOr ts = {   -- Conjunction with or: "change of control or direct listing"
      np = mkNP or_Conj ts ;
      det = emptyDet -- won't make a difference anymore
      } ;
    TAnd ts = {
      np = mkNP and_Conj ts ;
      det = emptyDet -- won't make a difference anymore
      } ;

    -- See the two instances of ListAP https://www.grammaticalframework.org/lib/doc/synopsis/index.html#ListAP
    BaseProperty = mkListAP ; -- : AP -> AP -> ListAP
    ConsProperty = mkListAP ; -- : AP -> ListAP -> ListAP
    POr = mkAP or_Conj ;      -- Conjunction with or: "pre-money or post-money"
    PAnd = mkAP and_Conj ;

    --PNot : Property -> Property ;

    -- : Property -> Kind -> Kind ;    -- voluntary termination
    KProperty prop kind = case prop.isPre of {
      True => kind ** {cn = mkCN prop kind.cn} ;
      False => kind ** {adv = cc2 kind.adv (ap2adv prop) }
      } ;

    -----------------------------------------------------------------

  oper
    -- Resource Grammar Library doesn't have any_Det, so we make it ourselves.
    -- Determiners are supposed to be closed class, so the constructor isn't
    -- exported in the API. (Silly, if you ask me.)
    -- The options are: open a low-level module and use the hidden constructor, or do this hack.
    any_Det : Det = a_Det ** { -- Extend a_Det: keyword ** is record extension
      s = "any"                -- Keep other fields from a_Det, but replace s with the string "any"
      } ;
    all_Det : Det = aPl_Det ** {
      s = "all"
      } ;
    emptyDet : Det = a_Det ** {
      s = []
      } ;

    emptyAdv : Adv = mkAdv [] ;
    emptyAP : AP = <mkAP (mkA []):AP> ** {s = \\_ => []} ;

    ap2adv : AP -> Adv = \ap -> lin Adv (mkUtt ap) ;  -- RGL has no AP->Adv fun
    adv2ap : Adv -> AP = AdjectiveEng.AdvAP emptyAP ; -- RGL has no Adv->AP fun

    -- Wrapper to make NPs out of our record types
    -- Det is separate due to special stuff in SAFEQueryEng.
    term : Det -> {cn : CN ; adv : Adv} -> {np : NP ; det : Det} = \det,kind -> {
      np = mkNP det (merge kind) ;
      det = det
      } ;

    -- Merge the discontinuous Kind into a single CN
    merge : {cn : CN ; adv : Adv} -> CN = \kind -> mkCN kind.cn kind.adv ;

  --   -- Merge the discontinuous Property
  --   comp : {ap : AP ; adv : Adv} -> VP = \prop ->
  --     case prop.propType of {
  --       OnlyAP  => mkVP prop.ap ;
  --       OnlyAdv => mkVP prop.adv ;
  --       Both =>

  --     }

  -- param
  --   PropType = OnlyAP | OnlyAdv | Both ;
}
