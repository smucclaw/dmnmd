abstract DMN =
  WordnetNPs
  --, Numeral
  ** {

  flags startcat = DTable ;

  cat
    -- Categories. Partly correspond to the Haskell types,
    -- but some things need to be handled differently, e.g. column headers.
    DTable ;
    DTRow ;
    [DTRow]{2} ;
    FEELexp ;
    [FEELexp]{2} ;
    FCell ;
    [FCell]{2} ;
    FCells ;
    FBinOp ;
    Comment ;
    DMNVal ;
    Bool ;

  fun

    Flt, Flte, Feq,
    Fgte, Fgt : FBinOp ;

    True, False : Bool ; -- All best languages require you to define your own Bool

    VNOne : DMNVal ; -- Special constructor for 1: to prevent "you have 1 messages"
    VS : String -> DMNVal ;
    VN : Float  -> DMNVal ;
    VB : Bool   -> DMNVal ;

    NoComment : Comment ;
    CommentString : String -> Comment ;

    FAnything : FEELexp ;
    FNullary  : DMNVal -> FEELexp ;
    FSection  : FBinOp -> DMNVal -> FEELexp ;
    FInRange  : Float  -> Float  -> FEELexp ;
    FInRangeInt  : Int  -> Int  -> FEELexp ;
  -- FFunction : FNumFunction -> FEELexp ;

    -- BaseFEELexp, ConsFEELexp constructed automatically thanks to [FEELexp]{2}
    Disj : [FEELexp] -> FEELexp ; -- Used inside one cell

    -- Add header to an FEELexp to build a FCell
    Attribute,  -- Fallback: Header is FEELexp "Dish is Stew"
    Event,      -- {Event,Dissolution} ~ "upon Dissolution"
    Location,   -- {City,Paris} ~ "In Paris", "In any City"
    Duration,   -- {Weeks,[3..5]} ~ "Between 3 and 5 Weeks"
    TimeClock,  -- {Time,14:00} ~ "At 2 PM", "At any Time"
    TimeSeason, -- {Month,May} ~ "In May", "In any Month"
    Weight,     -- {XWeight,3 kg} ~ "X weighs 3 kg"
    Length,     -- {XLength,3 m} ~ "X is 3 m long"
    Height,     -- {XHeight,3 m} ~ "X is 3 m tall"
    Boolean     -- {Liquidation,True} ~ "is liquidation"
     : CN -> FEELexp -> FCell ;

    AmountMass, -- {Cups,X,5} ~ "With 5 Cups of X". NB. the header needs to contain the unit and the material.
    AmountCount -- {Count, X, =<10} ~ "With 10 or fewer Xs"
      : CN -> CN -> FEELexp -> FCell ;

    -- BaseFCell, ConsFCell constructed automatically thanks to [FCell]{2}
    Many : [FCell] -> FCells ;
    Single : FCell -> FCells ;

    -- Booleans in many cells behave like one cell:
    -- "when X, Y and Z" sounds more natural than
    -- "when X, when Y and when Z"
    BoolFCell : [FCell] -> FCell ;

    -- Construct a row from input and output cells.
    -- Concrete syntaxes may leave out comment, line number or both.
    Row : Int     -> -- row number
          FCells  -> -- inputs
          FCells  -> -- outputs
          Comment -> -- comments
          DTRow ;

    -- Finally, the full table.
    Table : [DTRow] -> DTable ;

    Consequence : [DTRow] -> DTable ;

}
