abstract DMN = {

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

    VS : String -> DMNVal ;
    VN : Float  -> DMNVal ;
    VB : Bool   -> DMNVal ;

    NoComment : Comment ;
    CommentString : String -> Comment ;

    FAnything : FEELexp ;
    FNullary  : DMNVal -> FEELexp ;
    FSection  : FBinOp -> DMNVal -> FEELexp ;
    FInRange  : Float  -> Float  -> FEELexp ;
  -- FFunction : FNumFunction -> FEELexp ;

    -- BaseFEELexp, ConsFEELexp constructed automatically thanks to [FEELexp]{2}
    Disj : [FEELexp] -> FEELexp ; -- Used inside one cell

    -- Expression + header = Cell
    ColHd : String -> FEELexp -> FCell ;

    -- BaseFCell, ConsFCell constructed automatically thanks to [FCell]{2}
    Many : [FCell] -> FCells ;
    Single : FCell -> FCells ;

    -- Construct a row from input and output cells.
    -- Concrete syntaxes may leave out comment, line number or both.
    Row : Int     -> -- row number
          FCells  -> -- inputs
          FCells  -> -- outputs
          Comment -> -- comments
          DTRow ;

    -- Finally, the full table.
    Table : [DTRow] -> DTable ;
}
