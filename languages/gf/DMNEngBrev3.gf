concrete DMNEngBrev3 of DMN =
  WordnetNPsEng
  --,NumeralEng
  **
  DMNBase
    with
     (Prelude = Prelude),
     (Syntax = SyntaxEng),
     (Symbolic = SymbolicEng),
     (Grammar = GrammarEng),
     (LexDMN = LexDMNEng),
     (Coordination = Coordination) ** open DMNParams in {

  oper
    brev = B3 ;
    order = IfThen ;
    tablesep = ";" ;
}
