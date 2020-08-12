concrete DMNEngBrev3 of DMN =
  WordnetNPsEng
  --,NumeralEng
  **
  DMNBase
    with
     (Prelude = Prelude),
     (Syntax = SyntaxEng),
     (Symbolic = SymbolicEng),
     (Sentence = SentenceEng),
     (Extend = ExtendEng),
     (LexDMN = LexDMNEng),
     (Coordination = Coordination) ** open DMNParams in {

  oper
    brev = B3 ;
    order = IfThen ;
    tablesep = ";" ;
}
