concrete DMNEngHornBrev1 of DMN =
  WordnetNPsEng
--  ,NumeralEng
   **
  DMNBase
    with
     (Prelude = Prelude),
     (Coordination = Coordination),
     (Syntax = SyntaxEng),
     (Sentence = SentenceEng),
     (Extend = ExtendEng),
     (Symbolic = SymbolicEng),
     (LexDMN = LexDMNEng)
     ** open DMNParams in {

  oper
    brev = B1 ;
    order = ThenIf ;
    tablesep = "\\" ;
}
