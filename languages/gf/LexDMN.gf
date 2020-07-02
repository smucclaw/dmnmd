interface LexDMN = open DMNParams, Syntax in {
  oper
   anything_NP : NP ;
   upon_Prep : Prep ;
   at_Prep : Prep ;

   flt,
   flte,
   feq,
   fgte,
   fgt : HeaderType => Str ;
}
