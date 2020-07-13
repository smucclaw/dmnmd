interface LexDMN = open DMNParams, Syntax in {
  oper
   anything_NP : NP ;
   emptyNP : NP ;
   upon_Prep : Prep ;
   at_Prep : Prep ;

   np2s : NP -> S ;
   
   flt,
   flte,
   feq,
   fgte,
   fgt : HeaderType => Str ;
}
