concrete DMNEngBrev3 of DMN = DMNEngBase
  with
   (Prelude = Prelude),
   (Coordination = Coordination) ** open DMNTextPatterns in {

  lin
    -- The only real work is to format a single row and definite table separator.

    -- : (rownum : Int) -> (in,out : FCells) -> Comment -> DTRow ;
    Row = rowIfThen B3 ;

  oper

    tablesep = ";" ;
}
