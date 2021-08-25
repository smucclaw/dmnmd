{-# LANGUAGE OverloadedStrings #-}
import Text.XML.HXT.XMLSchema.AbstractSyntax
import Data.Map.Strict (fromList)
import Data.String (IsString (fromString))
import Text.XML.HXT.Core (QName, mkName)

instance IsString QName where
  fromString = mkName

schm = XmlSchema
    { sTargetNS = Just "https://www.omg.org/spec/DMN/20191111/MODEL/"
    , sIncludes = []
    , sSimpleTypes = fromList
        [
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}AlignmentKind"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "start" }
                        , Enumeration { unEnumeration = "end" }
                        , Enumeration { unEnumeration = "center" }
                        ]
                    )
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}KnownColor"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "maroon" }
                        , Enumeration { unEnumeration = "red" }
                        , Enumeration { unEnumeration = "orange" }
                        , Enumeration { unEnumeration = "yellow" }
                        , Enumeration { unEnumeration = "olive" }
                        , Enumeration { unEnumeration = "purple" }
                        , Enumeration { unEnumeration = "fuchsia" }
                        , Enumeration { unEnumeration = "white" }
                        , Enumeration { unEnumeration = "lime" }
                        , Enumeration { unEnumeration = "green" }
                        , Enumeration { unEnumeration = "navy" }
                        , Enumeration { unEnumeration = "blue" }
                        , Enumeration { unEnumeration = "aqua" }
                        , Enumeration { unEnumeration = "teal" }
                        , Enumeration { unEnumeration = "black" }
                        , Enumeration { unEnumeration = "silver" }
                        , Enumeration { unEnumeration = "gray" }
                        ]
                    )
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}rgb"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:int" }
                    ,
                        [ MinIncl { unMinIncl = "0" }
                        , MaxIncl { unMaxIncl = "255" }
                        ]
                    )
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAssociationDirection"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "None" }
                        , Enumeration { unEnumeration = "One" }
                        , Enumeration { unEnumeration = "Both" }
                        ]
                    )
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBuiltinAggregator"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "SUM" }
                        , Enumeration { unEnumeration = "COUNT" }
                        , Enumeration { unEnumeration = "MIN" }
                        , Enumeration { unEnumeration = "MAX" }
                        ]
                    )
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionTableOrientation"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "Rule-as-Row" }
                        , Enumeration { unEnumeration = "Rule-as-Column" }
                        , Enumeration { unEnumeration = "CrossTable" }
                        ]
                    )
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionKind"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "FEEL" }
                        , Enumeration { unEnumeration = "Java" }
                        , Enumeration { unEnumeration = "PMML" }
                        ]
                    )
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tHitPolicy"
            , Restr
                { unRestr =
                    ( BaseAttr { unBaseAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                    ,
                        [ Enumeration { unEnumeration = "UNIQUE" }
                        , Enumeration { unEnumeration = "FIRST" }
                        , Enumeration { unEnumeration = "PRIORITY" }
                        , Enumeration { unEnumeration = "ANY" }
                        , Enumeration { unEnumeration = "COLLECT" }
                        , Enumeration { unEnumeration = "RULE ORDER" }
                        , Enumeration { unEnumeration = "OUTPUT ORDER" }
                        ]
                    )
                }
            )
        ]
    , sComplexTypes = fromList
        [
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Bounds"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}x"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}y"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}width"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}height"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Color"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}red"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:rgb" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}green"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:rgb" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}blue"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:rgb" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDI"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "unbounded"
                                                    }
                                                , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDiagram" }
                                                )
                                            }
                                        , ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "unbounded"
                                                    }
                                                , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNStyle" }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        , []
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDecisionServiceDividerLine"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Edge"
                                ,
                                    ( Nothing
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDiagram"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Diagram"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}Size"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Dimension" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDiagramElement" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNEdge"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Edge"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNLabel" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmnElementRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:QName" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}sourceElement"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:QName" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}targetElement"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:QName" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNLabel"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Shape"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}Text"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNShape"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Shape"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNLabel" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDecisionServiceDividerLine" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmnElementRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:QName" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}isListedInputData"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}isCollapsed"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNStyle"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:Style"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}FillColor"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Color" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}StrokeColor"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Color" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}FontColor"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Color" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontFamily"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontSize"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontItalic"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontBold"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontUnderline"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}fontStrikeThrough"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}labelHorizontalAlignement"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:AlignmentKind" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}labelVerticalAlignment"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:AlignmentKind" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}Diagram"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:DiagramElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}name"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}documentation"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}resolution"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}DiagramElement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Nothing
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{http://www.omg.org/spec/DMN/20180521/DI/}extension"
                                                        , elemTypeDef = ETDAnonymCtDecl
                                                            { unETDAnonymCtDecl = ComplexType
                                                                { ctMixed = Nothing
                                                                , ctDef = NewCT
                                                                    { unNewCT =
                                                                        ( Just
                                                                            ( CompSq
                                                                                { unCompSq =
                                                                                    ( MinMaxOcc
                                                                                        { minOcc = Nothing
                                                                                        , maxOcc = Nothing
                                                                                        }
                                                                                    ,
                                                                                        [ ChSeqAn
                                                                                            { unChSeqAn =
                                                                                                ( MinMaxOcc
                                                                                                    { minOcc = Just "0"
                                                                                                    , maxOcc = Just "unbounded"
                                                                                                    }
                                                                                                , Any
                                                                                                    { namespace = Just "##other"
                                                                                                    , processContents = Just "lax"
                                                                                                    }
                                                                                                )
                                                                                            }
                                                                                        ]
                                                                                    )
                                                                                }
                                                                            )
                                                                        , []
                                                                        )
                                                                    }
                                                                }
                                                            }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        , ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "1"
                                                    }
                                                , ElRef { unElRef = "{http://www.omg.org/spec/DMN/20180521/DI/}di:Style" }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}sharedStyle"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:IDREF" }
                                        , attrUse = Nothing
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}id"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:ID" }
                                        , attrUse = Nothing
                                        }
                                    }
                                }
                            , AnyAttr
                                { unAnyAttr = Any
                                    { namespace = Just "##other"
                                    , processContents = Just "lax"
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Dimension"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}width"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}height"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}Edge"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:DiagramElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DI/}waypoint"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Point" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Point"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}x"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DC/}y"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:double" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}Shape"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{http://www.omg.org/spec/DMN/20180521/DI/}di:DiagramElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Bounds" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}Style"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Nothing
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{http://www.omg.org/spec/DMN/20180521/DI/}extension"
                                                        , elemTypeDef = ETDAnonymCtDecl
                                                            { unETDAnonymCtDecl = ComplexType
                                                                { ctMixed = Nothing
                                                                , ctDef = NewCT
                                                                    { unNewCT =
                                                                        ( Just
                                                                            ( CompSq
                                                                                { unCompSq =
                                                                                    ( MinMaxOcc
                                                                                        { minOcc = Nothing
                                                                                        , maxOcc = Nothing
                                                                                        }
                                                                                    ,
                                                                                        [ ChSeqAn
                                                                                            { unChSeqAn =
                                                                                                ( MinMaxOcc
                                                                                                    { minOcc = Just "0"
                                                                                                    , maxOcc = Just "unbounded"
                                                                                                    }
                                                                                                , Any
                                                                                                    { namespace = Just "##other"
                                                                                                    , processContents = Just "lax"
                                                                                                    }
                                                                                                )
                                                                                            }
                                                                                        ]
                                                                                    )
                                                                                }
                                                                            )
                                                                        , []
                                                                        )
                                                                    }
                                                                }
                                                            }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{http://www.omg.org/spec/DMN/20180521/DI/}id"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:ID" }
                                        , attrUse = Nothing
                                        }
                                    }
                                }
                            , AnyAttr
                                { unAnyAttr = Any
                                    { namespace = Just "##other"
                                    , processContents = Just "lax"
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tArtifact"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Nothing
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAssociation"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tArtifact"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}sourceRef"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}targetRef"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}associationDirection"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAssociationDirection" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAuthorityRequirement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompCh
                                            { unCompCh =
                                                ( MinMaxOcc
                                                    { minOcc = Just "1"
                                                    , maxOcc = Just "1"
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredDecision"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredInput"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredAuthority"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBinding"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "1"
                                                    , maxOcc = Just "1"
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}parameter"
                                                        , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        , ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "1"
                                                    }
                                                , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        , []
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessContextElement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}URI"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessKnowledgeModel"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocable"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}encapsulatedLogic"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionDefinition" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}authorityRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAuthorityRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tContext"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}contextEntry" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tContextEntry"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}variable"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "1"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "1"
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}description"
                                                        , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        , ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "1"
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}extensionElements"
                                                        , elemTypeDef = ETDAnonymCtDecl
                                                            { unETDAnonymCtDecl = ComplexType
                                                                { ctMixed = Nothing
                                                                , ctDef = NewCT
                                                                    { unNewCT =
                                                                        ( Just
                                                                            ( CompSq
                                                                                { unCompSq =
                                                                                    ( MinMaxOcc
                                                                                        { minOcc = Nothing
                                                                                        , maxOcc = Nothing
                                                                                        }
                                                                                    ,
                                                                                        [ ChSeqAn
                                                                                            { unChSeqAn =
                                                                                                ( MinMaxOcc
                                                                                                    { minOcc = Just "0"
                                                                                                    , maxOcc = Just "unbounded"
                                                                                                    }
                                                                                                , Any
                                                                                                    { namespace = Just "##other"
                                                                                                    , processContents = Just "lax"
                                                                                                    }
                                                                                                )
                                                                                            }
                                                                                        ]
                                                                                    )
                                                                                }
                                                                            )
                                                                        , []
                                                                        )
                                                                    }
                                                                }
                                                            }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}id"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:ID" }
                                        , attrUse = Just "optional"
                                        }
                                    }
                                }
                            , Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}label"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                        , attrUse = Just "optional"
                                        }
                                    }
                                }
                            , AnyAttr
                                { unAnyAttr = Any
                                    { namespace = Just "##other"
                                    , processContents = Just "lax"
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}href"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                        , attrUse = Just "required"
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Nothing
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecision"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}question"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}allowedAnswers"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}variable"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}informationRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}authorityRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAuthorityRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}supportedObjective"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}impactedPerformanceIndicator"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionMaker"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionOwner"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}usingProcess"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}usingTask"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionRule"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputEntry"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tUnaryTests" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}outputEntry"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tLiteralExpression" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}annotationEntry"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRuleAnnotation" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionService"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocable"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}outputDecision"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}encapsulatedDecision"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputDecision"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputData"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionTable"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}input"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInputClause" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}output"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tOutputClause" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}annotation"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRuleAnnotationClause" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}rule"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionRule" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}hitPolicy"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tHitPolicy" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}aggregation"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBuiltinAggregator" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}preferredOrientation"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionTableOrientation" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}outputLabel"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDefinitions"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}import" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}itemDefinition"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tItemDefinition" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}drgElement" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}artifact" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}elementCollection"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tElementCollection" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}businessContextElement" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDI" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expressionLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}namespace"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}exporter"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}exporterVersion"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tElementCollection"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}drgElement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionDefinition"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}formalParameter"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}kind"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionKind" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionItem"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}parameters"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}outputTypeRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tGroup"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tArtifact"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}name"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tImport"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}namespace"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}locationURI"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}importType"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tImportedValues"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tImport"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}importedElement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expressionLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationRequirement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqCh
                                                        { unChSeqCh =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "1"
                                                                , maxOcc = Just "1"
                                                                }
                                                            ,
                                                                [ ChSeqEl
                                                                    { unChSeqEl =
                                                                        ( MinMaxOcc
                                                                            { minOcc = Nothing
                                                                            , maxOcc = Nothing
                                                                            }
                                                                        , ElDef
                                                                            { unElDef = ElementDef
                                                                                { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredDecision"
                                                                                , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                                , elemSubstGroup = Nothing
                                                                                }
                                                                            }
                                                                        )
                                                                    }
                                                                , ChSeqEl
                                                                    { unChSeqEl =
                                                                        ( MinMaxOcc
                                                                            { minOcc = Nothing
                                                                            , maxOcc = Nothing
                                                                            }
                                                                        , ElDef
                                                                            { unElDef = ElementDef
                                                                                { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredInput"
                                                                                , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                                , elemSubstGroup = Nothing
                                                                                }
                                                                            }
                                                                        )
                                                                    }
                                                                ]
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInputClause"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputExpression"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tLiteralExpression" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputValues"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tUnaryTests" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInputData"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}variable"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocable"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}variable"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocation"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}binding"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBinding" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tItemDefinition"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
                                ,
                                    ( Just
                                        ( CompCh
                                            { unCompCh =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqSq
                                                        { unChSeqSq =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            ,
                                                                [ ChSeqEl
                                                                    { unChSeqEl =
                                                                        ( MinMaxOcc
                                                                            { minOcc = Nothing
                                                                            , maxOcc = Nothing
                                                                            }
                                                                        , ElDef
                                                                            { unElDef = ElementDef
                                                                                { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeRef"
                                                                                , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                                , elemSubstGroup = Nothing
                                                                                }
                                                                            }
                                                                        )
                                                                    }
                                                                , ChSeqEl
                                                                    { unChSeqEl =
                                                                        ( MinMaxOcc
                                                                            { minOcc = Just "0"
                                                                            , maxOcc = Nothing
                                                                            }
                                                                        , ElDef
                                                                            { unElDef = ElementDef
                                                                                { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}allowedValues"
                                                                                , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tUnaryTests" }
                                                                                , elemSubstGroup = Nothing
                                                                                }
                                                                            }
                                                                        )
                                                                    }
                                                                ]
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}itemComponent"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tItemDefinition" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}functionItem"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}isCollection"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:boolean" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeRequirement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "1"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}requiredKnowledge"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeSource"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}authorityRequirement"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAuthorityRequirement" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}type"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}owner"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}locationURI"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tList"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElRef { unElRef = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression" }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tLiteralExpression"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompCh
                                            { unCompCh =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Just "1"
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}text"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}importedValues"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tImportedValues" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expressionLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Nothing
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}name"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "required"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tOrganizationUnit"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessContextElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionMade"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionOwned"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tOutputClause"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}outputValues"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tUnaryTests" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}defaultOutputEntry"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tLiteralExpression" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}name"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        , Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}typeRef"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tPerformanceIndicator"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessContextElement"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}impactingDecision"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElementReference" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRelation"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}column"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    , ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "unbounded"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}row"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tList" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    , []
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRuleAnnotation"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Just
                            ( CompSq
                                { unCompSq =
                                    ( MinMaxOcc
                                        { minOcc = Nothing
                                        , maxOcc = Nothing
                                        }
                                    ,
                                        [ ChSeqEl
                                            { unChSeqEl =
                                                ( MinMaxOcc
                                                    { minOcc = Just "0"
                                                    , maxOcc = Nothing
                                                    }
                                                , ElDef
                                                    { unElDef = ElementDef
                                                        { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}text"
                                                        , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                        , elemSubstGroup = Nothing
                                                        }
                                                    }
                                                )
                                            }
                                        ]
                                    )
                                }
                            )
                        , []
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRuleAnnotationClause"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = NewCT
                    { unNewCT =
                        ( Nothing
                        ,
                            [ Attr
                                { unAttr = AttrDef
                                    { unAttrDef = AttributeDef
                                        { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}name"
                                        , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                        , attrUse = Nothing
                                        }
                                    }
                                }
                            ]
                        )
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tTextAnnotation"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tArtifact"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Just "0"
                                                                , maxOcc = Just "1"
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}text"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}textFormat"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                    , attrUse = Nothing
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tUnaryTests"
            , ComplexType
                { ctMixed = Nothing
                , ctDef = CCont
                    { unCCont = ComplexContent
                        { ccMixed = Nothing
                        , ccDef = CCExt
                            { unCCExt =
                                ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression"
                                ,
                                    ( Just
                                        ( CompSq
                                            { unCompSq =
                                                ( MinMaxOcc
                                                    { minOcc = Nothing
                                                    , maxOcc = Nothing
                                                    }
                                                ,
                                                    [ ChSeqEl
                                                        { unChSeqEl =
                                                            ( MinMaxOcc
                                                                { minOcc = Nothing
                                                                , maxOcc = Nothing
                                                                }
                                                            , ElDef
                                                                { unElDef = ElementDef
                                                                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}text"
                                                                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:string" }
                                                                    , elemSubstGroup = Nothing
                                                                    }
                                                                }
                                                            )
                                                        }
                                                    ]
                                                )
                                            }
                                        )
                                    ,
                                        [ Attr
                                            { unAttr = AttrDef
                                                { unAttrDef = AttributeDef
                                                    { attrName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expressionLanguage"
                                                    , attrTypeDef = ATDTypeAttr { unATDTypeAttr = "{http://www.w3.org/2001/XMLSchema}xsd:anyURI" }
                                                    , attrUse = Just "optional"
                                                    }
                                                }
                                            }
                                        ]
                                    )
                                )
                            }
                        }
                    }
                }
            )
        ]
    , sElements = fromList
        [
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Bounds"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DC/}Bounds"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Bounds" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Color"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DC/}Color"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Color" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDI"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDI"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDI" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDecisionServiceDividerLine"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDecisionServiceDividerLine"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDecisionServiceDividerLine" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDiagram"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDiagram"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNDiagram" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDiagramElement"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNDiagramElement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DI/}di:DiagramElement" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNEdge"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNEdge"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNEdge" }
                    , elemSubstGroup = Just "dmndi:DMNDiagramElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}DMNElement"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}DMNElement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDMNElement" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNLabel"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNLabel"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNLabel" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNShape"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNShape"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNShape" }
                    , elemSubstGroup = Just "dmndi:DMNDiagramElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNStyle"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}DMNStyle"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/DMNDI/}dmndi:DMNStyle" }
                    , elemSubstGroup = Just "di:Style"
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Dimension"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DC/}Dimension"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Dimension" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DC/}Point"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DC/}Point"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DC/}dc:Point" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{http://www.omg.org/spec/DMN/20180521/DI/}Style"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{http://www.omg.org/spec/DMN/20180521/DI/}Style"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{http://www.omg.org/spec/DMN/20180521/DI/}di:Style" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}artifact"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}artifact"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tArtifact" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}association"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}association"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAssociation" }
                    , elemSubstGroup = Just "artifact"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}authorityRequirement"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}authorityRequirement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tAuthorityRequirement" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}businessContextElement"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}businessContextElement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessContextElement" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}businessKnowledgeModel"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}businessKnowledgeModel"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tBusinessKnowledgeModel" }
                    , elemSubstGroup = Just "invocable"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}context"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}context"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tContext" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}contextEntry"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}contextEntry"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tContextEntry" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}decision"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decision"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecision" }
                    , elemSubstGroup = Just "drgElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionService"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionService"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionService" }
                    , elemSubstGroup = Just "invocable"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionTable"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}decisionTable"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDecisionTable" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}definitions"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}definitions"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDefinitions" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}drgElement"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}drgElement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tDRGElement" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}elementCollection"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}elementCollection"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tElementCollection" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}expression"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tExpression" }
                    , elemSubstGroup = Nothing
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}functionDefinition"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}functionDefinition"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionDefinition" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}functionItem"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}functionItem"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tFunctionItem" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}group"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}group"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tGroup" }
                    , elemSubstGroup = Just "artifact"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}import"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}import"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tImport" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}informationItem"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}informationItem"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationItem" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}informationRequirement"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}informationRequirement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInformationRequirement" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputData"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}inputData"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInputData" }
                    , elemSubstGroup = Just "drgElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}invocable"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}invocable"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocable" }
                    , elemSubstGroup = Just "drgElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}invocation"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}invocation"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tInvocation" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}itemDefinition"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}itemDefinition"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tItemDefinition" }
                    , elemSubstGroup = Just "namedElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeRequirement"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeRequirement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeRequirement" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeSource"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}knowledgeSource"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tKnowledgeSource" }
                    , elemSubstGroup = Just "drgElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}list"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}list"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tList" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}literalExpression"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}literalExpression"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tLiteralExpression" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}namedElement"
            , ElAbs
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}namedElement"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tNamedElement" }
                    , elemSubstGroup = Just "DMNElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}organizationUnit"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}organizationUnit"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tOrganizationUnit" }
                    , elemSubstGroup = Just "businessContextElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}performanceIndicator"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}performanceIndicator"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tPerformanceIndicator" }
                    , elemSubstGroup = Just "businessContextElement"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}relation"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}relation"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tRelation" }
                    , elemSubstGroup = Just "expression"
                    }
                }
            )
        ,
            ( "{https://www.omg.org/spec/DMN/20191111/MODEL/}textAnnotation"
            , ElDef
                { unElDef = ElementDef
                    { elemName = "{https://www.omg.org/spec/DMN/20191111/MODEL/}textAnnotation"
                    , elemTypeDef = ETDTypeAttr { unETDTypeAttr = "{https://www.omg.org/spec/DMN/20191111/MODEL/}tTextAnnotation" }
                    , elemSubstGroup = Just "artifact"
                    }
                }
            )
        ]
    , sGroups = fromList []
    , sAttributes = fromList []
    , sAttributeGroups = fromList []
    }
