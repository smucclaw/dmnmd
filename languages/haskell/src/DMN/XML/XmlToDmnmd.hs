{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module DMN.XML.XmlToDmnmd where

import DMN.XML.ParseDMN as X
import qualified DMN.Types as T
import Data.Maybe (maybeToList)
import qualified Data.List as L
import DMN.DecisionTable (mkFs)

-- $> :t runEx1

-- $> dmnThings <- runEx1

-- $> convertIt =<< dmnThings

convertAll :: [XDMN] -> [T.DecisionTable]
convertAll x = x >>= convertIt

convertIt :: X.XDMN -> [T.DecisionTable]
convertIt d = do
    desicions <- X.defsDescisions d
    convdec desicions

convdec :: X.Decision -> [T.DecisionTable]
convdec dec = do
    let decisionName = dmnnName $ decLabel dec
    ExprDTable tabl <- maybeToList $ X.decDTable dec -- TODO: Partial!
    pure $ convTable decisionName tabl

convTable :: String -> X.DecisionTable -> T.DecisionTable
-- convTable = undefined
convTable name (X.DecisionTable
  { X.dtLabel,
    X.dtHitPolicy ,
    X.dtInput ,
    X.dtOutput ,
    X.dtRules
  }) = T.DTable 
    { T.tableName = name
    , T.hitpolicy = dtHitPolicy
    , T.header    = inputHeaders ++ outputHeaders
    , T.allrows   = zipWith (convRule inputTypes outputTypes) [1..] dtRules
    }
    where
        inputHeaders = map convInputHeader dtInput
        outputHeaders = map convOutputHeader dtOutput
        inputTypes = map T.vartype inputHeaders
        outputTypes = map T.vartype outputHeaders


-- data InOrOut = TIn X.TableInput | TOut X.TableOutput

-- inAndOut :: [X.TableInput] -> [X.TableOutput] -> [InOrOut]
-- inAndOut ins outs = map TIn ins ++ map TOut outs

-- convHeader :: [X.TableInput] -> [X.TableOutput] -> [T.ColHeader]
-- -- convHeader ins outs = error $ "convHeader:\nIns: " ++ show ins ++ "\nouts: " ++ show outs
-- convHeader ins outs = map convHeader' $ inAndOut ins outs

convInputHeader :: TableInput -> T.ColHeader
convInputHeader (tin@TableInput { tinpName , tinpLabel , tinpExpr = InputExpression inpexpr }) 
    -- = error $ show tin
    = T.DTCH 
        { T.label   = T.DTCH_In
        , T.varname = maybe "I don't know?" id . fmap innerText $ tleContent inpexpr -- TODO: I think this is correct, but I'm not sure
        , T.vartype = fmap convertType . exprTypeRef $ tleExpr inpexpr
        , T.enums   = Nothing -- TODO: This is definitely wrong
        }
    -- = error "not implemented"

convOutputHeader :: TableOutput -> T.ColHeader
convOutputHeader ( tout@TableOutput { toutName , toutLabel , toutTypeRef })
    = T.DTCH 
        { T.label   = T.DTCH_Out
        , T.varname = columnLabel $ toutLabel -- TODO: Is this what we want?
        , T.vartype = Just $ convertType $ toutTypeRef
        , T.enums   = Nothing
        }

convertType :: TypeRef -> T.DMNType
convertType (TypeRef "string") = T.DMN_String
convertType (TypeRef "boolean") = T.DMN_Boolean
convertType (TypeRef "integer") = T.DMN_Number
convertType (TypeRef tname) = error $ "Unknown type: " ++ show tname

outThing :: TableOutput
outThing =
  TableOutput
    { toutName = dmnLabeled "OuputClause_99999" "beverages",
      toutLabel = ColumnLabel {columnLabel = "Beverages"},
      toutTypeRef = TypeRef {typeRef = "string"}
    }


{-
thing :: TableInput
thing =
  TableInput
    { tinpName = dmnWithId "InputClause_1acmlkd"
    , tinpLabel = ColumnLabel { columnLabel = "Dish" }
    , tinpExpr = InputExpression
          { inputExprLabel = dmnWithId "LiteralExpression_0bqgrlg"
          , inputExprTypeRef = TypeRef {typeRef = "string"}
          , inputExprText = TextElement {innerText = "desiredDish"}
          }
    }
-}

convRule :: [Maybe T.DMNType] -> [Maybe T.DMNType] -> Int -> Rule -> T.DTrow
convRule intypes outtypes rowNumber ( Rule
    { ruleLabel
    , ruleDescription
    , ruleInputEntry
    , ruleOutputEntry
    })
    = T.DTrow
        { T.row_number   = Just rowNumber
        , T.row_inputs   = zipWith convInputEntry intypes ruleInputEntry
        , T.row_outputs  = zipWith convOutputEntry outtypes ruleOutputEntry
        , T.row_comments = pure $ fmap description ruleDescription -- TODO: This may be bogus
        }

convInputEntry :: Maybe T.DMNType -> InputEntry -> [T.FEELexp]
convInputEntry intype (InputEntry { ieLabel , ieText = TextElement str }) 
    = mkFs intype str

convOutputEntry :: Maybe T.DMNType -> OutputEntry -> [T.FEELexp]
convOutputEntry outtype (OutputEntry { outputEntryLabel , outputEntryText = TextElement str }) 
    = mkFs outtype str

oeExample :: OutputEntry
oeExample =
  OutputEntry
    { outputEntryLabel = dmnWithId "LiteralExpression_1kr45vj"
    , outputEntryText = TextElement {innerText = "\"Aecht Schlenkerla Rauchbier\""}
    }

ieExample :: InputEntry
ieExample =
  InputEntry
    { ieLabel = dmnWithId "UnaryTests_03g3ci0"
    , ieText = TextElement { innerText = "\"Spareribs\"" }
    }


{-

    -- = error "not implemented"

--}