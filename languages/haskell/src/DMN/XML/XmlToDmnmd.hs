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
    tabl <- maybeToList $ X.decDTable dec
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
    , T.header    = convHeader dtInput dtOutput
    , T.allrows   = zipWith convRule [1..] dtRules
    }

data InOrOut = TIn X.TableInput | TOut X.TableOutput

inAndOut :: [X.TableInput] -> [X.TableOutput] -> [InOrOut]
inAndOut ins outs = map TIn ins ++ map TOut outs

convHeader :: [X.TableInput] -> [X.TableOutput] -> [T.ColHeader]
-- convHeader ins outs = error $ "convHeader:\nIns: " ++ show ins ++ "\nouts: " ++ show outs
convHeader ins outs = map convHeader' $ inAndOut ins outs

convHeader' :: InOrOut -> T.ColHeader
convHeader' (TIn (tin@TableInput { tinpName , tinpLabel , tinpExpr })) 
    -- = error $ show tin
    = T.DTCH 
        { T.label   = T.DTCH_In
        , T.varname = innerText $ inputExprText tinpExpr -- TODO: I think this is correct, but I'm not sure
        , T.vartype = Just $ convertType $ inputExprTypeRef tinpExpr
        , T.enums   = Nothing -- TODO: This is definitely wrong
        }
convHeader' (TOut ( tout@TableOutput { toutName , toutLabel , toutTypeRef }))
    -- = error $ show tout
    = T.DTCH 
        { T.label   = T.DTCH_Out
        , T.varname = columnLabel $ toutLabel -- TODO: Is this what we want?
        , T.vartype = Just $ convertType $ toutTypeRef
        , T.enums   = Nothing
        }
    -- = error "not implemented"

convertType :: TypeRef -> T.DMNType
convertType (TypeRef "string") = T.DMN_String
convertType (TypeRef "boolean") = T.DMN_Boolean
convertType (TypeRef "integer") = T.DMN_Number
convertType (TypeRef tname) = error $ "Unknown type: " ++ show tname

outThing :: TableOutput
outThing =
  TableOutput
    { toutName = dmnNamed "OuputClause_99999" "beverages",
      toutLabel = ColumnLabel {columnLabel = "Beverages"},
      toutTypeRef = TypeRef {typeRef = "string"}
    }


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

convRule :: Int -> Rule -> T.DTrow
convRule rowNumber ( Rule
    { ruleLabel
    , ruleDescription
    , ruleInputEntry
    , ruleOutputEntry
    })
    = T.DTrow
        { T.row_number   = Just rowNumber
        , T.row_inputs   = convInputEntry <$> ruleInputEntry
        , T.row_outputs  = fmap convOutputEntry ruleOutputEntry
        , T.row_comments = pure $ fmap description ruleDescription -- TODO: This may be bogus
        }

convInputEntry :: InputEntry -> [T.FEELexp]
convInputEntry ( InputEntry { ieLabel , ieText = TextElement str }) 
    -- = error $ show ie
    = mkFs Nothing str -- TODO: First arg shouldn't be Nothing

convOutputEntry :: OutputEntry -> [T.FEELexp]
convOutputEntry ( OutputEntry { outputEntryLabel , outputEntryText = TextElement str }) 
    = mkFs Nothing str -- TODO: First arg shouldn't be Nothing
-- convOutputEntry = error . show

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