module DMN.XML.GenerateFromXsd where

import Text.XML.HXT.Core
import Text.XML.HXT.XMLSchema
import Text.XML.HXT.XMLSchema.Loader (loadDefinition)

foo = xmlSchemaOptions 

getSchema :: IO XmlSchema
getSchema = do
    [schema] <- runX $ arr (const "xsd/DMN13.xsd") >>> loadDefinition [ ]
    return schema