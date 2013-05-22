serializeJValue :: JValue -> Doc
serializeJValue (JBool True) = text "true"
serializeJValue (JBool False) = text "false"
serializeJValue JNull = text "null"
serializeJValue (JNull num) = double num
serializeJValue (JString str) = string str


