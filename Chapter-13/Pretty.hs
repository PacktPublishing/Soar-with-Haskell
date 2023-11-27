import Prelude hiding ((<>))
import Text.PrettyPrint

data Value = Primitive String | Record [(String,Value)]

card :: Value
card = Record [("name", 
                 Record [("first", Primitive "Tom")
                        ,("last", Primitive "Schrijvers")])
              ,("role", Primitive "author")
              ]

format :: Value -> Doc
format (Primitive s) = doubleQuotes (text s)
format (Record l)    = 
  sep [lbrace, (nest 2 (sep [entry e | e <- l])), rbrace] 
    where
     entry :: (String, Value) -> Doc
     entry (k, v) = 
       doubleQuotes (text k) <+> colon <+> format v <> comma


v = Record [("name", Record [("first", Primitive "Tom"), ("last", Primitive "Schrijvers")])
           ,("role", Primitive "author")
           ]
