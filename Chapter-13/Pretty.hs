import Prelude hiding ((<>))
import Text.PrettyPrint

data Value = Primitive String | Record [(String,Value)]

format :: Value -> Doc
format (Primitive s) = doubleQuotes (text s)
format (Record l)    = sep [lbrace, (nest 2 (sep [doubleQuotes (text k) <+> colon <+> format v <> comma | (k,v) <- l])), rbrace]

v = Record [("name", Record [("first", Primitive "Tom"), ("last", Primitive "Schrijvers")])
           ,("role", Primitive "author")
           ]
