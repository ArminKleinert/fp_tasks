module Coding ( code, decode )
      
where
  
import HTypes ( HTree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )

code :: HTable -> [Char] -> HCode
code htable = concat . map (Coding.lookup htable)

lookup :: HTable -> Char -> HCode
lookup []  c   =  error "lookup..."
lookup ((char, hc):rtable) c | char == c   =  hc
                             | otherwise    = Coding.lookup rtable c
                                        
decode :: HTree -> HCode -> [Char]
decode htree = decodeByte htree
               where
                 decodeByte (Node n t1 t2) (L:rest) = decodeByte t1 rest
                 decodeByte (Node n t1 t2) (R:rest) = decodeByte t2 rest
                 decodeByte (Leaf c n) rest = c:decodeByte htree rest
                 decodeByte t [] = []
