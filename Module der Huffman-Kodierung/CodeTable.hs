module CodeTable ( codeTable )
     
where

import HTypes

codeTable :: HTree -> HTable
codeTable ht = convert [] ht
                  where
                  convert :: HCode -> HTree -> HTable
                  convert hc (Leaf c n) = [(c, hc)]
                  convert hc (Node n tl tr) = (convert (hc++[L]) tl) ++ 
                                                          (convert (hc++[R]) tr)
                                                          

