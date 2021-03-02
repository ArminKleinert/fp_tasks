module HC_main where

import HTypes ( HTree(Leaf,Node), Bit(L,R), HCode, HTable )
import Coding ( code, decode )
import MakeCode ( codes, codeTable )
import Frequency ( frequency )
import MakeTree( makeTree, toTreeList )

htree = codes "Welche Welche ......"
htable = codeTable htree
  
hc_zip = code htable "Welche Welche ......"
hc_unzip = decode htree hc_zip




