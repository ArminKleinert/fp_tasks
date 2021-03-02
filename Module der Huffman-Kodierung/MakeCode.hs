module MakeCode ( codes, codeTable )
       
where

import HTypes  ( HTree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )
import Frequency ( frequency )
import MakeTree ( makeTree )
import CodeTable ( codeTable )

codes :: [Char] -> HTree
codes = makeTree . frequency

