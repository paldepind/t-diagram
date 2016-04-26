{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Element
       (
         Element(Compiler, Interpreter, (:>))
       , toTree
       , draw
       ) where

import GHC.TypeLits
import Data.Proxy
import GHC.Exts (Constraint)
import Diagrams.Prelude hiding (All, (:>))
import Diagram
import Data.Typeable
import Diagrams.TwoD.Text

-- | A helper for applying constraints to several type variables
type family All c xs :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- | Represents the diagram elments at the type level
data Element = Interpreter Symbol Symbol
             | Compiler Symbol Symbol Symbol
             | Element :> Element

-- data a :> b

-- | Represents a tre of diagram elements at the term level
-- the phantom type represents the type of the root node
data ElementTree = CompilerNode String String String ElementTree
                 | InterpreterNode String String ElementTree
                 | Leaf
                 deriving (Show)

data Root t = Root ElementTree deriving (Show)

-- data InterpreterNode t = InterpreterNode input
-- data CompilerNode t = CompilerNode input

-- | Set the input field of an ElementTree
setInput :: ElementTree -> ElementTree -> ElementTree
setInput (CompilerNode sl tl il _) i = CompilerNode sl tl il i
setInput (InterpreterNode il ml _) i = InterpreterNode il ml i

printComp :: forall a b c. All KnownSymbol '[a, b, c] => Proxy (Interpreter a b) -> Proxy (Interpreter b c) -> String
printComp a b = "Interpreter from " ++ symbolVal (Proxy :: Proxy a) ++ " to " ++ symbolVal (Proxy :: Proxy b)

-- class Stringify (d :: Element) where
--   stringify :: Proxy d -> String

-- instance (All KnownSymbol '[s, i]) => Stringify (Interpreter s i) where
--   stringify _ = "interpreter from " ++ symbolVal (Proxy :: Proxy s) ++ " in " ++ symbolVal (Proxy :: Proxy i)

-- instance (All KnownSymbol '[s, t, i]) => Stringify (Compiler s t i) where
--   stringify _ = "compiler from " ++ source ++ " to " ++ target ++ " implemented in " ++ impl
--     where source = symbolVal (Proxy :: Proxy s)
--           target = symbolVal (Proxy :: Proxy t)
--           impl = symbolVal (Proxy :: Proxy i)

-- instance (All KnownSymbol '[a, b, c]) => Stringify (Interpreter a b :> Interpreter b c) where
--   stringify _ = (stringify (Proxy :: Proxy (Interpreter a b))) ++ " into " ++ (stringify (Proxy :: Proxy (Interpreter b c)))

-- | Class for turning Element types into tree at the term level
class ToTree d where
  type TreeRoot d :: Element
  toTree :: Proxy d -> Root (TreeRoot d)

instance (All KnownSymbol '[sl, tl, il]) => ToTree (Compiler sl tl il) where
  type TreeRoot (Compiler sl tl il) = Compiler sl tl il
  toTree _ = Root (CompilerNode sl tl il Leaf)
    where sl = symbolVal (Proxy :: Proxy sl)
          tl = symbolVal (Proxy :: Proxy tl)
          il = symbolVal (Proxy :: Proxy il)

instance (All KnownSymbol '[sl, ml]) => ToTree (Interpreter sl ml) where
  type TreeRoot (Interpreter sl ml) = Interpreter sl ml
  toTree _ = Root (InterpreterNode sl ml Leaf)
    where sl = symbolVal (Proxy :: Proxy sl)
          ml = symbolVal (Proxy :: Proxy ml)

instance (ToTree a, ToTree b, InputTo (TreeRoot a) (TreeRoot b))
         => ToTree ((a :: Element) :> (b :: Element)) where
  type TreeRoot (a :> b) = NewRoot (TreeRoot a) (TreeRoot b)
  toTree _ = (toTree (Proxy :: Proxy a)) `inputTo` (toTree (Proxy :: Proxy b))

-- instance (ToTree b, InputTo a (TreeRoot b))
--          => ToTree (Root a :> (b :: Element)) where
--   type TreeRoot (a :> b) = NewRoot (TreeRoot a) (TreeRoot b)
--   toTree _ = (toTree (Proxy :: Proxy a)) `inputTo` (toTree (Proxy :: Proxy b))

class InputTo (a :: Element) (b :: Element) where
  type NewRoot a b :: Element
  inputTo :: Root a -> Root b -> Root (NewRoot a b)

instance (All KnownSymbol '[sl, ml, ml']) --, ToTree (Interpreter sl ml), ToTree (Interpreter ml ml'))
         => InputTo (Interpreter sl ml) (Interpreter ml ml') where
  type NewRoot (Interpreter sl ml) (Interpreter ml ml') = Interpreter sl ml'
  inputTo i i' = Root $ InterpreterNode sl ml interpreterNode'
    where (Root (InterpreterNode sl ml _)) = i--toTree (Proxy :: Proxy (Interpreter sl ml))
          (Root interpreterNode') = i'--toTree (Proxy :: Proxy (Interpreter ml ml'))

instance (All KnownSymbol '[sl, ml, tl, il], ToTree (Interpreter sl ml), ToTree (Compiler ml tl il))
         => InputTo (Interpreter sl ml) (Compiler ml tl il) where
  type NewRoot (Interpreter sl ml) (Compiler ml tl il) = Compiler sl tl il
  inputTo i c = Root $ CompilerNode sl tl il interpreterNode
    where (Root interpreterNode) = i--toTree (Proxy :: Proxy (Interpreter sl ml))
          (Root (CompilerNode sl tl il _))    = c--toTree (Proxy :: Proxy (Compiler ml tl il))

instance (All KnownSymbol '[sl, tl, il, ml], ToTree (Compiler sl tl il), ToTree (Interpreter il ml))
         => InputTo (Compiler sl tl il) (Interpreter il ml) where
  type NewRoot (Compiler sl tl il) (Interpreter il ml) = Compiler sl tl ml
  inputTo c i = Root $ InterpreterNode il sl compilerNode
    where (Root compilerNode)    = c--toTree (Proxy :: Proxy (Compiler ml tl il))
          (Root (InterpreterNode il sl _)) = i--toTree (Proxy :: Proxy (Interpreter il ml))

instance (All KnownSymbol '[sl, tl, il, tl', il'], ToTree (Compiler sl tl il), ToTree (Compiler il tl' il'))
         => InputTo (Compiler sl tl il) (Compiler il tl' il') where
  type NewRoot (Compiler sl tl il) (Compiler il tl' il') = Compiler sl tl' il'
  inputTo c c' = Root $ CompilerNode sl tl il compilerNode
    where (Root compilerNode)  = c--toTree (Proxy :: Proxy (Compiler sl tl il))
          (Root (CompilerNode sl tl il _)) = c'-- toTree (Proxy :: Proxy (Compiler il tl' il'))

renderInputTo Leaf parent = parent
renderInputTo (CompilerNode sl tl il _) parent =
  (tDiagram sl tl il) `tIntoT` parent
renderInputTo (InterpreterNode sl ml _) parent =
  (iDiagram sl ml) `iOntoI` parent

drawElementTree (CompilerNode sl tl il inputElement) =
   inputElement `renderInputTo` tDiagram sl tl il
drawElementTree (InterpreterNode sl ml inputElement) =
   inputElement `renderInputTo` iDiagram sl ml

draw :: (RealFloat n, Typeable n,
         Renderable (Diagrams.TwoD.Text.Text n) b,
         Renderable (Path V2 n) b,
         ToTree a)
        => Proxy a -> QDiagram b V2 n Any
draw t = drawElementTree element
  where (Root element) = toTree t
