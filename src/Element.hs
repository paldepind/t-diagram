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

-- | Represents a tre of diagram elements at the term level
-- the phantom type represents the type of the root node
data ElementTree = CompilerNode String String String ElementTree
                 | InterpreterNode String String ElementTree
                 | Leaf
                 deriving (Show)

data Root t = Root ElementTree deriving (Show)

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

class InputTo (a :: Element) (b :: Element) where
  type NewRoot a b :: Element
  inputTo :: Root a -> Root b -> Root (NewRoot a b)

instance InputTo (Interpreter sl ml) (Interpreter ml ml') where
  type NewRoot (Interpreter sl ml) (Interpreter ml ml') = Interpreter sl ml'
  inputTo i i' = Root $ InterpreterNode sl ml interpreterNode'
    where (Root (InterpreterNode sl ml _)) = i
          (Root interpreterNode') = i'

instance InputTo (Interpreter sl ml) (Compiler ml tl il) where
  type NewRoot (Interpreter sl ml) (Compiler ml tl il) = Compiler sl tl il
  inputTo i c = Root $ CompilerNode sl tl il interpreterNode
    where (Root interpreterNode) = i
          (Root (CompilerNode sl tl il _))    = c

instance InputTo (Compiler sl tl il) (Interpreter il ml) where
  type NewRoot (Compiler sl tl il) (Interpreter il ml) = Compiler sl tl ml
  inputTo c i = Root $ InterpreterNode il sl compilerNode
    where (Root compilerNode)    = c
          (Root (InterpreterNode il sl _)) = i

instance InputTo (Compiler sl tl il) (Compiler il tl' il') where
  type NewRoot (Compiler sl tl il) (Compiler il tl' il') = Compiler sl tl' il'
  inputTo c c' = Root $ CompilerNode sl tl il compilerNode
    where (Root compilerNode)  = c
          (Root (CompilerNode sl tl il _)) = c'

renderInputTo (CompilerNode _ _ _ Leaf) = \a b -> b
renderInputTo (InterpreterNode _ _ (Leaf)) = iIntoT
renderInputTo (CompilerNode _ _ _ (CompilerNode _ _ _ _)) = tIntoT
renderInputTo (CompilerNode _ _ _ (InterpreterNode _ _ _)) = iIntoT
renderInputTo (InterpreterNode _ _ (CompilerNode _ _ _ _)) = tOntoI
renderInputTo (InterpreterNode _ _ (InterpreterNode _ _ _)) = iOntoI

drawElementTree :: (RealFloat n1, Typeable n1,
                    Renderable (Text n1) b1, Renderable (Path V2 n1) b1) =>
                   ElementTree -> QDiagram b1 V2 n1 Any
drawElementTree c@(CompilerNode sl tl il inputNode) =
  renderInputTo c input parent
  where parent = tDiagram sl tl il
        input  = drawElementTree inputNode
drawElementTree i@(InterpreterNode sl ml inputNode) =
  renderInputTo i input parent
  where parent = iDiagram sl ml
        input  = drawElementTree inputNode
drawElementTree Leaf = mempty

draw :: (RealFloat n, Typeable n,
         Renderable (Diagrams.TwoD.Text.Text n) b,
         Renderable (Path V2 n) b,
         ToTree a)
        => Proxy a -> QDiagram b V2 n Any
draw t = drawElementTree element
  where (Root element) = toTree t
