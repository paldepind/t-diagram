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

module Element
       (
         Element(Compiler, Interpreter, (:>))
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

-- | Set the input field of an ElementTree
setInput :: ElementTree -> ElementTree -> ElementTree
setInput (CompilerNode sl tl il _) i = CompilerNode sl tl il i
setInput (InterpreterNode il ml _) i = InterpreterNode il ml i

printComp :: forall a b c. All KnownSymbol '[a, b, c] => Proxy (Interpreter a b) -> Proxy (Interpreter b c) -> String
printComp a b = "Interpreter from " ++ symbolVal (Proxy :: Proxy a) ++ " to " ++ symbolVal (Proxy :: Proxy b)

class Stringify (d :: Element) where
  stringify :: Proxy d -> String

instance (All KnownSymbol '[s, i]) => Stringify (Interpreter s i) where
  stringify _ = "interpreter from " ++ symbolVal (Proxy :: Proxy s) ++ " in " ++ symbolVal (Proxy :: Proxy i)

instance (All KnownSymbol '[s, t, i]) => Stringify (Compiler s t i) where
  stringify _ = "compiler from " ++ source ++ " to " ++ target ++ " implemented in " ++ impl
    where source = symbolVal (Proxy :: Proxy s)
          target = symbolVal (Proxy :: Proxy t)
          impl = symbolVal (Proxy :: Proxy i)

instance (All KnownSymbol '[a, b, c]) => Stringify (Interpreter a b :> Interpreter b c) where
  stringify _ = (stringify (Proxy :: Proxy (Interpreter a b))) ++ " into " ++ (stringify (Proxy :: Proxy (Interpreter b c)))

-- | Class for turning Element types into tree at the term level
class ToTree (d :: Element) where
  toTree :: Proxy d -> ElementTree

instance (All KnownSymbol '[sl, tl, il]) => ToTree (Compiler sl tl il) where
  toTree _ = CompilerNode sl tl il Leaf
    where sl = symbolVal (Proxy :: Proxy sl)
          tl = symbolVal (Proxy :: Proxy tl)
          il = symbolVal (Proxy :: Proxy il)

instance (All KnownSymbol '[sl, ml]) => ToTree (Interpreter sl ml) where
  toTree _ = InterpreterNode sl ml Leaf
    where sl = symbolVal (Proxy :: Proxy sl)
          ml = symbolVal (Proxy :: Proxy ml)

instance (All KnownSymbol '[sl, ml, tl, il], ToTree (Interpreter sl ml), ToTree (Compiler ml tl il))
         => ToTree (Interpreter sl ml :> Compiler ml tl il) where
  toTree _ = setInput compilerNode interpreterNode
    where interpreterNode = toTree (Proxy :: Proxy (Interpreter sl ml))
          compilerNode    = toTree (Proxy :: Proxy (Compiler ml tl il))

instance (All KnownSymbol '[sl, ml, ml'], ToTree (Interpreter sl ml), ToTree (Interpreter ml ml'))
         => ToTree (Interpreter sl ml :> Interpreter ml ml') where
  toTree _ = setInput interpreterNode' interpreterNode
    where interpreterNode  = toTree (Proxy :: Proxy (Interpreter sl ml))
          interpreterNode' = toTree (Proxy :: Proxy (Interpreter ml ml'))          

instance (All KnownSymbol '[sl, tl, il, ml], ToTree (Compiler sl tl il), ToTree (Interpreter il ml))
         => ToTree (Compiler sl tl il :> Interpreter il ml) where
  toTree _ = setInput compilerNode interpreterNode
    where compilerNode    = toTree (Proxy :: Proxy (Compiler ml tl il))
          interpreterNode = toTree (Proxy :: Proxy (Interpreter il ml))

inputTo Leaf parent = parent
inputTo (CompilerNode sl tl il _) parent =
  (tDiagram sl tl il) `tIntoT` parent
inputTo (InterpreterNode sl ml _) parent =
  (iDiagram sl ml) `iIntoT` parent

drawElementTree (CompilerNode sl tl il inputElement) =
   inputElement `inputTo` tDiagram sl tl il
drawElementTree (InterpreterNode sl ml inputElement) =
   inputElement `inputTo` iDiagram sl ml

draw :: (RealFloat n, Typeable n,
         Renderable (Diagrams.TwoD.Text.Text n) b,
         Renderable (Path V2 n) b,
         ToTree a)
        => Proxy a -> QDiagram b V2 n Any
draw = drawElementTree . toTree


-- class Draw (d :: Element) where
--   draw :: (RealFloat n, Typeable n,
--            Renderable (Diagrams.TwoD.Text.Text n) b,
--            Renderable (Path V2 n) b) 
--           => Proxy d -> QDiagram b V2 n Any

-- instance (All KnownSymbol '[s, t, i]) => Draw (Compiler s t i) where
--   draw _ = tDiagram source target impl
--     where source = symbolVal (Proxy :: Proxy s)
--           target = symbolVal (Proxy :: Proxy t)
--           impl = symbolVal (Proxy :: Proxy i)

-- instance (All KnownSymbol '[s, t, ct, i], Draw (Interpreter s t), Draw (Compiler t ct i)) =>
--          Draw (Interpreter s t :> Compiler t ct i) where
--   draw _ = iIntoT (draw (Proxy :: Proxy (Interpreter s t))) (draw (Proxy :: Proxy (Compiler t ct i)))
