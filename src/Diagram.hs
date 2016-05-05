{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Diagram where

import Diagrams.Prelude hiding (All, (:>))
import Diagrams.Backend.SVG.CmdLine
import Data.Typeable
import Diagrams.TwoD.Text

-- tSquare :: (InSpace V2 n t, TrailLike t, OrderedField n) => String -> n -> _
-- tSquare :: String -> _ -> _

spc = 0.2

tSquare t s = (text t <> (square s)) # fontSize (normalized 0.08)

alignTLS t = t # alignTL # moveOriginBy (V2 (-0.5) (-0.5))

alignBLS t = t # alignBL # moveOriginBy (V2 (-0.5) (0.5))

alignTC = alignT . centerX

alignBC = alignB . centerX

tDiagram :: (RealFloat n, Typeable n,
             Renderable (Diagrams.TwoD.Text.Text n) b,
             Renderable (Path V2 n) b) =>
            String -> String -> String -> QDiagram b V2 n Any
tDiagram sl tl il =
  ((tSquare sl 1 ||| square 1 ||| tSquare tl 1) # center === tSquare il 1) # lw 1

-- iDiagram :: String -> String -> Diagram B
iDiagram sl ml = tSquare sl 1 === tSquare ml 1

iIntoT i t = alignBC i # translateY (-1) ||| strutX spc ||| alignTC t

iOntoI i1 i2 = alignBC i1 === strutY spc === alignTC i2

tOntoI t i = alignBC t === strutY spc === alignTC i

tIntoT t1 t2 = alignBR t1 # translate ((1 - spc) *^ dir) `atop` alignTL t2
  where dir = V2 1 (-1)
