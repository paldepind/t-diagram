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

squareSides t r b l =
  center $ top === (center $ left ||| strut (1 ^& 1) ||| right) === bottom
  where hideSide b s = if b then s else mempty
        top = hideSide t (hrule 1)
        right = hideSide r (vrule 1)
        bottom = hideSide b (hrule 1)
        left = hideSide l (vrule 1)

tSquare t r b l txt = (text txt <> (squareSides t r b l)) # fontSize (normalized 0.05)

tSquareTLBR = tSquare True True True True
tSquareRBL = tSquare False True True True
tSquareTBR = tSquare True False True True
tSquareTLB = tSquare True True True False
tSquareTRL = tSquare True True False True
tSquareTLR = tSquare True True False True
squareT = squareSides True False False False

alignTLS t = t # alignTL # moveOriginBy (V2 (-0.5) (-0.5))

alignBLS t = t # alignBL # moveOriginBy (V2 (-0.5) (0.5))

alignTC = alignT . centerX

alignBC = alignB . centerX

tDiagram :: (RealFloat n, Typeable n,
             Renderable (Diagrams.TwoD.Text.Text n) b,
             Renderable (Path V2 n) b) =>
            String -> String -> String -> QDiagram b V2 n Any
tDiagram sl tl il =
  ((tSquareTBR sl ||| squareT ||| tSquareTLB tl) # center === tSquareRBL il) # lw 1

iDiagram sl ml = tSquareTLR sl === tSquareRBL ml

pDiagram l = tSquareTRL l

iIntoT i t = alignBC i # translateY (-1) ||| strutX spc ||| alignTC t

iOntoI i1 i2 = alignBC i1 === strutY spc === alignTC i2

tOntoI t i = alignBC t === strutY spc === alignTC i

tIntoT t1 t2 = alignBR t1 # translate ((1 - spc) *^ dir) `atop` alignTL t2
  where dir = V2 1 (-1)

tIntoP t p = alignBC t === strutY spc === alignTC p

iIntoP i p = alignBC i === strutY spc === alignTC p
