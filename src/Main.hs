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
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude hiding ((:>))
import Diagrams.Backend.SVG.CmdLine
import Data.Proxy

import Element
import Diagram

iX86inARM = iDiagram "x86" "ARM"
iPerlInX86 = iDiagram "Perl" "x86"
mlToCInPerl = tDiagram "ML" "C" "Perl"
proToArmInMl = tDiagram "PL" "ARM" "C"

-- main = mainWith (proToArmInMl `tIntoT` mlToCInPerl `tOntoI` iPerlInX86 `iOntoI` iX86inARM)
-- main = mainWith (draw (Proxy :: Proxy (Compiler "ML" "C" "Perl")) :: Diagram B)
type Dia = Interpreter "Lisp" "Haskell" :> Interpreter "Haskell" "ML" :> Compiler "ML" "C" "Perl"
-- type Dia = Interpreter "Haskell" "ML" :> Compiler "ML" "C" "Perl"
-- type Dia = Compiler "Haskell" "ML" "C" :> Interpreter "C" "x86"
-- type Dia = Interpreter "C" "Lisp" :> Compiler "Lisp" "HS" "Perl" :> Compiler "Perl" "C" "x86"
main = mainWith (draw (Proxy :: Proxy Dia) :: Diagram B)
