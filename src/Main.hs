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

module Main where

import Diagrams.Backend.SVG.CmdLine

import Element
import Diagram

iX86inARM = iDiagram "x86" "ARM"
iPerlInX86 = iDiagram "Perl" "x86"
mlToCInPerl = tDiagram "ML" "C" "Perl"
proToArmInMl = tDiagram "PL" "ARM" "C"

main = mainWith (proToArmInMl `tIntoT` mlToCInPerl `tOntoI` iPerlInX86 `iOntoI` iX86inARM)
