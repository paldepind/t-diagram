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

-- {-# LANGUAGE CPP                   #-}
-- {-# LANGUAGE ConstraintKinds       #-}
-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
-- {-# LANGUAGE DeriveGeneric         #-}

-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PolyKinds             #-}

-- {-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeOperators         #-}

module Main where

import GHC.TypeLits
import Data.Proxy
import GHC.Exts (Constraint)

-- | A helper for applying constraints to several type variables
type family All c xs :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

class Eval (x :: Number) where
  eval :: Proxy x -> Int

data Element = Intepreter Symbol Symbol

-- data comp :: Intepreter a b -> Intepreter b c -> Intepreter a c
-- (:+:) :: Intepreter a b -> Intepreter b c -> Intepreter a c
data (a :: Element) :+: b

type family (a :: Element) :||: (b :: Element) :: Element
type instance Intepreter sl il :||: Intepreter il iil = Intepreter sl iil

class DiaComp (a :: Element) (b :: Element) where
  data Pl a b ::Element -> Element -> Element

printComp :: forall a b c. All KnownSymbol '[a, b, c] => Proxy (Intepreter a b) -> Proxy (Intepreter b c) -> String
printComp a b = "Intepreter from " ++ symbolVal (Proxy :: Proxy a) ++ " to " ++ symbolVal (Proxy :: Proxy b)

data Number = One | Add Number Number
type Two = Add One One

instance Eval One where
  eval _ = 1

instance (Eval a, Eval b) => Eval (Add a b) where
  eval _ = eval (Proxy :: Proxy a) + eval (Proxy :: Proxy b)
