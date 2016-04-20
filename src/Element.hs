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

module Element
       (
         Element
       ) where

import GHC.TypeLits
import Data.Proxy
import GHC.Exts (Constraint)

-- | A helper for applying constraints to several type variables
type family All c xs :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

data Element = Interpreter Symbol Symbol
             | Compiler Symbol Symbol Symbol
             | Element :> Element

-- data comp :: Interpreter a b -> Interpreter b c -> Interpreter a c
-- (:+:) :: Interpreter a b -> Interpreter b c -> Interpreter a c
-- data (a :: Element) :> b

-- type family (a :: Element) :||: (b :: Element) :: Element
-- type instance Interpreter sl il :||: Interpreter il iil = Interpreter sl iil

-- class DiaComp (a :: Element) (b :: Element) where
  -- data Pl a b ::Element -> Element -> Element

printComp :: forall a b c. All KnownSymbol '[a, b, c] => Proxy (Interpreter a b) -> Proxy (Interpreter b c) -> String
printComp a b = "Interpreter from " ++ symbolVal (Proxy :: Proxy a) ++ " to " ++ symbolVal (Proxy :: Proxy b)

class Render (d :: Element) where
  render :: Proxy d -> String

instance (All KnownSymbol '[s, i]) => Render (Interpreter s i) where
  render _ = "interpreter from " ++ symbolVal (Proxy :: Proxy s) ++ " in " ++ symbolVal (Proxy :: Proxy i)

instance (All KnownSymbol '[s, t, i]) => Render (Compiler s t i) where
  render _ = "compiler from " ++ source ++ " to " ++ target ++ " implemented in " ++ impl
    where source = symbolVal (Proxy :: Proxy s)
          target = symbolVal (Proxy :: Proxy t)
          impl = symbolVal (Proxy :: Proxy i)

instance (All KnownSymbol '[a, b, c]) => Render (Interpreter a b :> Interpreter b c) where
  render _ = (render (Proxy :: Proxy (Interpreter a b))) ++ " into " ++ (render (Proxy :: Proxy (Interpreter b c)))

-- instance (All KnownSymbol '[s, m, t, i]) => Render (Interpreter s m :> Compiler m t i) where
--   render _ = 

class Eval (x :: Number) where
  eval :: Proxy x -> Int

data Number = One | Add Number Number
type Two = Add One One

instance Eval One where
  eval _ = 1

instance (Eval a, Eval b) => Eval (Add a b) where
  eval _ = eval (Proxy :: Proxy a) + eval (Proxy :: Proxy b)
