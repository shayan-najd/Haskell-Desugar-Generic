{-# LANGUAGE Rank2Types,ScopedTypeVariables #-}
module Language.Haskell.Exts.SimpleGenerics where

import Data.Data
import Data.Maybe(fromJust)

-- taken from SYB
everywhere' :: (forall a. Data a => a -> a)
            -> (forall a. Data a => a -> a)
everywhere' f = gmapT (everywhere' f) . f


generic :: forall a b. (Data a, Data b) => 
           (a -> a) -> b -> b
generic f = everywhere' 
            (\e -> if (typeOf e == typeOf (undefined :: a))
                   then fromJust $ cast $ f $ fromJust $ cast e
                   else e)
              
everywhereM' :: Monad m => (forall a. Data a => a -> m a) 
                        -> (forall a. Data a => a -> m a)
everywhereM' f x = do x' <- f x
                      gmapM (everywhereM' f) x'
                                
genericM :: forall a b m. (Monad m,Data a, Data b) => 
           (a -> m a) -> b -> m b
genericM f = everywhereM' 
             (\e -> if (typeOf e == typeOf (undefined :: a))
                    then fromJust $ gcast $ f $ fromJust $ cast e
                    else return e)