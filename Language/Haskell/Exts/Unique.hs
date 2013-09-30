{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Haskell.Exts.Unique  where
import qualified Prelude 
import Control.Monad.Trans  (lift)
import Control.Monad.State  (put,get,evalState,State)
import Control.Monad.Reader (ask,ReaderT(..))
import Control.Monad.Error  (throwError,ErrorT(..))
import Prelude (($),Either(..),String,Int,succ,(++),return,Show(..)
               ,const,(.),flip)
 
type Unique a = ErrorT String (ReaderT String (State Int)) a  

newVar :: Unique String
newVar = do 
  seed <- ask
  r <- lift get 
  lift $ put $ succ r
  return $ seed ++ (Prelude.show r)

runUnique :: Unique a -> String -> Either String a
runUnique c seed =  evalState ((runReaderT $ runErrorT c) seed) 0

initUnique :: Unique ()
initUnique = lift $ put 0

error :: String -> Unique a
error = throwError

type Desugaring a = a -> Unique a

noDesugaring :: Desugaring a
noDesugaring = return

notSupported :: Desugaring a
notSupported = const $ error  "Not Supported!"

fromRightS :: Either String a -> a
fromRightS (Right x) = x
fromRightS (Left  x) = Prelude.error x

unUnique :: Unique a -> a
unUnique = fromRightS . flip runUnique "_x"

 