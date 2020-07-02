import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum, guard, when, unless)
import Data.Char (isNumber, isPunctuation, isDigit)


askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s


data PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

instance Semigroup PwdError where
  (PwdError a) <> (PwdError b) = PwdError $ a ++ b

instance Monoid PwdError where
  mempty = PwdError ""

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."


validate :: String -> PwdErrorIOMonad String
validate pass = do
    when(length pass < 8) $ tellAndBreak "Incorrect input: password is too short!"
    unless(any isDigit  pass) $ tellAndBreak "Incorrect input: password must contain some digits!"
    unless(any isPunctuation pass)  $ tellAndBreak "Incorrect input: password must contain some punctuation!"
    return pass where

    tellAndBreak msg = do
      liftIO $ putStrLn msg
      throwE (PwdError msg)


-- GHCi> runExceptT askPassword
-- Enter your new password:
-- qwerty
-- Incorrect input: password is too short!
-- qwertyuiop
-- Incorrect input: password must contain some digits!
-- qwertyuiop123
-- Incorrect input: password must contain some punctuation!
-- qwertyuiop123!!!
-- Storing in database...
-- GHCi>

-- ExceptT PwdError IO a
getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  pass <- liftIO getLine
  validate pass

-- getValidPassword :: PwdErrorIOMonad String
-- getValidPassword = do
--   s <- liftIO getLine
--   catchE (validate s) (\(PwdError a) -> do liftIO $ putStrLn a; throwE $ PwdError a)

-- validate :: String -> PwdErrorIOMonad String
-- validate s = do
--   when (length s < 8) $ throwE $ PwdError "Incorrect input: password is too short!"
--   when (not $ any isNumber s) $ throwE $ PwdError "Incorrect input: password must contain some digits!"
--   when (not $ any isPunctuation s) $ throwE $ PwdError "Incorrect input: password must contain some punctuation!"
--   return s
