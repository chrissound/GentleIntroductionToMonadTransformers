-- start snippet main
{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

--start snippet EitherIO
data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}
--end snippet EitherIO

--start snippet EitherIOFunctor
instance Functor (EitherIO e) where
  fmap f ex = wrapped
    where
      unwrapped = runEitherIO ex
      fmapped   = fmap (fmap f) unwrapped
      wrapped   = EitherIO fmapped
--end snippet EitherIOFunctor

--start snippet EitherIOApMon
instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)
--end snippet EitherIOApMon

-- start snippet LoginError
data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show
-- end snippet LoginError

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail
-- end snippet main

-- start snippet printResult'
printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text        -> T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"
-- end snippet printResult'

-- start snippet printResult
printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")
-- end snippet printResult

-- start snippet getToken
getToken :: EitherIO LoginError Text
getToken = do
  EitherIO (fmap Right (T.putStrLn "Enter email address:"))
  input <- EitherIO (fmap Right T.getLine)
  EitherIO (return (getDomain input))
-- end snippet printResult

-- start snippet users
users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]
-- end snippet users

-- start snippet userLogin
userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userpw == password
             then return token

             else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left
-- end snippet userLogin

