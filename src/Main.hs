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
getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)
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

