{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Quiet (qshowsPrec)
import System.Exit (exitFailure)

newtype UserId = UserId { unUserId :: String } deriving (Generic)

instance Show UserId where showsPrec = qshowsPrec

main :: IO ()
main =
  if show (UserId "simon") == "UserId \"simon\"" then
    pure ()
  else
    exitFailure
