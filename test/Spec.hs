{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import GHC.Generics (Generic)
import Quiet (Quiet(..))
import System.Exit (exitFailure)

newtype UserId = UserId { unUserId :: String }
  deriving (Generic)
  deriving (Read, Show) via (Quiet UserId)

--instance Show UserId where showsPrec = qshowsPrec

main :: IO ()
main =
  if show (UserId "simon") == "UserId \"simon\"" then
    pure ()
  else
    exitFailure
