{-# LANGUAGE FlexibleContexts #-}

-- | Generic deriving of 'Read' / 'Show' with no record labels.
--
-- Often one wants to create a @newtype@ which has a convieniant field
-- accessor like @unUserId@ below, but that makes the derived 'Show'
-- instance overly verbose.
--
-- For example:
--
-- @
-- newtype UserId = UserId { unUserId :: String } deriving (Show)
-- @
--
-- Renders as:
--
-- >>> show (UserId "simon")
-- UserId {unUserId = "simon"}
--
-- With 'qshowsPrec' you can have a @Show@ instance which doesn't print
-- the field labels. It will render as if the @unUserId@ accessor wasn't
-- present at all.
--
-- @
-- newtype UserId = UserId { unUserId :: String } deriving (Generic)
--
-- instance Show UserId where showsPrec = qshowsPrec
-- @
--
-- >>> show (UserId "simon")
-- UserId "simon"
--
-- A compatible 'Read' instance can also be derived using 'qreadPrec' if
-- necessary.
--
-- @
-- instance Read UserId where showsPrec = qreadPrec
-- @
--
module Quiet (
    qshowsPrec
  , qreadPrec
  ) where

import GHC.Generics (Generic(..), Rep)

import Text.ParserCombinators.ReadPrec (ReadPrec)

import Quiet.Internal (ConType(..), QShow(..), QRead(..))


-- | This implements a quiet version of 'Text.Show.showsPrec' which omits
--   labels for record fields when rendering constructors.
qshowsPrec :: (Generic a, QShow (Rep a)) => Int -> a -> ShowS
qshowsPrec n =
  qshowsPrec_ ConPrefix n . from

-- | This implements a quiet version of 'Text.Read.readPrec' which expects
--   labels for record fields to be omitted when parsing constructors.
qreadPrec :: (Generic a, QRead (Rep a)) => ReadPrec a
qreadPrec =
  fmap to (qreadPrec_ ConPrefix)
