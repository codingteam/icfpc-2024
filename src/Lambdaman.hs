{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

module Lambdaman where

import Control.Monad
import Control.Monad.State
import Data.Array
import GHC.Generics
import Data.Hashable
import qualified Data.HashSet as H
import qualified Data.PQueue.Prio.Min as Q

data Cell = Empty | Pill | Wall | Man
    deriving (Eq, Show, Generic)

instance Hashable Cell

data Direction = U | R | D | L
    deriving (Eq, Show)

type Position = (Int, Int)
type Grid = Array Position Cell

data Problem = Problem {
    pGrid :: !Grid
  , pPosition :: !Position
  }
  deriving (Show)

instance Hashable (Array (Int,Int) Cell) where
    hashWithSalt salt a = hashWithSalt salt $ elems a

type Value = Int

type Path = [(Direction, Grid)]

data AState = AState {
      aOpen :: !(Q.MinPQueue Int Path)
    , aClosed :: !(H.HashSet Grid)
    }

type A a = State AState a

aStar :: Problem -> A (Maybe Path)
aStar p = do
        put $ AState Q.empty H.empty
        loop
    where
        loop = do
            open <- gets aOpen
            case Q.minView open of
                Nothing -> return Nothing
                Just (path, rest) -> do
                    modify $ \st -> st {aOpen = rest}
                    let grid = evalPath p path
