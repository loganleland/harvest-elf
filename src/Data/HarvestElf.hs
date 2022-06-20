-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HarvestElf
-- Copyright   :  (c) Logan Leland, 2022
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Logan Leland <ethicalmath@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Harvest (parse) the ELF format.
--
-----------------------------------------------------------------------------

module Data.HarvestElf
  (
    Elf,
    harvest,
    pprint,
    FH.ElfFHeader,
    PH.ElfPHeader,
    SH.ElfSHeader
  ) where

import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HarvestFHeader as FH
import qualified Data.HarvestPHeader as PH
import qualified Data.HarvestSHeader as SH

data Elf = Elf
  { fHeader :: FH.ElfFHeader
  , pHeader :: [PH.ElfPHeader]
  , sHeader :: [SH.ElfSHeader]
  }
  deriving Show

harvest :: BSL.ByteString -> Elf
harvest a = do
  let eheader = G.runGet FH.parseElfFHeader a
  let pheader = G.runGet (PH.parseElfPHeader eheader) a
  let sheader = G.runGet (SH.parseElfSHeader eheader) a
  Elf { fHeader = eheader, pHeader=pheader, sHeader=SH.section sheader eheader a}

pprint :: Elf -> String
pprint a = "Harvested Elf File: " ++ "\n"
           ++ FH.pprint (fHeader a) ++ "=========\n"
           ++ (concat $ map PH.pprint (pHeader a))
           ++ (concat $ map SH.pprint (sHeader a))
