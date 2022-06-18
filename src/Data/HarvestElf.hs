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
-- Harvest the ELF format.
-- https://refspecs.linuxfoundation.org/elf/gabi4+/
--
-----------------------------------------------------------------------------

module Data.HarvestElf
  (
    Elf,
    elf,
    printElf,
    PH.ElfPHeader,
    SH.ElfSHeader
  ) where

import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BSL
import Data.HarvestFHeader
import qualified Data.HarvestPHeader as PH
import qualified Data.HarvestSHeader as SH

data Elf = Elf
  { file :: ElfFHeader
  , pHeader :: [PH.ElfPHeader]
  , sHeader :: [SH.ElfSHeader]
  }
  deriving Show

elf :: BSL.ByteString -> Elf
elf a = do
  let eheader = G.runGet parseElfFHeader a
  let pheader = G.runGet (PH.parseElfPHeader (elfProgramHeaderOFF eheader) (fromIntegral $ elfProgramHeaderEntryCount eheader) (elfEI_OSABI eheader) (elfMachine eheader)) a
  let sheader = G.runGet (SH.parseElfSHeader (elfSectionHeaderOFF eheader) (fromIntegral $ elfSectionHeaderEntryCount eheader)) a
  Elf { file = eheader, pHeader=pheader, sHeader=SH.section sheader eheader a}

printElf :: Elf -> String
printElf a = "Harvested Elf File: " ++ "\n"
           ++ printHeader (file a) ++ "=========\n"
           ++ (concat $ map PH.pprint (pHeader a))
           ++ (concat $ map SH.pprint (sHeader a))
