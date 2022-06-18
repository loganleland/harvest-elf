-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HarvestPHeader
-- Copyright   :  (c) Logan Leland, 2022
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Logan Leland <ethicalmath@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Harvest the ELF Program Header format.
--
-----------------------------------------------------------------------------

module Data.HarvestPHeader
  where

import Data.Word
import Control.Monad (replicateM)
import qualified Data.Binary.Get as G
import Data.HarvestFHeader

data ElfPHeader = ElfPHeader
  { pTy :: Either Word32 PTy
  , pFlag :: Word32
  , pOff :: Word64
  , pVOff :: Word64
  , pPAddr :: Word64
  , pFilesz :: Word64
  , pMemsz :: Word64
  , pAlign :: Word64
  }
  deriving Show

pprint :: ElfPHeader -> String
pprint a = "ELF Program Header" ++ "\n"
           ++ "========" ++ "\n"
           ++ "P Type: " ++ show (pTy a) ++ "\n"
           ++ "P Flags: " ++ show (pFlag a) ++ "\n"
           ++ "P Offset: " ++ show (pOff a) ++ "\n"
           ++ "P Virtual Address: " ++ show (pVOff a) ++ "\n"
           ++ "P Physical Address: " ++ show (pPAddr a) ++ "\n"
           ++ "Segment File Image Size: " ++ show (pFilesz a) ++ "\n"
           ++ "Segment Memory Size: " ++ show (pMemsz a) ++ "\n"
           ++ "Alignment: " ++ show (pAlign a) ++ "\n"

-- Segment Type 
data PTy = PT_NULL | PT_LOAD | PT_DYNAMIC | PT_INTERP | PT_NOTE |
           PT_SHLIB | PT_PHDR | PT_TLS | PT_LOOS | PT_HIOS |
           PT_LOPROC | PT_HIPROC | PT_GNU_STACK | PT_GNU_EH_FRAME |
           PT_SUNW_EH_FRAME | PT_SUNW_UNWIND | PT_GNU_RELRO |
           PT_GNU_PROPERTY | PT_OPENBSD_RANDOMIZE | PT_OPENBSD_WXNEEDED |
           PT_OPENBSD_BOOTDATA | PT_ARM_ARCHEXT | PT_ARM_EXIDX | PT_ARM_UNWIND |
           PT_MIPS_REGINFO | PT_MIPS_RTPROC | PT_MIPS_OPTIONS | PT_MIPS_ABIFLAGS
  deriving (Eq, Show)

getPTy' :: Word32 -> Either Word8 ElfOSABI -> Either Word16 ElfMachine ->
           Either Word32 PTy
getPTy' a b c
  | a==0 = Right PT_NULL
  | a==1 = Right PT_LOAD
  | a==2 = Right PT_DYNAMIC
  | a==3 = Right PT_INTERP
  | a==4 = Right PT_NOTE
  | a==5 = Right PT_SHLIB
  | a==6 = Right PT_PHDR
  | a==7 = Right PT_TLS
  | a==1685382480 = Right PT_GNU_EH_FRAME
  | a==1685382480 && b==Right Solaris = Right PT_SUNW_EH_FRAME
  | a==1684333904 && b==Right Solaris = Right PT_SUNW_UNWIND
  | a==1685382482 = Right PT_GNU_RELRO
  | a==1685382483 = Right PT_GNU_PROPERTY
  | a==1705237478 = Right PT_OPENBSD_RANDOMIZE
  | a==1705237479 = Right PT_OPENBSD_WXNEEDED
  | a==1705253862 = Right PT_OPENBSD_BOOTDATA
  | a==1879048192 &&
    c==Right EM_ARM || c==Right EM_AARCH64 = Right PT_ARM_ARCHEXT
  --The below 2 have the same value, need to derive
  --from other info in header
  | a==1879048193 &&
    c==Right EM_ARM || c==Right EM_AARCH64 = Right PT_ARM_EXIDX
  | a==1879048193 &&
    c==Right EM_ARM || c==Right EM_AARCH64 = Right PT_ARM_UNWIND
  | a==1879048192 &&
    c==Right EM_MIPS || c==Right EM_MIPS_RS3_LE ||
    c==Right EM_MIPS_RS4_BE = Right PT_MIPS_REGINFO
  | a==1879048193 &&
    c== Right EM_MIPS || c==Right EM_MIPS_RS3_LE || c==Right EM_MIPS_RS4_BE
    = Right PT_MIPS_RTPROC
  | a==1879048194 &&
    c==Right EM_MIPS || c==Right EM_MIPS_RS3_LE || c==Right EM_MIPS_RS4_BE
    = Right PT_MIPS_OPTIONS
  | a==1879048195 &&
    c==Right EM_MIPS || c==Right EM_MIPS_RS3_LE || c==Right EM_MIPS_RS4_BE
    = Right PT_MIPS_ABIFLAGS
  | a==1685382481 = Right PT_GNU_STACK
  | a==1610612736 = Right PT_LOOS
  | a==1879048191 = Right PT_HIOS
  | a==1879048192 = Right PT_LOPROC  
  | a==2147483647 = Right PT_HIPROC
  | otherwise = Left a

parseElfPHeader :: Word64 -> Int -> Either Word8 ElfOSABI ->
                   Either Word16 ElfMachine -> G.Get [ElfPHeader]
parseElfPHeader a b c d = do
  G.skip (fromIntegral a)
  replicateM b $ parseElfPHeader' c d

parseElfPHeader' :: Either Word8 ElfOSABI -> Either Word16 ElfMachine ->
                    G.Get ElfPHeader
parseElfPHeader' a b = do
  pTy' <- G.getWord32le
  pFlag' <- G.getWord32le
  pOff' <- G.getWord64le
  pVOff' <- G.getWord64le
  pPAddr' <- G.getWord64le
  pFilesz' <- G.getWord64le
  pMemsz' <- G.getWord64le
  pAlign' <- G.getWord64le
  return ElfPHeader { pTy = getPTy' pTy' a b
                    , pFlag = pFlag'
                    , pOff = pOff'
                    , pVOff = pVOff'
                    , pPAddr = pPAddr'
                    , pFilesz = pFilesz'
                    , pMemsz = pMemsz'
                    , pAlign = pAlign'
                    }
