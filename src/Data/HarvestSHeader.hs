-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HarvestSHeader
-- Copyright   :  (c) Logan Leland, 2022
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Logan Leland <ethicalmath@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Harvest the ELF Segment Header format.
--
-----------------------------------------------------------------------------

module Data.HarvestSHeader
  where

import Data.Word
import Data.Bits (Bits, testBit)
import Control.Monad (replicateM)
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.HarvestFHeader as FH

----------------------------------------------
-- ELF Section
----------------------------------------------

data ElfSHeader' = ElfSHeader'
  { sName :: Word32
  , sTy :: Either Word32 ShTy
  , sFlags :: [ShFlag]
  , sAddr :: Word64
  , sOffset :: Word64
  , sSize :: Word64
  , sLink :: Word32
  , sInfo :: Word32
  , sAddrAlign :: Word64
  , sEntSize :: Word64
  }
  deriving Show

data ElfSHeader = ElfSHeader
  { name :: BS.ByteString
  , esh :: ElfSHeader'
  , dat :: BS.ByteString
  }
  deriving Show

printSHeader' :: ElfSHeader' -> String
printSHeader' a =
              "Offset into string table: " ++ show (sName a) ++ "\n"
           ++ "Section Type: " ++ show (sTy a) ++ "\n"
           ++ "Section Flags: " ++ show (sFlags a) ++ "\n"
           ++ "Virtual Address: " ++ show (sAddr a) ++ "\n"
           ++ "File Image Offset: " ++ show (sOffset a) ++ "\n"
           ++ "File Image Size: " ++ show (sSize a) ++ "\n"
           ++ "Index: " ++ show (sLink a) ++ "\n"
           ++ "Info: " ++ show (sInfo a) ++ "\n"
           ++ "Alignment: " ++ show (sAddrAlign a) ++ "\n"
           ++ "Entry Size: " ++ show (sEntSize a) ++ "\n"

pprint :: ElfSHeader -> String
pprint a = "ELF Section\n"
           ++ "========\n"
           ++ "Name: " ++ show (name a) ++ "\n"
           ++ printSHeader' (esh a)
           ++ "Data: " ++ show (dat a) ++ "\n"
           ++ "\n=======\n"

data ShTy = SHT_NULL | SHT_PROGBITS | SHT_SYMTAB | SHT_STRTAB | SHT_RELA |
           SHT_HASH | SHT_DYNAMIC | SHT_NOTE | SHT_NOBITS | SHT_REL |
           SHT_SHLIB | SHT_DYNSYM | SHT_INIT_ARRAY | SHT_FINI_ARRAY |
           SHT_PREINIT_ARRAY | SHT_GROUP | SHT_SYMTAB_SHNDX | SHT_RELR |
           SHT_LOOS | SHT_ANDROID_REL | SHT_ANDROID_RELA | SHT_LLVM_ODRTAB |
           SHT_LLVM_LINKER_OPTIONS | SHT_LLVM_ADDRSIG |
           SHT_LLVM_DEPENDENT_LIBRARIES | SHT_LLVM_SYMPART |
           SHT_LLVM_PART_EHDR | SHT_LLVM_PART_PHDR | SHT_LLVM_BB_ADDR_MAP |
           SHT_LLVM_CALL_GRAPH_PROFILE | SHT_ANDROID_RELR | SHT_GNU_ATTRIBUTES |
           SHT_GNU_HASH | SHT_GNU_verdef | SHT_GNU_verneed | SHT_GNU_versym |
           SHT_HIOS | SHT_LOPROC | SHT_ARM_EXIDX | SHT_ARM_PREEMPTMAP |
           SHT_ARM_ATTRIBUTES | SHT_ARM_DEBUGOVERLAY | SHT_ARM_OVERLAYSECTION |
           SHT_X86_64_UNWIND | SHT_MIPS_REGINFO | SHT_MIPS_OPTIONS |
           SHT_MIPS_DWARF | SHT_MIPS_ABIFLAGS | SHT_MSP430_ATTRIBUTES |
           SHT_RISCV_ATTRIBUTES | SHT_CSKY_ATTRIBUTES | SHT_HIPROC |
           SHT_LOUSER | SHT_HIUSER
  deriving (Show, Eq)

getShTy :: Word32 -> Either Word32 ShTy
getShTy a
  | a==0 = Right SHT_NULL
  | a==1 = Right SHT_PROGBITS
  | a==2 = Right SHT_SYMTAB
  | a==3 = Right SHT_STRTAB
  | a==4 = Right SHT_RELA
  | a==5 = Right SHT_HASH
  | a==6 = Right SHT_DYNAMIC
  | a==7 = Right SHT_NOTE
  | a==8 = Right SHT_NOBITS
  | a==9 = Right SHT_REL
  | a==10 = Right SHT_SHLIB
  | a==11 = Right SHT_DYNSYM
  | a==14 = Right SHT_INIT_ARRAY
  | a==15 = Right SHT_FINI_ARRAY
  | a==16 = Right SHT_PREINIT_ARRAY
  | a==17 = Right SHT_GROUP
  | a==18 = Right SHT_SYMTAB_SHNDX
  | a==19 = Right SHT_RELR
  | a==1610612736 = Right SHT_LOOS
  | a==1610612737 = Right SHT_ANDROID_REL
  | a==1610612738 = Right SHT_ANDROID_RELA
  | a==1879002112 = Right SHT_LLVM_ODRTAB
  | a==1879002113 = Right SHT_LLVM_LINKER_OPTIONS
  | a==1879002115 = Right SHT_LLVM_ADDRSIG
  | a==1879002116 = Right SHT_LLVM_DEPENDENT_LIBRARIES
  | a==1879002117 = Right SHT_LLVM_SYMPART
  | a==1879002118 = Right SHT_LLVM_PART_EHDR
  | a==1879002119 = Right SHT_LLVM_PART_PHDR
  | a==1879002120 = Right SHT_LLVM_BB_ADDR_MAP
  | a==1879002121 = Right SHT_LLVM_CALL_GRAPH_PROFILE
  | a==1879047936 = Right SHT_ANDROID_RELR
  | a==1879048181 = Right SHT_GNU_ATTRIBUTES
  | a==1879048182 = Right SHT_GNU_HASH
  | a==1879048189 = Right SHT_GNU_verdef
  | a==1879048190 = Right SHT_GNU_verneed
  | a==1879048191 = Right SHT_GNU_versym
  | a==1879048191 = Right SHT_HIOS
  | a==1879048192 = Right SHT_LOPROC
  | a==1879048193 = Right SHT_ARM_EXIDX
  | a==1879048194 = Right SHT_ARM_PREEMPTMAP
  | a==1879048195 = Right SHT_ARM_ATTRIBUTES
  | a==1879048196 = Right SHT_ARM_DEBUGOVERLAY
  | a==1879048197 = Right SHT_ARM_OVERLAYSECTION
  | a==1879048193 = Right SHT_X86_64_UNWIND
  | a==1879048198 = Right SHT_MIPS_REGINFO
  | a==1879048205 = Right SHT_MIPS_OPTIONS
  | a==1879048222 = Right SHT_MIPS_DWARF
  | a==1879048234 = Right SHT_MIPS_ABIFLAGS
  | a==1879048195 = Right SHT_MSP430_ATTRIBUTES
  | a==1879048195 = Right SHT_RISCV_ATTRIBUTES
  | a==2147483647 = Right SHT_HIPROC
  | a==2147483648 = Right SHT_LOUSER
  | a==4294967295 = Right SHT_HIUSER
  | otherwise = Left a

data ShFlag = SHF_WRITE | SHF_ALLOC | SHF_EXECINSTR | SHF_MERGE | SHF_EXT
  deriving (Show, Eq)

getShFlag64 :: Word64 -> [ShFlag]
getShFlag64 = getShFlag' 8

getShFlag32 :: Word32 -> [ShFlag]
getShFlag32 = getShFlag' 4


getShFlag' :: Bits a => Int -> a -> [ShFlag]
getShFlag' 0 _ = []
getShFlag' 1 a = if testBit a 1 then SHF_WRITE : getShFlag' 0 a else getShFlag' 0 a
getShFlag' 2 a = if testBit a 2 then SHF_ALLOC : getShFlag' 1 a else getShFlag' 1 a
getShFlag' 3 a = if testBit a 3 then SHF_EXECINSTR : getShFlag' 2 a else getShFlag' 2 a
getShFlag' _ b = getShFlag' 3 b

parseElfSHeader :: FH.ElfFHeader -> G.Get [ElfSHeader']
parseElfSHeader a = do
  G.skip $ fromIntegral $ FH.elfSectionHeaderOFF a
  replicateM (fromIntegral $ FH.elfSectionHeaderEntryCount a) $ parseElfSHeader' a

parseElfSHeader' :: FH.ElfFHeader -> G.Get ElfSHeader'
parseElfSHeader' a = do
  sName' <- G.getWord32le
  sTy' <- G.getWord32le
  e <- case FH.elfClass a of
    Right FH.ElfClass64 -> do
      sFlags' <- G.getWord64le
      sAddr' <- G.getWord64le
      sOffset' <- G.getWord64le
      sSize' <- G.getWord64le
      sLink' <- G.getWord32le
      sInfo' <- G.getWord32le
      sAddrAlign' <- G.getWord64le
      sEntSize' <- G.getWord64le
      return ElfSHeader' { sName = sName'
                         , sTy = getShTy sTy'
                         , sFlags = getShFlag64 sFlags'
                         , sAddr = sAddr'
                         , sOffset = sOffset'
                         , sSize = sSize'
                         , sLink = sLink'
                         , sInfo = sInfo'
                         , sAddrAlign = sAddrAlign'
                         , sEntSize = sEntSize'
                         }
    Right FH.ElfClass32 -> do
      sFlags' <- G.getWord32le
      sAddr' <- G.getWord32le
      sOffset' <- G.getWord32le
      sSize' <- G.getWord32le
      sLink' <- G.getWord32le
      sInfo' <- G.getWord32le
      sAddrAlign' <- G.getWord32le
      sEntSize' <- G.getWord32le
      return ElfSHeader' { sName = sName'
                         , sTy = getShTy sTy'
                         , sFlags = getShFlag32 sFlags'
                         , sAddr = fromIntegral sAddr'
                         , sOffset = fromIntegral sOffset'
                         , sSize = fromIntegral sSize'
                         , sLink = sLink'
                         , sInfo = fromIntegral sInfo'
                         , sAddrAlign = fromIntegral sAddrAlign'
                         , sEntSize = fromIntegral sEntSize'
                         }
    Left b -> fail $ "Cannot derive 32-bit or 64-bit: " ++ (show b)
  return e

stringTbl :: FH.ElfFHeader -> [ElfSHeader'] -> G.Get BS.ByteString
stringTbl a b = sectionData' (b!!nameOff)
  where
    nameOff = fromIntegral $ FH.elfSectionHeaderNameIndex a

sectionData' :: ElfSHeader' -> G.Get BS.ByteString
sectionData' a = do
  G.skip fOff
  G.getByteString fSize
  where
    fOff = fromIntegral $ sOffset a
    fSize = fromIntegral $ sSize a

sectionData :: [ElfSHeader'] -> BSL.ByteString -> [BS.ByteString]
sectionData a b = map (flip G.runGet b) $ map sectionData' a

-- string table -> offset into string table -> name
getSectionName :: BS.ByteString -> Int -> BS.ByteString
getSectionName a b = BS.takeWhile (not . (==0)) $ BS.drop b a

section :: [ElfSHeader'] -> FH.ElfFHeader -> BSL.ByteString -> [ElfSHeader]
section a b c = map comb $ zip stbL $ zip a sd
  where
    sd = sectionData a c
    stbL = map (getSectionName $ G.runGet (stringTbl b a) c) $ map (fromIntegral . sName) a
    comb = \l -> ElfSHeader { name = fst l, esh = fst $ snd l, dat = snd $ snd l}
