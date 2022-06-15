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
    printElf
  ) where

import Data.Word
import Control.Monad (replicateM)
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

----------------------------------------------
-- ELF Header
----------------------------------------------

data ElfClass = ElfClass32 | ElfClass64
  deriving (Eq,Show)

data ElfEndianness = LE | BE
  deriving (Eq,Show)

data ElfVersion = Current
  deriving (Eq,Show)

data ElfOSABI
  = SystemV
  | HPUX
  | NetBSD
  | Linux
  | GNUHurd
  | Solaris
  | AIX
  | IRIX
  | FreeBSD
  | Tru64
  | Modesto
  | OpenBSD
  | OpenVMS
  | NonStopKernel
  | AROS
  | FenixOS
  | NuxiCloudABI
  deriving (Eq,Show)

data ElfType
  = ET_NONE
  | ET_REL
  | ET_EXEC
  | ET_DYN
  | ET_CORE
  | ET_LOOS
  | ET_HIOS
  | ET_LOPROC
  | ET_HIPROC
  deriving (Eq,Show)

data ElfMachine
  = EM_NONE
  | EM_M32
  | EM_SPARC
  | EM_386
  | EM_68K
  | EM_88K
  | EM_486
  | EM_860
  | EM_MIPS
  | EM_MIPS_RS3_LE
  | EM_MIPS_RS4_BE
  | EM_PARISC
  | EM_SPARC32PLUS
  | EM_PPC
  | EM_PPC64
  | EM_SPU
  | EM_ARM
  | EM_SH
  | EM_SPARCV9
  | EM_H8_300
  | EM_IA_64
  | EM_X86_64
  | EM_S390
  | EM_CRIS
  | EM_M32R
  | EM_MN10300
  | EM_OPENRISC
  | EM_ARCOMPACT
  | EM_XTENSA
  | EM_BLACKFIN
  | EM_UNICORE
  | EM_ALTERA_NIOS2
  | EM_TI_C6000
  | EM_HEXAGON
  | EM_NDS32
  | EM_AARCH64
  | EM_TILEPRO
  | EM_MICROBLAZE
  | EM_TILEGX
  | EM_ARCV2
  | EM_RISCV
  | EM_BPF
  | EM_CSKY
  | EM_LOONGARCH
  | EM_FRV
  | EM_ALPHA
  | EM_CYGNUS_M32R
  | EM_S390_OLD
  | EM_CYGNUS_MN10300
  deriving (Eq,Show)

elfMagic' :: Word32 -> Either Word32 Word32
elfMagic' a
  | a == 1179403647 = Right a
  | otherwise = Left a

elfClass' :: Word8 -> Either Word8 ElfClass
elfClass' a
  | a == 1 = Right ElfClass32
  | a == 2 = Right ElfClass64
  | otherwise = Left a

elfEndian' :: Word8 -> Either Word8 ElfEndianness
elfEndian' a
  | a == 1 = Right LE
  | a == 2 = Right BE
  | otherwise = Left a

elfEIVersion' :: Word8 -> Either Word8 ElfVersion
elfEIVersion' a
  | a == 1 = Right Current
  | otherwise = Left a

elfVersion' :: Word32 -> Either Word32 ElfVersion
elfVersion' a
  | a == 1 = Right Current
  | otherwise = Left a

elfOSABI' :: Word8 -> Either Word8 ElfOSABI
elfOSABI' a
  | a == 0 = Right SystemV
  | a == 1 = Right HPUX
  | a == 2 = Right NetBSD
  | a == 3 = Right Linux
  | a == 4 = Right GNUHurd
  | a == 6 = Right Solaris
  | a == 7 = Right AIX
  | a == 8 = Right IRIX
  | a == 9 = Right FreeBSD
  | a == 10 = Right Tru64
  | a == 11 = Right Modesto
  | a == 12 = Right OpenBSD
  | a == 13 = Right OpenVMS
  | a == 14 = Right NonStopKernel
  | a == 15 = Right AROS
  | a == 16 = Right FenixOS
  | a == 17 = Right NuxiCloudABI
  | otherwise = Left a

elfType' :: Word16 -> Either Word16 ElfType
elfType' a
  | a == 0 = Right ET_NONE
  | a == 1 = Right ET_REL
  | a == 2 = Right ET_EXEC
  | a == 3 = Right ET_DYN
  | a == 4 = Right ET_CORE
  | otherwise = Left a

elfMachine' :: Word16 -> Either Word16 ElfMachine
elfMachine' a
  | a == 0 = Right EM_NONE
  | a == 1 = Right EM_M32
  | a == 2 = Right EM_SPARC
  | a == 3 = Right EM_386
  | a == 4 = Right EM_68K
  | a == 5 = Right EM_88K
  | a == 6 = Right EM_486 -- Perhaps disused
  | a == 7 = Right EM_860
  | a == 8 = Right EM_MIPS --MIPS R3000 (officially, big-endian only)
  | a == 15 = Right EM_PARISC --HPPA
  | a == 18 = Right EM_SPARC32PLUS --Sun's "v8plus"
  | a == 20 = Right EM_PPC --PowerPC
  | a == 21 = Right EM_PPC64 --PowerPC64
  | a == 23 = Right EM_SPU --Cell BE SPU
  | a == 40 = Right EM_ARM --ARM 32 bit
  | a == 42 = Right EM_SH --SuperH
  | a == 43 = Right EM_SPARCV9 --SPARC v9 64-bit
  | a == 46 = Right EM_H8_300 --Renesas H8/300
  | a == 50 = Right EM_IA_64 --HP/Intel IA-64
  | a == 62 = Right EM_X86_64 --AMD x86-64
  | a == 22 = Right EM_S390 --IBM S/390
  | a == 76 = Right EM_CRIS --Axis Communications 32-bit embedded processor
  | a == 88 = Right EM_M32R --Renesas M32R
  | a == 89 = Right EM_MN10300 --Panasonic/MEI MN10300, AM33
  | a == 92 = Right EM_OPENRISC --OpenRISC 32-bit embedded processor
  | a == 93 = Right EM_ARCOMPACT --ARCompact processor
  | a == 94 = Right EM_XTENSA --Tensilica Xtensa Architecture
  | a == 106 = Right EM_BLACKFIN --ADI Blackfin Processor
  | a == 110 = Right EM_UNICORE --UniCore-32
  | a == 113 = Right EM_ALTERA_NIOS2 --Altera Nios II soft-core processor
  | a == 140 = Right EM_TI_C6000 --TI C6X DSPs
  | a == 164 = Right EM_HEXAGON --QUALCOMM Hexagon
  | a == 167 = Right EM_NDS32 --Andes Technology compact code size
                              --embedded RISC processor family
  | a == 183 = Right EM_AARCH64 --ARM 64 bit
  | a == 188 = Right EM_TILEPRO --Tilera TILEPro
  | a == 189 = Right EM_MICROBLAZE --Xilinx MicroBlaze
  | a == 191 = Right EM_TILEGX --Tilera TILE-Gx
  | a == 195 = Right EM_ARCV2 --ARCv2 Cores
  | a == 243 = Right EM_RISCV --RISC-V
  | a == 247 = Right EM_BPF --Linux BPF - in-kernel virtual machine
  | a == 252 = Right EM_CSKY --C-SKY
  | a == 258 = Right EM_LOONGARCH --LoongArch
  | a == 21569 = Right EM_FRV --Fujitsu FR-V
  --This is an interim value that we will use until the committee comes
  --up with a final number.
  | a == 36902 = Right EM_ALPHA
  --Bogus old m32r magic number, used by old tools.
  | a == 36929 = Right EM_CYGNUS_M32R
  --This is the old interim value for S/390 architecture
  | a == 41872 = Right EM_S390_OLD
  --Also Panasonic/MEI MN10300, AM33
  | a == 48879 = Right EM_CYGNUS_MN10300
  | otherwise = Left a
 
data ElfHeader = ElfHeader
  { elfMagic :: Either Word32 Word32
  , elfClass :: Either Word8 ElfClass
  , elfEIEndian :: Either Word8 ElfEndianness
  , elfEI_Version :: Either Word8 ElfVersion
  , elfEI_OSABI :: Either Word8 ElfOSABI
  , elfEI_ABIVersion :: Word8
  , elfPad :: [Word8]
  , elfTy :: Either Word16 ElfType
  , elfMachine :: Either Word16 ElfMachine
  , elfVersion :: Either Word32 ElfVersion
  , elfEntry :: Word64
  , elfProgramHeaderOFF :: Word64
  , elfSectionHeaderOFF :: Word64
  , elfFlags :: Word32
  , elfHeaderSize :: Word16
  , elfProgramHeaderEntrySize :: Word16
  , elfProgramHeaderEntryCount :: Word16
  , elfSectionHeaderEntrySize :: Word16
  , elfSectionHeaderEntryCount :: Word16
  , elfSectionHeaderNameIndex :: Word16
  } deriving (Eq, Show)

printHeader :: ElfHeader -> String
printHeader a = "========\n"
           ++ "ELF Header" ++ "\n"
           ++ "========\n"
           ++ "Magic: " ++ show (elfMagic a) ++ "\n"
           ++ "EI Class: " ++ show (elfClass a) ++ "\n"
           ++ "Endian: " ++ show (elfEIEndian a) ++ "\n"
           ++ "EI Version: " ++ show (elfEI_Version a) ++ "\n"
           ++ "EI OSABI: " ++ show (elfEI_OSABI a) ++ "\n"
           ++ "EI ABI Version: " ++ show (elfEI_ABIVersion a) ++ "\n"
           ++ "Pad: " ++ show (elfPad a) ++ "\n"
           ++ "E_Type: " ++ show (elfTy a) ++ "\n"
           ++ "Machine: " ++ show (elfMachine a) ++ "\n"
           ++ "Version: " ++ show (elfVersion a) ++ "\n"
           ++ "Entry: " ++ show (elfEntry a) ++ "\n"
           ++ "Program Header Offset: " ++ show (elfProgramHeaderOFF a) ++ "\n"
           ++ "Section Header Offset: " ++ show (elfSectionHeaderOFF a) ++ "\n"
           ++ "Flags: " ++ show (elfFlags a) ++ "\n"
           ++ "Header Size: " ++ show (elfHeaderSize a) ++ "\n"
           ++ "Program Header Entry Size: " ++ show (elfProgramHeaderEntrySize a) ++ "\n"
           ++ "Program Header Entry Count: " ++ show (elfProgramHeaderEntryCount a) ++ "\n"
           ++ "Section Header Entry Size: " ++ show (elfSectionHeaderEntrySize a) ++ "\n"
           ++ "Section Header Entry Count: " ++ show (elfSectionHeaderEntryCount a) ++ "\n"
           ++ "Header Name Index: " ++ show (elfSectionHeaderNameIndex a) ++ "\n"

parseElfHeader :: G.Get ElfHeader
parseElfHeader = do
  ei_mag <- G.getWord32le
  ei_class <- G.getWord8
  ei_data <- G.getWord8
  ei_version <- G.getWord8
  ei_osabi <- G.getWord8
  ei_abiVersion <- G.getWord8
  ei_pad <- replicateM 7 G.getWord8
  ty <- G.getWord16le
  machine <- G.getWord16le
  version <- G.getWord32le
  entry <- G.getWord64le
  pheader_off <- G.getWord64le
  sheader_off <- G.getWord64le
  flags <- G.getWord32le
  header_size <- G.getWord16le
  pheader_entry_size <- G.getWord16le
  pheader_entry_count <- G.getWord16le
  sheader_entry_size <- G.getWord16le
  sheader_entry_count <- G.getWord16le
  sheader_name_index <- G.getWord16le

  return ElfHeader { elfMagic = elfMagic' ei_mag
                   , elfClass = elfClass' ei_class
                   , elfEIEndian = elfEndian' ei_data
                   , elfEI_Version = elfEIVersion' ei_version
                   , elfEI_OSABI = elfOSABI' ei_osabi
                   , elfEI_ABIVersion = ei_abiVersion
                   , elfPad = ei_pad
                   , elfTy = elfType' ty
                   , elfMachine = elfMachine' machine
                   , elfVersion = elfVersion' version
                   , elfEntry = entry
                   , elfProgramHeaderOFF = pheader_off
                   , elfSectionHeaderOFF = sheader_off
                   , elfFlags = flags
                   , elfHeaderSize = header_size
                   , elfProgramHeaderEntrySize = pheader_entry_size
                   , elfProgramHeaderEntryCount = pheader_entry_count
                   , elfSectionHeaderEntrySize = sheader_entry_size
                   , elfSectionHeaderEntryCount = sheader_entry_count
                   , elfSectionHeaderNameIndex = sheader_name_index
                   }

----------------------------------------------
-- ELF Program Header
----------------------------------------------

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

printPHeader :: ElfPHeader -> String
printPHeader a = "ELF Program Header" ++ "\n"
           ++ "========" ++ "\n"
           ++ "P Type: " ++ show (pTy a) ++ "\n"
           ++ "P Flags: " ++ show (pFlag a) ++ "\n"
           ++ "P Offset: " ++ show (pOff a) ++ "\n"
           ++ "P Virtual Address: " ++ show (pVOff a) ++ "\n"
           ++ "P Physical Address: " ++ show (pPAddr a) ++ "\n"
           ++ "Segment File Image Size: " ++ show (pFilesz a) ++ "\n"
           ++ "Segment Memory Size: " ++ show (pMemsz a) ++ "\n"
           ++ "Alignment: " ++ show (pAlign a) ++ "\n"
 
data PTy = PT_NULL | PT_LOAD | PT_DYNAMIC | PT_INTERP | PT_NOTE |
           PT_SHLIB | PT_PHDR | PT_TLS | PT_LOOS | PT_HIOS |
           PT_LOPROC | PT_HIPROC
  deriving (Eq, Show)

getPTy' :: Word32 -> Either Word32 PTy
getPTy' a
  | a==0 = Right PT_NULL
  | a==1 = Right PT_LOAD
  | a==2 = Right PT_DYNAMIC
  | a==3 = Right PT_INTERP
  | a==4 = Right PT_NOTE
  | a==5 = Right PT_SHLIB
  | a==6 = Right PT_PHDR
  | a==7 = Right PT_TLS
  | a==1610612736 = Right PT_LOOS
  | a==1879048191 = Right PT_HIOS
  | a==1879048192 = Right PT_LOPROC  
  | a==2147483647 = Right PT_HIPROC

parseElfPHeader :: Word64 -> Int -> G.Get [ElfPHeader]
parseElfPHeader a b = do
  G.skip (fromIntegral a)
  replicateM b parseElfPHeader'

parseElfPHeader' :: G.Get ElfPHeader
parseElfPHeader' = do
  pTy' <- G.getWord32le
  pFlag' <- G.getWord32le
  pOff' <- G.getWord64le
  pVOff' <- G.getWord64le
  pPAddr' <- G.getWord64le
  pFilesz' <- G.getWord64le
  pMemsz' <- G.getWord64le
  pAlign' <- G.getWord64le
  return ElfPHeader { pTy = getPTy' pTy'
                    , pFlag = pFlag'
                    , pOff = pOff'
                    , pVOff = pVOff'
                    , pPAddr = pPAddr'
                    , pFilesz = pFilesz'
                    , pMemsz = pMemsz'
                    , pAlign = pAlign'
                    }

----------------------------------------------
-- ELF Section
----------------------------------------------

data ElfSHeader' = ElfSHeader'
  { sName :: Word32
  , sTy :: Either Word32 ShTy
  , sFlags :: Either Word64 ShFlag
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

printSHeader :: ElfSHeader -> String
printSHeader a = "ELF Section\n"
           ++ "========\n"
           ++ "Name: " ++ show (name a) ++ "\n"
           ++ printSHeader' (esh a)
           ++ "Data: " ++ show (dat a) ++ "\n"
           ++ "\n=======\n"

data ShTy = SHT_NULL | SHT_PROGBITS | SHT_SYMTAB | SHT_STRTAB | SHT_RELA |
            SHT_HASH | SHT_DYNAMIC | SHT_NOTE | SHT_NOBITS | SHT_REL |
            SHT_SHLIB | SHT_DYNSYM | SHT_INIT_ARRAY | SHT_FINI_ARRAY |
            SHT_PREINIT_ARRAY | SHT_GROUP | SHT_SYMTAB_SHNDX | SHT_NUM |
            SHT_LOOS            
  deriving (Show, Eq)

getShTy :: Word32 -> Either Word32 ShTy
getShTy a
  | a == 0 = Right SHT_NULL
  | a == 1 = Right SHT_PROGBITS
  | a == 2 = Right SHT_SYMTAB
  | a == 3 = Right SHT_STRTAB
  | a == 4 = Right SHT_RELA
  | a == 5 = Right SHT_HASH
  | a == 6 = Right SHT_DYNAMIC
  | a == 7 = Right SHT_NOTE
  | a == 8 = Right SHT_NOBITS
  | a == 9 = Right SHT_REL
  | a == 10 = Right SHT_SHLIB
  | a == 11 = Right SHT_DYNSYM
  | a == 12 = Right SHT_INIT_ARRAY
  | a == 13 = Right SHT_FINI_ARRAY
  | a == 14 = Right SHT_PREINIT_ARRAY
  | a == 15 = Right SHT_GROUP
  | a == 16 = Right SHT_SYMTAB_SHNDX
  | a == 17 = Right SHT_NUM
  | a == 1610612736 = Right SHT_LOOS
  | otherwise = Left a

data ShFlag = SHF_WRITE | SHF_ALLOC | SHF_EXECINSTR | SHF_MERGE |
              SHF_STRINGS | SHF_INFO_LINK | SHF_LINK_ORDER |
              SHF_OS_NONCONFORMING | SHF_GROUP | SHF_TLS | SHF_MASKOS |
              SHF_MASKPROC | SHF_ORDERED | SHF_EXCLUDE
  deriving (Show, Eq)

getShFlag :: Word64 -> Either Word64 ShFlag
getShFlag a
  | a == 1 = Right SHF_WRITE
  | a == 2 = Right SHF_ALLOC
  | a == 4 = Right SHF_EXECINSTR
  | a == 16 = Right SHF_MERGE
  | a == 32 = Right SHF_STRINGS
  | a == 64 = Right SHF_INFO_LINK
  | a == 128 = Right SHF_LINK_ORDER
  | a == 256 = Right SHF_OS_NONCONFORMING
  | a == 512 = Right SHF_GROUP
  | a == 1024 = Right SHF_TLS
  | a == 267386880 = Right SHF_MASKOS
  | a == 4026531840 = Right SHF_MASKPROC
  | a == 67108864 = Right SHF_ORDERED
  | a == 134217728 = Right SHF_EXCLUDE
  | otherwise = Left a

parseElfSHeader :: Word64 -> Int -> G.Get [ElfSHeader']
parseElfSHeader a b = do
  G.skip (fromIntegral a)
  replicateM b parseElfSHeader'

parseElfSHeader' :: G.Get ElfSHeader'
parseElfSHeader' = do
  sName' <- G.getWord32le
  sTy' <- G.getWord32le
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
                     , sFlags = getShFlag sFlags'
                     , sAddr = sAddr'
                     , sOffset = sOffset'
                     , sSize = sSize'
                     , sLink = sLink'
                     , sInfo = sInfo'
                     , sAddrAlign = sAddrAlign'
                     , sEntSize = sEntSize'
                     }

stringTbl :: ElfHeader -> [ElfSHeader'] -> G.Get BS.ByteString
stringTbl a b = sectionData' (b!!nameOff)
  where
    nameOff = fromIntegral $ elfSectionHeaderNameIndex a

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

section :: [ElfSHeader'] -> ElfHeader -> BSL.ByteString -> [ElfSHeader]
section a b c = map comb $ zip stbL $ zip a sd
  where
    sd = sectionData a c
    stbL = map (getSectionName $ G.runGet (stringTbl b a) c) $ map (fromIntegral . sName) a
    comb = \l -> ElfSHeader { name = fst l, esh = fst $ snd l, dat = snd $ snd l}

data Elf = Elf
  { file :: ElfHeader
  , pHeader :: [ElfPHeader]
  , sHeader :: [ElfSHeader]
  }
  deriving Show

elf :: BSL.ByteString -> Elf
elf a = do
  let eheader = G.runGet parseElfHeader a
  let pheader = G.runGet (parseElfPHeader (elfProgramHeaderOFF eheader) (fromIntegral $ elfProgramHeaderEntryCount eheader)) a
  let sheader = G.runGet (parseElfSHeader (elfSectionHeaderOFF eheader) (fromIntegral $ elfSectionHeaderEntryCount eheader)) a
  Elf { file = eheader, pHeader=pheader, sHeader=section sheader eheader a}

printElf :: Elf -> String
printElf a = "Harvested Elf File: " ++ "\n"
           ++ printHeader (file a) ++ "=========\n"
           ++ (concat $ map printPHeader (pHeader a))
           ++ (concat $ map printSHeader (sHeader a))
