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
    elf
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
  = ElfSystemV
  | ElfHPUX
  | ElfNetBSD
  | ElfLinux
  | ElfGNUHurd
  | ElfSolaris
  | ElfAIX
  | ElfIRIX
  | ElfFreeBSD
  | ElfTru64
  | ElfModesto
  | ElfOpenBSD
  | ElfOpenVMS
  | ElfNonStopKernel
  | ElfAROS
  | ElfFenixOS
  | ElfNuxiCloudABI
  deriving (Eq,Show)

data ElfType
  = ElfET_NONE
  | ElfET_REL
  | ElfET_EXEC
  | ElfET_DYN
  | ElfET_CORE
  | ElfET_LOOS
  | ElfET_HIOS
  | ElfET_LOPROC
  | ElfET_HIPROC
  deriving (Eq,Show)

data ElfMachine
  = ElfEM_NONE          --0 No machine
  |  ElfEM_M32          --1 AT&T WE 32100
  |  ElfEM_SPARC        --2 SPARC
  |  ElfEM_386          --3 Intel 80386
  |  ElfEM_68K          --4 Motorola 68000
  |  ElfEM_88K          --5 Motorola 88000
  |  ElfReserved        --6 Reserved for future use (was EM_486)
  |  ElfEM_860          --7 Intel 80860
  |  ElfEM_MIPS         --8 MIPS I Architecture
  |  ElfEM_S370         --9 IBM System/370 Processor
  |  ElfEM_MIPS_RS3_LE  --10  MIPS RS3000 Little-endian
    --reserved          --11-14 Reserved for future use 
  |  ElfEM_PARISC       --15  Hewlett-Packard PA-RISC
    --reserved          --16  Reserved for future use
  |  ElfEM_VPP500       --17  Fujitsu VPP500
  |  ElfEM_SPARC32PLUS  --18  Enhanced instruction set SPARC
  |  ElfEM_960          --19  Intel 80960
  |  ElfEM_PPC          --20  PowerPC
  |  ElfEM_PPC64        --21  64-bit PowerPC
  |  ElfEM_S390         --22  IBM System/390 Processor
    --reserved          --23-35 Reserved for future use
  |  ElfEM_V800         --36  NEC V800
  |  ElfEM_FR20         --37  Fujitsu FR20
  |  ElfEM_RH32         --38  TRW RH-32
  |  ElfEM_RCE          --39  Motorola RCE
  |  ElfEM_ARM          --40  Advanced RISC Machines ARM
  |  ElfEM_ALPHA        --41  Digital Alpha
  |  ElfEM_SH           --42  Hitachi SH
  |  ElfEM_SPARCV9      --43  SPARC Version 9
  |  ElfEM_TRICORE      --44  Siemens TriCore embedded processor
  |  ElfEM_ARC          --45  Argonaut RISC Core, Argonaut Technologies Inc.
  |  ElfEM_H8_300       --46  Hitachi H8/300
  |  ElfEM_H8_300H      --47  Hitachi H8/300H
  |  ElfEM_H8S          --48  Hitachi H8S
  |  ElfEM_H8_500       --49  Hitachi H8/500
  |  ElfEM_IA_64        --50  Intel IA-64 processor architecture
  |  ElfEM_MIPS_X       --51  Stanford MIPS-X
  |  ElfEM_COLDFIRE     --52  Motorola ColdFire
  |  ElfEM_68HC12       --53  Motorola M68HC12
  |  ElfEM_MMA          --54  Fujitsu MMA Multimedia Accelerator
  |  ElfEM_PCP          --55  Siemens PCP
  |  ElfEM_NCPU         --56  Sony nCPU embedded RISC processor
  |  ElfEM_NDR1         --57  Denso NDR1 microprocessor
  |  ElfEM_STARCORE     --58  Motorola Star*Core processor
  |  ElfEM_ME16         --59  Toyota ME16 processor
  |  ElfEM_ST100        --60  STMicroelectronics ST100 processor
  |  ElfEM_TINYJ        --61  Advanced Logic Corp. TinyJ embedded processor family
  |  ElfEM_X86_64       --62  AMD x86-64 architecture
  |  ElfEM_PDSP         --63  Sony DSP Processor
  |  ElfEM_PDP10        --64  Digital Equipment Corp. PDP-10
  |  ElfEM_PDP11        --65  Digital Equipment Corp. PDP-11
  |  ElfEM_FX66         --66  Siemens FX66 microcontroller
  |  ElfEM_ST9PLUS      --67  STMicroelectronics ST9+ 8/16 bit microcontroller
  |  ElfEM_ST7          --68  STMicroelectronics ST7 8-bit microcontroller
  |  ElfEM_68HC16       --69  Motorola MC68HC16 Microcontroller
  |  ElfEM_68HC11       --70  Motorola MC68HC11 Microcontroller
  |  ElfEM_68HC08       --71  Motorola MC68HC08 Microcontroller
  |  ElfEM_68HC05       --72  Motorola MC68HC05 Microcontroller
  |  ElfEM_SVX          --73  Silicon Graphics SVx
  |  ElfEM_ST19         --74  STMicroelectronics ST19 8-bit microcontroller
  |  ElfEM_VAX          --75  Digital VAX
  |  ElfEM_CRIS         --76  Axis Communications 32-bit embedded processor
  |  ElfEM_JAVELIN      --77  Infineon Technologies 32-bit embedded processor
  |  ElfEM_FIREPATH     --78  Element 14 64-bit DSP Processor
  |  ElfEM_ZSP          --79  LSI Logic 16-bit DSP Processor
  |  ElfEM_MMIX         --80  Donald Knuth's educational 64-bit processor
  |  ElfEM_HUANY        --81  Harvard University machine-independent object files
  |  ElfEM_PRISM        --82  SiTera Prism
  |  ElfEM_AVR          --83  Atmel AVR 8-bit microcontroller
  |  ElfEM_FR30         --84  Fujitsu FR30
  |  ElfEM_D10V         --85  Mitsubishi D10V
  |  ElfEM_D30V         --86  Mitsubishi D30V
  |  ElfEM_V850         --87  NEC v850
  |  ElfEM_M32R         --88  Mitsubishi M32R
  |  ElfEM_MN10300      --89  Matsushita MN10300
  |  ElfEM_MN10200      --90  Matsushita MN10200
  |  ElfEM_PJ           --91  picoJava
  |  ElfEM_OPENRISC     --92  OpenRISC 32-bit embedded processor
  |  ElfEM_ARC_A5       --93  ARC Cores Tangent-A5
  |  ElfEM_XTENSA       --94  Tensilica Xtensa Architecture
  |  ElfEM_VIDEOCORE    --95  Alphamosaic VideoCore processor
  |  ElfEM_TMM_GPP      --96  Thompson Multimedia General Purpose Processor
  |  ElfEM_NS32K        --97  National Semiconductor 32000 series
  |  ElfEM_TPC          --98  Tenor Network TPC processor
  |  ElfEM_SNP1K        --99  Trebia SNP 1000 processor
  |  ElfEM_ST200        --100 STMicroelectronics (www.st.com) ST200 microcontrolle
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
  | a == 0 = Right ElfSystemV
  | a == 1 = Right ElfHPUX
  | a == 2 = Right ElfNetBSD
  | a == 3 = Right ElfLinux
  | a == 4 = Right ElfGNUHurd
  | a == 6 = Right ElfSolaris
  | a == 7 = Right ElfAIX
  | a == 8 = Right ElfIRIX
  | a == 9 = Right ElfFreeBSD
  | a == 10 = Right ElfTru64
  | a == 11 = Right ElfModesto
  | a == 12 = Right ElfOpenBSD
  | a == 13 = Right ElfOpenVMS
  | a == 14 = Right ElfNonStopKernel
  | a == 15 = Right ElfAROS
  | a == 16 = Right ElfFenixOS
  | a == 17 = Right ElfNuxiCloudABI
  | otherwise = Left a

elfType' :: Word16 -> Either Word16 ElfType
elfType' a
  | a == 0 = Right ElfET_NONE
  | a == 1 = Right ElfET_REL
  | a == 2 = Right ElfET_EXEC
  | a == 3 = Right ElfET_DYN
  | a == 4 = Right ElfET_CORE
  | otherwise = Left a

elfMachine' :: Word16 -> Either Word16 ElfMachine
elfMachine' a
  | a == 0 = Right ElfEM_NONE
  | a == 1 = Right ElfEM_M32
  | a == 2 = Right ElfEM_SPARC
  | a == 3 = Right ElfEM_386
  | a == 4 = Right ElfEM_68K
  | a == 5 = Right ElfEM_88K
  | a == 6 = Right ElfReserved
  | a == 7 = Right ElfEM_860
  | a == 8 = Right ElfEM_MIPS
  | a == 9 = Right ElfEM_S370
  | a == 10 = Right ElfEM_MIPS_RS3_LE
  | a == 15 = Right ElfEM_PARISC
  | a == 17 = Right ElfEM_VPP500
  | a == 18 = Right ElfEM_SPARC32PLUS
  | a == 19 = Right ElfEM_960
  | a == 20 = Right ElfEM_PPC
  | a == 21 = Right ElfEM_PPC64
  | a == 22 = Right ElfEM_S390
  | a == 36 = Right ElfEM_V800
  | a == 37 = Right ElfEM_FR20
  | a == 38 = Right ElfEM_RH32
  | a == 39 = Right ElfEM_RCE
  | a == 40 = Right ElfEM_ARM
  | a == 41 = Right ElfEM_ALPHA
  | a == 42 = Right ElfEM_SH
  | a == 43 = Right ElfEM_SPARCV9
  | a == 44 = Right ElfEM_TRICORE
  | a == 45 = Right ElfEM_ARC
  | a == 46 = Right ElfEM_H8_300
  | a == 47 = Right ElfEM_H8_300H
  | a == 48 = Right ElfEM_H8S
  | a == 49 = Right ElfEM_H8_500
  | a == 50 = Right ElfEM_IA_64
  | a == 51 = Right ElfEM_MIPS_X
  | a == 52 = Right ElfEM_COLDFIRE
  | a == 53 = Right ElfEM_68HC12
  | a == 54 = Right ElfEM_MMA
  | a == 55 = Right ElfEM_PCP
  | a == 56 = Right ElfEM_NCPU
  | a == 57 = Right ElfEM_NDR1
  | a == 58 = Right ElfEM_STARCORE
  | a == 59 = Right ElfEM_ME16
  | a == 60 = Right ElfEM_ST100
  | a == 61 = Right ElfEM_TINYJ
  | a == 62 = Right ElfEM_X86_64
  | a == 63 = Right ElfEM_PDSP
  | a == 64 = Right ElfEM_PDP10
  | a == 65 = Right ElfEM_PDP11
  | a == 66 = Right ElfEM_FX66
  | a == 67 = Right ElfEM_ST9PLUS
  | a == 68 = Right ElfEM_ST7
  | a == 69 = Right ElfEM_68HC16
  | a == 70 = Right ElfEM_68HC11
  | a == 71 = Right ElfEM_68HC08
  | a == 72 = Right ElfEM_68HC05
  | a == 73 = Right ElfEM_SVX
  | a == 74 = Right ElfEM_ST19
  | a == 75 = Right ElfEM_VAX
  | a == 76 = Right ElfEM_CRIS
  | a == 77 = Right ElfEM_JAVELIN
  | a == 78 = Right ElfEM_FIREPATH
  | a == 79 = Right ElfEM_ZSP
  | a == 80 = Right ElfEM_MMIX
  | a == 81 = Right ElfEM_HUANY
  | a == 82 = Right ElfEM_PRISM
  | a == 83 = Right ElfEM_AVR
  | a == 84 = Right ElfEM_FR30
  | a == 85 = Right ElfEM_D10V
  | a == 86 = Right ElfEM_D30V
  | a == 87 = Right ElfEM_V850
  | a == 88 = Right ElfEM_M32R
  | a == 89 = Right ElfEM_MN10300
  | a == 90 = Right ElfEM_MN10200
  | a == 91 = Right ElfEM_PJ
  | a == 92 = Right ElfEM_OPENRISC
  | a == 93 = Right ElfEM_ARC_A5
  | a == 94 = Right ElfEM_XTENSA
  | a == 95 = Right ElfEM_VIDEOCORE
  | a == 96 = Right ElfEM_TMM_GPP
  | a == 97 = Right ElfEM_NS32K
  | a == 98 = Right ElfEM_TPC
  | a == 99 = Right ElfEM_SNP1K
  | a == 100 = Right ElfEM_ST200 
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

pprint :: ElfHeader -> String
pprint a = "ELF Header" ++ "\n"
           ++ "========" ++ "\n"
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

data PTy = PT_NULL | PT_LOAD | PT_DYNAMIC | PT_INTERP | PT_NOTE |
           PT_SHLIB | PT_PHDR | PT_TLS | PT_LOOS | PT_HIOS |
           PT_LOPROC | PT_HIPROC
  deriving Show

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
--  ElfPHeader <$> G.getWord32le <*> G.getWord32le <*> G.getWord64le
--             <*> G.getWord64le <*> G.getWord64le <*> G.getWord64le
--             <*> G.getWord64le <*> G.getWord64le

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
