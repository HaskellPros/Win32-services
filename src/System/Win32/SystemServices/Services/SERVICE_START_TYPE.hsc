{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}
module System.Win32.SystemServices.Services.SERVICE_START_TYPE
    ( SERVICE_START_TYPE (..)
    , pattern SERVICE_AUTO_START
    , pattern SERVICE_BOOT_START
    , pattern SERVICE_DEMAND_START
    , pattern SERVICE_DISABLED
    , pattern SERVICE_SYSTEM_START
    ) where

import Import

newtype SERVICE_START_TYPE = SERVICE_START_TYPE { unServiceStartType :: DWORD }
  deriving (Eq, Bits, Storable, Show)

#include <Windows.h>

pattern SERVICE_AUTO_START   = SERVICE_START_TYPE #{const SERVICE_AUTO_START}
pattern SERVICE_BOOT_START   = SERVICE_START_TYPE #{const SERVICE_BOOT_START}
pattern SERVICE_DEMAND_START = SERVICE_START_TYPE #{const SERVICE_DEMAND_START}
pattern SERVICE_DISABLED     = SERVICE_START_TYPE #{const SERVICE_DISABLED}
pattern SERVICE_SYSTEM_START = SERVICE_START_TYPE #{const SERVICE_SYSTEM_START}
