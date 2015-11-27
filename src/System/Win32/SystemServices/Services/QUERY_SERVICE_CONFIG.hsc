{-# LANGUAGE ForeignFunctionInterface #-}
module System.Win32.SystemServices.Services.QUERY_SERVICE_CONFIG where

import Import

#include <Windows.h>

-- Contains configuration information for an installed service
data QUERY_SERVICE_CONFIG = QUERY_SERVICE_CONFIG
  { rawServiceType :: DWORD
  , rawStartType :: DWORD
  , rawErrorControl :: DWORD
  , rawBinaryPathName :: LPWSTR
  , rawLoadOrderGroup :: LPWSTR
  , rawTagId :: DWORD
  , rawDependencies :: LPWSTR
  , rawServiceStartName :: LPWSTR
  , rawDisplayName :: LPWSTR
  } deriving (Show)

type LPQUERY_SERVICE_CONFIG = Ptr QUERY_SERVICE_CONFIG

instance Storable QUERY_SERVICE_CONFIG where
  sizeOf _ = #{size QUERY_SERVICE_CONFIG}
  alignment _ = alignment (undefined :: DWORD)
  peek p = QUERY_SERVICE_CONFIG
    <$> #{peek QUERY_SERVICE_CONFIG, dwServiceType} p
    <*> #{peek QUERY_SERVICE_CONFIG, dwStartType} p
    <*> #{peek QUERY_SERVICE_CONFIG, dwErrorControl} p
    <*> #{peek QUERY_SERVICE_CONFIG, lpBinaryPathName} p
    <*> #{peek QUERY_SERVICE_CONFIG, lpLoadOrderGroup} p
    <*> #{peek QUERY_SERVICE_CONFIG, dwTagId} p
    <*> #{peek QUERY_SERVICE_CONFIG, lpDependencies} p
    <*> #{peek QUERY_SERVICE_CONFIG, lpServiceStartName} p
    <*> #{peek QUERY_SERVICE_CONFIG, lpDisplayName} p
  poke p x = do
    #{poke QUERY_SERVICE_CONFIG, dwServiceType}       p $ rawServiceType x
    #{poke QUERY_SERVICE_CONFIG, dwStartType}         p $ rawStartType x
    #{poke QUERY_SERVICE_CONFIG, dwErrorControl}      p $ rawErrorControl x
    #{poke QUERY_SERVICE_CONFIG, lpBinaryPathName}    p $ rawBinaryPathName x
    #{poke QUERY_SERVICE_CONFIG, lpLoadOrderGroup}    p $ rawLoadOrderGroup x
    #{poke QUERY_SERVICE_CONFIG, dwTagId}             p $ rawTagId x
    #{poke QUERY_SERVICE_CONFIG, lpDependencies}      p $ rawDependencies x
    #{poke QUERY_SERVICE_CONFIG, lpServiceStartName}  p $ rawServiceStartName x
    #{poke QUERY_SERVICE_CONFIG, lpDisplayName}       p $ rawDisplayName x

--BOOL WINAPI QueryServiceConfig(
--  _In_      SC_HANDLE              hService,
--  _Out_opt_ LPQUERY_SERVICE_CONFIG lpServiceConfig,
--  _In_      DWORD                  cbBufSize,
--  _Out_     LPDWORD                pcbBytesNeeded
--);
foreign import stdcall "windows.h QueryServiceConfigW"
    c_QueryServiceConfig :: HANDLE -> LPQUERY_SERVICE_CONFIG -> DWORD -> LPDWORD -> IO BOOL
