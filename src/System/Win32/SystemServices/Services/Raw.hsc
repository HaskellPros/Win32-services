{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.SystemServices.Services.Raw where

import Import
import System.Win32.SystemServices.Services.SERVICE_STATUS
import System.Win32.SystemServices.Services.SERVICE_TABLE_ENTRY
import System.Win32.SystemServices.Services.Types

#include <windows.h>

foreign import stdcall "wrapper"
    smfToFunPtr :: SERVICE_MAIN_FUNCTION -> IO LPSERVICE_MAIN_FUNCTION

foreign import stdcall "wrapper"
    handlerToFunPtr :: HANDLER_FUNCTION_EX -> IO LPHANDLER_FUNCTION_EX

-- BOOL WINAPI ChangeServiceConfig(
--   _In_      SC_HANDLE hService,
--   _In_      DWORD     dwServiceType,
--   _In_      DWORD     dwStartType,
--   _In_      DWORD     dwErrorControl,
--   _In_opt_  LPCTSTR   lpBinaryPathName,
--   _In_opt_  LPCTSTR   lpLoadOrderGroup,
--   _Out_opt_ LPDWORD   lpdwTagId,
--   _In_opt_  LPCTSTR   lpDependencies,
--   _In_opt_  LPCTSTR   lpServiceStartName,
--   _In_opt_  LPCTSTR   lpPassword,
--   _In_opt_  LPCTSTR   lpDisplayName
-- );
foreign import stdcall "windows.h ChangeServiceConfigW"
  c_ChangeServiceConfig :: HANDLE -> DWORD -> DWORD -> DWORD -> LPCWSTR -> LPCWSTR -> LPDWORD -> LPCWSTR -> LPCWSTR -> LPCWSTR -> LPCWSTR -> IO BOOL

-- BOOL WINAPI CloseServiceHandle(
--   _In_  SC_HANDLE hSCObject
-- );
foreign import stdcall "windows.h CloseServiceHandle"
  c_CloseServiceHandle :: HANDLE -> IO BOOL

-- BOOL WINAPI ControlService(
--   _In_   SC_HANDLE hService,
--   _In_   DWORD dwControl,
--   _Out_  LPSERVICE_STATUS lpServiceStatus
-- );
foreign import stdcall "windows.h ControlService"
  c_ControlService :: HANDLE -> DWORD -> Ptr SERVICE_STATUS -> IO BOOL

-- SC_HANDLE WINAPI OpenSCManager(
--   _In_opt_  LPCTSTR lpMachineName,
--   _In_opt_  LPCTSTR lpDatabaseName,
--   _In_      DWORD dwDesiredAccess
-- );
foreign import stdcall "windows.h OpenSCManagerW"
  c_OpenSCManager :: LPCWSTR -> LPCWSTR -> DWORD -> IO HANDLE

-- SC_HANDLE WINAPI OpenService(
--   _In_  SC_HANDLE hSCManager,
--   _In_  LPCTSTR lpServiceName,
--   _In_  DWORD dwDesiredAccess
-- );
foreign import stdcall "windows.h OpenServiceW"
  c_OpenService :: HANDLE -> LPTSTR -> DWORD -> IO HANDLE

-- BOOL WINAPI QueryServiceStatus(
--   _In_   SC_HANDLE hService,
--   _Out_  LPSERVICE_STATUS lpServiceStatus
-- );
foreign import stdcall "windows.h QueryServiceStatus"
    c_QueryServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

-- I've not been able to get RegisterServiceCtrlHandler to work on Windows 7 64-bit.
foreign import stdcall "windows.h RegisterServiceCtrlHandlerExW"
    c_RegisterServiceCtrlHandlerEx :: LPTSTR -> LPHANDLER_FUNCTION_EX  -> Ptr () -> IO HANDLE

foreign import stdcall "windows.h SetServiceStatus"
    c_SetServiceStatus :: HANDLE -> Ptr SERVICE_STATUS -> IO BOOL

-- BOOL WINAPI StartService(
--   _In_      SC_HANDLE hService,
--   _In_      DWORD dwNumServiceArgs,
--   _In_opt_  LPCTSTR *lpServiceArgVectors
-- );
foreign import stdcall "windows.h StartServiceW"
  c_StartService :: HANDLE -> DWORD -> LPTSTR -> IO BOOL

foreign import stdcall "windows.h StartServiceCtrlDispatcherW"
    c_StartServiceCtrlDispatcher :: Ptr SERVICE_TABLE_ENTRY -> IO BOOL

data RawEnumServiceStatus = RawEnumServiceStatus
  { rawESSServiceName   :: LPWSTR
  , rawESSDisplayName   :: LPWSTR
  , rawESSServiceStatus :: SERVICE_STATUS
  } deriving (Show)

instance Storable RawEnumServiceStatus where
  sizeOf _ = #{size ENUM_SERVICE_STATUS}
  alignment _ = alignment (undefined :: DWORD)
  poke p x = do
    #{poke ENUM_SERVICE_STATUS, lpServiceName} p $ rawESSServiceName x
    #{poke ENUM_SERVICE_STATUS, lpDisplayName} p $ rawESSDisplayName x
    #{poke ENUM_SERVICE_STATUS, ServiceStatus} p $ rawESSServiceStatus x
  peek p = RawEnumServiceStatus
    <$> #{peek ENUM_SERVICE_STATUS, lpServiceName} p
    <*> #{peek ENUM_SERVICE_STATUS, lpDisplayName} p
    <*> #{peek ENUM_SERVICE_STATUS, ServiceStatus} p

data EnumServiceState
  = EnumServiceActive
  | EnumServiceInactive
  | EnumServiceAll
  deriving (Eq, Show)

enumServiceStateToDWORD :: EnumServiceState -> DWORD
enumServiceStateToDWORD x = case x of
  EnumServiceActive -> #{const SERVICE_ACTIVE}
  EnumServiceInactive -> #{const SERVICE_INACTIVE}
  EnumServiceAll -> #{const SERVICE_STATE_ALL}

-- BOOL WINAPI EnumDependentServices(
--   _In_      SC_HANDLE             hService,
--   _In_      DWORD                 dwServiceState,
--   _Out_opt_ LPENUM_SERVICE_STATUS lpServices,
--   _In_      DWORD                 cbBufSize,
--   _Out_     LPDWORD               pcbBytesNeeded,
--   _Out_     LPDWORD               lpServicesReturned
-- );
foreign import stdcall "windows.h EnumDependentServicesW"
    c_EnumDependentServices :: HANDLE -> DWORD -> Ptr RawEnumServiceStatus
      -> DWORD -> Ptr DWORD -> Ptr DWORD -> IO BOOL

eRROR_MORE_DATA :: DWORD
eRROR_MORE_DATA = #{const ERROR_MORE_DATA}
