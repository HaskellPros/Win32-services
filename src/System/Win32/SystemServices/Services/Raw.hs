{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.SystemServices.Services.Raw where

import Foreign.Ptr (Ptr)
import System.Win32.Types

import System.Win32.SystemServices.Services.SERVICE_STATUS
import System.Win32.SystemServices.Services.SERVICE_TABLE_ENTRY
import System.Win32.SystemServices.Services.Types

foreign import stdcall "wrapper"
    smfToFunPtr :: SERVICE_MAIN_FUNCTION -> IO LPSERVICE_MAIN_FUNCTION

foreign import stdcall "wrapper"
    handlerToFunPtr :: HANDLER_FUNCTION_EX -> IO LPHANDLER_FUNCTION_EX

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
