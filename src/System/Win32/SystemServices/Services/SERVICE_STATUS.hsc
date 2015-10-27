module System.Win32.SystemServices.Services.SERVICE_STATUS where

import Import
import System.Win32.SystemServices.Services.SERVICE_ACCEPT
import System.Win32.SystemServices.Services.SERVICE_STATE
import System.Win32.SystemServices.Services.SERVICE_TYPE

-- | Contains status information for a service.
data SERVICE_STATUS = SERVICE_STATUS
    { serviceType             :: SERVICE_TYPE
    -- ^ The type of service. This binding only supports the WIN32_OWN_PROCESS
    -- type.
    , currentState            :: SERVICE_STATE
    -- ^ The current state of the service.
    , controlsAccepted        :: [SERVICE_ACCEPT]
    -- ^ See 'SERVICE_ACCEPT' for details on this field.
    , win32ExitCode           :: DWORD
    -- ^ The error code the service uses to report an error that occurs when
    --   it is starting or stopping. To return an error code specific to the
    --   service, the service must set this value to
    --   'eRROR_SERVICE_SPECIFIC_ERROR' to indicate that the
    --   'serviceSpecificExitCode' member contains the error code. The service
    --   should set this value to 'nO_ERROR' when it is running and on normal
    --   termination.
    , serviceSpecificExitCode :: DWORD
    -- ^ A service-specific error code that the service returns when an error
    -- occurs while the service is starting or stopping. This value is
    -- ignored unless the 'win32ExitCode' member is set to
    -- 'eRROR_SERVICE_SPECIFIC_ERROR'.
    --
    -- This binding does not support service-specific error codes.
    , checkPoint              :: DWORD
    -- ^ The check-point value the service increments periodically to report
    --   its progress during a lengthy start, stop, pause, or continue
    --   operation. For example, the service should increment this value as it
    --   completes each step of its initialization when it is starting up. The
    --   user interface program that invoked the operation on the service uses
    --   this value to track the progress of the service during a lengthy
    --   operation. This value is not valid and should be zero when the
    --   service does not have a start, stop, pause, or continue operation
    --   pending.
    , waitHint                :: DWORD
    -- ^ The estimated time required for a pending start, stop, pause, or
    --   continue operation, in milliseconds. Before the specified amount of
    --   time has elapsed, the service should make its next call to the
    --   SetServiceStatus function with either an incremented dwCheckPoint
    --   value or a change in 'currentState'. If the amount of time specified
    --   by 'waitHint' passes, and 'checkPoint' has not been incremented or
    --   'currentState' has not changed, the service control manager or
    --   service control program can assume that an error has occurred and the
    --   service should be stopped. However, if the service shares a process
    --   with other services, the service control manager cannot terminate the
    --   service application because it would have to terminate the other
    --   services sharing the process as well.
    } deriving (Show)

#include <Windows.h>

instance Storable SERVICE_STATUS where
  sizeOf _ = #{size SERVICE_STATUS}
  alignment _ = alignment (undefined :: DWORD)
  peek p = SERVICE_STATUS
    <$> #{peek SERVICE_STATUS, dwServiceType} p
    <*> #{peek SERVICE_STATUS, dwCurrentState} p
    <*> peekServiceAccept (#{ptr SERVICE_STATUS, dwControlsAccepted} p)
    <*> #{peek SERVICE_STATUS, dwWin32ExitCode} p
    <*> #{peek SERVICE_STATUS, dwServiceSpecificExitCode} p
    <*> #{peek SERVICE_STATUS, dwCheckPoint} p
    <*> #{peek SERVICE_STATUS, dwWaitHint} p
  poke p x = do
    #{poke SERVICE_STATUS, dwServiceType} p $ serviceType x
    #{poke SERVICE_STATUS, dwCurrentState} p $ currentState x
    pokeServiceAccept (#{ptr SERVICE_STATUS, dwControlsAccepted} p) $ controlsAccepted x
    #{poke SERVICE_STATUS, dwWin32ExitCode} p $ win32ExitCode x
    #{poke SERVICE_STATUS, dwServiceSpecificExitCode} p $ serviceSpecificExitCode x
    #{poke SERVICE_STATUS, dwCheckPoint} p $ checkPoint x
    #{poke SERVICE_STATUS, dwWaitHint} p $ waitHint x
