{-# LANGUAGE PatternSynonyms #-}
module System.Win32.SystemServices.Services
    ( HandlerFunction
    , ServiceMainFunction
    , SCM_ACCESS_RIGHTS (..)
    , SVC_ACCESS_RIGHTS (..)
    , sarFromDword
    , sarToDword
    , SERVICE_ACCEPT (..)
    , SERVICE_CONTROL (..)
    , SERVICE_ERROR (..)
    , SERVICE_STATE (..)
    , pattern SERVICE_CONTINUE_PENDING
    , pattern SERVICE_PAUSE_PENDING
    , pattern SERVICE_PAUSED
    , pattern SERVICE_RUNNING
    , pattern SERVICE_START_PENDING
    , pattern SERVICE_STOP_PENDING
    , pattern SERVICE_STOPPED
    , SERVICE_STATUS (..)
    , SERVICE_TYPE (..)
    , SERVICE_START_TYPE(..)
    , pattern SERVICE_FILE_SYSTEM_DRIVER
    , pattern SERVICE_KERNEL_DRIVER
    , pattern SERVICE_WIN32_OWN_PROCESS
    , pattern SERVICE_WIN32_SHARE_PROCESS
    , pattern SERVICE_INTERACTIVE_PROCESS
    , pattern SERVICE_AUTO_START
    , pattern SERVICE_BOOT_START
    , pattern SERVICE_DEMAND_START
    , pattern SERVICE_DISABLED
    , pattern SERVICE_SYSTEM_START
    , SERVICE_ERROR_CONTROL (..)
    , pattern SERVICE_ERROR_CRITICAL
    , pattern SERVICE_ERROR_IGNORE
    , pattern SERVICE_ERROR_NORMAL
    , pattern SERVICE_ERROR_SEVERE
    , FromDWORD (..)
    , EnumServiceState (..)
    , EnumServiceStatus (..)
    , SERVICE_CONFIG (..)
    , ServiceConfig (..)
    , defServiceConfig
    , CreateServiceOpts (..)
    , defCreateServiceOpts
    , nO_ERROR
    , eRROR_SERVICE_SPECIFIC_ERROR
    , createService
    , createService'
    , changeServiceConfig
    , changeServiceConfig'
    , changeServiceConfigDependencies
    , changeServiceConfigStartType
    , queryServiceConfig
    , closeServiceHandle
    , controlService
    , openSCManagerDef
    , openService
    , openService'
    , queryServiceStatus
    , setServiceStatus
    , startServiceWithoutArgs
    , startServiceCtrlDispatcher
    , withHandle
    , enumDependentServices
    ) where

import Control.Exception
import Control.Monad (unless, forM_)
import Control.Monad.Fix
import Data.IORef
import Foreign.C.Types  (CWchar)
import Foreign.C.String (CWString, peekCWString, withCWString, withCWStringLen)

import Import
import System.Win32.SystemServices.Services.Raw
import System.Win32.SystemServices.Services.SCM_ACCESS_RIGHTS as AR
import System.Win32.SystemServices.Services.SVC_ACCESS_RIGHTS as SV
import System.Win32.SystemServices.Services.SERVICE_ACCEPT
import System.Win32.SystemServices.Services.SERVICE_CONTROL
import System.Win32.SystemServices.Services.SERVICE_ERROR
import System.Win32.SystemServices.Services.SERVICE_ERROR_CONTROL
import qualified System.Win32.SystemServices.Services.SERVICE_CONTROL as SC
import System.Win32.SystemServices.Services.SERVICE_STATE
import System.Win32.SystemServices.Services.SERVICE_STATUS
import System.Win32.SystemServices.Services.SERVICE_TABLE_ENTRY
import System.Win32.SystemServices.Services.SERVICE_TYPE
import qualified System.Win32.SystemServices.Services.SERVICE_OPTIONAL as SO
import System.Win32.SystemServices.Services.QUERY_SERVICE_CONFIG
import System.Win32.SystemServices.Services.SERVICE_START_TYPE
import System.Win32.SystemServices.Services.Types

-- | A handler function is registered with the service dispatcher thread
--   from a 'ServiceMainFunction'. The first argument is a 'HANDLE' returned
--   from calling 'registerServiceCtrlHandler'. The second argument represents
--   the command this service has been directed to perform.
type HandlerFunction = HANDLE -> SERVICE_CONTROL -> IO Bool

-- | The service dispatcher thread will call each function of this type that
--   you provide. The first argument will be the name of the service. Any
--   additional command-line parameters will appear in the second argument.
--
--   Each of these functions should call 'registerServiceCtrlHandler' to
--   register a function to handle incoming commands. It should then set
--   the service's status to 'START_PENDING', and specify that no controls
--   will be accepted. At this point the function may perform any other
--   initialization steps before setting the service's status to
--   'RUNNING'. All of this should take no more than 100ms.
type ServiceMainFunction = String -> [String] -> HANDLE -> IO ()

withCWString' :: Maybe String -> (CWString -> IO a) -> IO a
withCWString' (Just s) f = withCWString s f
withCWString' Nothing  f = f nullPtr

withCWStringList' :: Maybe [String] -> (CWString -> IO a) -> IO a
withCWStringList' (Just ss) f = withCWStringList ss f
withCWStringList' Nothing   f = f nullPtr

wNUL :: CWchar
wNUL = 0

withCWStringList :: [String] -> (CWString -> IO a) -> IO a
withCWStringList ss fun =
  let
    wsize = sizeOf (undefined :: CWchar)
    size' = if (length ss) == 0
      then 0
      else foldl (+) 0 $ map (\s -> length s + 1) ss

    size = (size' + 1) * wsize
  in
    allocaBytes size $ \ptr ->
      do
        pRef <- newIORef ptr
        forM_ ss $ \s ->
          withCWStringLen s $ \(p', l') -> do
            let l = l' * wsize
            p <- readIORef pRef

            copyBytes p p' l
            let nulPos = p `plusPtr` l

            poke nulPos wNUL
            writeIORef pRef $ nulPos `plusPtr` wsize

        p <- readIORef pRef
        poke p wNUL
        fun ptr

peekCWStringList :: LPCWSTR -> IO [String]
peekCWStringList pStr = do
  str <- peekCWString pStr
  let
    wsize = sizeOf (undefined :: CWchar)
    len = length str * wsize

  if len == 0
    then return []
    else do
      strs <- peekCWStringList $ pStr `plusPtr` (len + wsize)
      return $ str:strs
-- |Creates a service object and adds it to the specified service control manager database.
createService :: HANDLE
    -- ^ MSDN documentation: A handle to the service control manager database.
    -- This handle is returned by the OpenSCManager function and must have
    -- the SC_MANAGER_CREATE_SERVICE access right.
    -> String
    -- ^ MSDN documentation: The name of the service to install.
    -- The maximum string length is 256 characters.
    -> Maybe String
    -- ^ MSDN documentation: The display name to be used by user interface
    -- programs to identify the service. This string has a maximum length
    -- of 256 characters. The name is case-preserved in the service control
    -- manager. Display name comparisons are always case-insensitive.
    -> DWORD
    -- ^ MSDN documentation: The access to the service. Before granting
    -- the requested access, the system checks the access token of
    -- the calling process.
    -> DWORD
    -- ^ MSDN documentation: The service type.
    -> DWORD
    -- ^ MSDN documentation: The service start options.
    -> DWORD
    -- ^ MSDN documentation: The severity of the error, and action taken,
    -- if this service fails to start.
    -> Maybe String
    -- ^ MSDN documentation: The fully qualified path to the service binary
    -- file. If the path contains a space, it must be quoted so that it is
    -- correctly interpreted.
    -> Maybe String
    -- ^ MSDN documentation: he names of the load ordering group of which
    -- this service is a member.
    -> LPDWORD
    -- ^ MSDN documentation: A pointer to a variable that receives
    -- a tag value that is unique in the group specified in
    -- the lpLoadOrderGroup parameter.
    -> Maybe [String]
    -- ^ List of services or load ordering groups that the system must
    -- start before this service. Dependency on a group means that this
    -- service can run if at least one member of the group is running after
    -- an attempt to start all members of the group.
    -> Maybe String
    -- ^ MSDN documentation: The name of the account under which the service
    -- should run. If the service type is SERVICE_WIN32_OWN_PROCESS, use
    -- an account name in the form DomainName\UserName. The service process
    -- will be logged on as this user. If the account belongs to
    -- the built-in domain, you can specify .\UserName.
    -> Maybe String
    -- ^ MSDN documentation: The password to the account name specified by
    -- the lpServiceStartName parameter. Specify an empty string if
    -- the account has no password or if the service runs in the LocalService,
    -- NetworkService, or LocalSystem account.
    -> IO HANDLE
    -- ^ If the function succeeds, the return value is a handle to the service.
    -- This function will raise an exception if the Win32 call returned an
    -- error condition.
createService h name' displayName' ar svcType startType errCtl binPath' loadOrderGrp' tagId deps' startName' pass' =
  withCWString name' $ \name ->
  withCWString' displayName' $ \displayName ->
  withCWString' binPath' $ \binPath ->
  withCWString' loadOrderGrp' $ \loadOrderGrp ->
  withCWStringList' deps' $ \deps ->
  withCWString' startName' $ \startName ->
  withCWString' pass' $ \pass ->
    failIfNull (unwords ["CreateService", name']) $
      c_CreateService h name displayName ar svcType startType errCtl binPath loadOrderGrp tagId deps startName pass

data CreateServiceOpts = CreateServiceOpts
  { createSvcDisplayName    :: Maybe String
  , createSvcDesiredAccess  :: SVC_ACCESS_RIGHTS
  , createSvcServiceType    :: SERVICE_TYPE
  , createSvcStartType      :: SERVICE_START_TYPE
  , createSvcErrorControl   :: SERVICE_ERROR_CONTROL
  , createSvcBinaryPath     :: Maybe String
  , createSvcLoadOrderGroup :: Maybe String
  , createSvcDependencies   :: Maybe [String]
  , createSvcStartName      :: Maybe String
  , createSvcPassword       :: Maybe String
  }

defCreateServiceOpts :: CreateServiceOpts
defCreateServiceOpts = CreateServiceOpts
  { createSvcDisplayName = Nothing
  , createSvcDesiredAccess = SVC_AR_ALL_ACCESS
  , createSvcServiceType = SERVICE_WIN32_OWN_PROCESS
  , createSvcStartType = SERVICE_AUTO_START
  , createSvcErrorControl = SERVICE_ERROR_NORMAL
  , createSvcBinaryPath = Nothing
  , createSvcLoadOrderGroup = Nothing
  , createSvcDependencies = Nothing
  , createSvcStartName = Nothing
  , createSvcPassword = Nothing
  }

createService' :: HANDLE -> String -> CreateServiceOpts -> IO HANDLE
createService' h name o =
  createService h name
    (createSvcDisplayName o)
    (SV.toDWORD $ createSvcDesiredAccess o)
    (unServiceType $ createSvcServiceType o)
    (unServiceStartType $ createSvcStartType o)
    (unServiceErrorControl $ createSvcErrorControl o)
    (createSvcBinaryPath o)
    (createSvcLoadOrderGroup o)
    nullPtr
    (createSvcDependencies o)
    (createSvcStartName o)
    (createSvcPassword o)

data ServiceConfig = ServiceConfig
  { cscServiceType      :: Maybe SERVICE_TYPE
  , cscStartType        :: Maybe SERVICE_START_TYPE
  , cscErrorControl     :: Maybe SERVICE_ERROR_CONTROL
  , cscBinaryPathName   :: Maybe String
  , cscLoadOrderGroup   :: Maybe String
  , cscDependencies     :: Maybe [String]
  , cscServiceStartName :: Maybe String
  , cscPassword         :: Maybe String
  , cscDisplayName      :: Maybe String
  }

defServiceConfig :: ServiceConfig
defServiceConfig = ServiceConfig
  { cscServiceType = Nothing
  , cscStartType = Nothing
  , cscErrorControl = Nothing
  , cscBinaryPathName = Nothing
  , cscLoadOrderGroup = Nothing
  , cscDependencies = Nothing
  , cscServiceStartName = Nothing
  , cscPassword = Nothing
  , cscDisplayName = Nothing
  }

changeServiceConfig :: HANDLE -> DWORD -> DWORD -> DWORD -> Maybe String -> Maybe String -> LPDWORD -> Maybe [String] -> Maybe String -> Maybe String -> Maybe String -> IO ()
changeServiceConfig h svcType startType errCtl path' loadOrderGrp' tag srvDeps' startName' pass' displayname' =
  withCWString' path' $ \path ->
    withCWString' loadOrderGrp' $ \loadOrderGrp ->
      withCWStringList' srvDeps' $ \srvDeps ->
        withCWString' startName' $ \startName ->
          withCWString' pass' $ \pass ->
            withCWString' displayname' $ \displayname ->
              failIfFalse_ (unwords ["changeServiceConfig"]) $
                c_ChangeServiceConfig h svcType startType errCtl path loadOrderGrp tag srvDeps startName pass displayname

changeServiceConfig' :: HANDLE -> ServiceConfig -> IO ()
changeServiceConfig' h c =
  let snc = SO.toDWORD SO.SERVICE_NO_CHANGE
  in  changeServiceConfig h
        (maybe snc unServiceType $ cscServiceType c)
        (maybe snc unServiceStartType $ cscStartType c)
        (maybe snc unServiceErrorControl $ cscErrorControl c)
        (cscBinaryPathName c)
        (cscLoadOrderGroup c)
        nullPtr
        (cscDependencies c)
        (cscServiceStartName c)
        (cscPassword c)
        (cscDisplayName c)

changeServiceConfigDependencies :: HANDLE -> [String] -> IO ()
changeServiceConfigDependencies h dependsOnSvcs =
  let snc = SO.toDWORD SO.SERVICE_NO_CHANGE
  in changeServiceConfig h snc snc snc Nothing Nothing nullPtr (Just dependsOnSvcs) Nothing Nothing Nothing

changeServiceConfigStartType :: HANDLE -> DWORD -> IO ()
changeServiceConfigStartType h startType =
  let snc = SO.toDWORD SO.SERVICE_NO_CHANGE in
  changeServiceConfig h snc startType snc Nothing Nothing nullPtr Nothing Nothing Nothing Nothing

data SERVICE_CONFIG = SERVICE_CONFIG
  { scServiceType       :: Int
  , scStartType         :: Int
  , scErrorControl      :: Int
  , scBinaryPathName    :: String
  , scLoadOrderGroup    :: String
  , scTagId             :: Int
  , scDependencies      :: [String]
  , scServiceStartName  :: String
  , scDisplayName       :: String
  } deriving (Show)

fromRawServiceConfig :: QUERY_SERVICE_CONFIG -> IO SERVICE_CONFIG
fromRawServiceConfig config = do
  bpn <- peekCWString     $ rawBinaryPathName config
  ogr <- peekCWString     $ rawLoadOrderGroup config
  dep <- peekCWStringList $ rawDependencies config
  ssn <- peekCWString     $ rawServiceStartName config
  sdn <- peekCWString     $ rawDisplayName config
  return SERVICE_CONFIG
    { scServiceType      = fromIntegral $ rawServiceType  config
    , scStartType        = fromIntegral $ rawStartType    config
    , scErrorControl     = fromIntegral $ rawErrorControl config
    , scBinaryPathName   = bpn
    , scLoadOrderGroup   = ogr
    , scTagId            = fromIntegral $ rawTagId config
    , scDependencies     = dep
    , scServiceStartName = ssn
    , scDisplayName      = sdn
    }

failIfTrue_ :: String -> IO Bool -> IO ()
failIfTrue_ s b = do
  b' <- b
  failIfFalse_ s $ return $ not b'

queryServiceConfig :: HANDLE -> IO SERVICE_CONFIG
queryServiceConfig h =
  alloca $ \pBufSize -> do
    failIfTrue_ (unwords ["queryServiceConfig", "get buffer size"]) $
      c_QueryServiceConfig h nullPtr 0 pBufSize
    bufSize <- peek pBufSize
    allocaBytes (fromIntegral bufSize) $ \pConfig -> do
      failIfFalse_ (unwords ["queryServiceConfig", "get actual config", "buf size is", show bufSize]) $
        c_QueryServiceConfig h pConfig bufSize pBufSize
      config <- peek pConfig
      fromRawServiceConfig config

closeServiceHandle :: HANDLE -> IO ()
closeServiceHandle =
    failIfFalse_ (unwords ["CloseServiceHandle"]) . c_CloseServiceHandle

controlService :: HANDLE -> SERVICE_CONTROL -> IO SERVICE_STATUS
controlService h c = alloca $ \pStatus -> do
    failIfFalse_ (unwords ["ControlService"])
        $ c_ControlService h (SC.toDWORD c) pStatus
    peek pStatus

openSCManagerDef :: SCM_ACCESS_RIGHTS -> IO HANDLE
openSCManagerDef ar =
    failIfNull (unwords ["OpenSCManager"])
        $ c_OpenSCManager nullPtr nullPtr (AR.toDWORD ar)

_openService :: HANDLE -> String -> DWORD -> IO HANDLE
_openService h n ar =
    withTString n $ \lpcwstr -> failIfNull (unwords ["OpenService", n])
        $ c_OpenService h lpcwstr ar

-- |Opens an existing service.
openService :: HANDLE
    -- ^ MSDN documentation: A handle to the service control manager
    -- database. The OpenSCManager function returns this handle.
    -> String
    -- ^ MSDN documentation: The name of the service to be opened. This is
    -- the name specified by the lpServiceName parameter of the CreateService
    -- function when the service object was created, not the service display
    -- name that is shown by user interface applications to identify the service.
    -> SVC_ACCESS_RIGHTS
    -- ^ The list of access rights for a service.
    -> IO HANDLE
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.openService h n ar =
openService h n ar = _openService h n (SV.toDWORD ar)

-- |Opens an existing service with list of access rights.
openService' :: HANDLE
    -- ^ MSDN documentation: A handle to the service control manager
    -- database. The OpenSCManager function returns this handle.
    -> String
    -- ^ MSDN documentation: The name of the service to be opened. This is
    -- the name specified by the lpServiceName parameter of the CreateService
    -- function when the service object was created, not the service display
    -- name that is shown by user interface applications to identify the service.
    -> [SVC_ACCESS_RIGHTS]
    -- ^ The list of access rights for a service.
    -> IO HANDLE
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.
openService' h n ars =
    _openService h n (SV.flag ars)

-- |Retrieves the current status of the specified service.
queryServiceStatus :: HANDLE
    -- ^ MSDN documentation: A handle to the service. This handle is returned
    -- by the OpenService or the CreateService function, and it must have the
    -- SERVICE_QUERY_STATUS access right. For more information, see Service
    -- Security and Access Rights.
    -> IO SERVICE_STATUS
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.
queryServiceStatus h = alloca $ \pStatus -> do
    failIfFalse_ (unwords ["QueryServiceStatus"])
        $ c_QueryServiceStatus h pStatus
    peek pStatus

-- | Register an handler function to be called whenever the operating system
-- receives service control messages.
registerServiceCtrlHandlerEx :: String
    -- ^ The name of the service. According to MSDN documentation this
    -- argument is unused in WIN32_OWN_PROCESS type services, which is the
    -- only type supported by this binding. Even so, it is recommended
    -- that the name of the service be used.
    --
    -- MSDN description: The name of the service run by the calling thread.
    -- This is the service name that the service control program specified in
    -- the CreateService function when creating the service.
    -> HandlerFunction
    -- ^ A Handler function to be called in response to service control
    -- messages. Behind the scenes this is translated into a "HandlerEx" type
    -- handler.
    -> IO (HANDLE, LPHANDLER_FUNCTION_EX)
    -- ^ The returned handle may be used in calls to SetServiceStatus. For
    -- convenience Handler functions also receive a handle for the service.
registerServiceCtrlHandlerEx str handler =
    withTString str $ \lptstr ->
    -- use 'ret' instead of (h', _) to avoid divergence.
    mfix $ \ret -> do
    fpHandler <- handlerToFunPtr $ toHandlerEx (fst ret) handler
    h <- failIfNull (unwords ["RegisterServiceCtrlHandlerEx", str])
        $ c_RegisterServiceCtrlHandlerEx lptstr fpHandler nullPtr
    return (h, fpHandler)

-- |Updates the service control manager's status information for the calling
-- service.
setServiceStatus :: HANDLE
    -- ^ MSDN documentation: A handle to the status information structure for
    -- the current service. This handle is returned by the
    -- RegisterServiceCtrlHandlerEx function.
    -> SERVICE_STATUS
    -- ^ MSDN documentation: A pointer to the SERVICE_STATUS structure the
    -- contains the latest status information for the calling service.
    -> IO ()
    -- ^ This function will raise an exception if the Win32 call returned an
    -- error condition.
setServiceStatus h status =
    with status $ \pStatus -> do
    failIfFalse_ (unwords ["SetServiceStatus", show h, show status])
        $ c_SetServiceStatus h pStatus

-- |Starts a service.
startServiceWithoutArgs :: HANDLE -> IO ()
    -- ^ MSDN documentation: A handle to the service. This handle is returned
    -- by the OpenService or CreateService function, and it must have the
    -- SERVICE_START access right.
startServiceWithoutArgs h =
    failIfFalse_ (unwords ["StartService"])
        $ c_StartService h 0 nullPtr

-- |Register a callback function to initialize the service, which will be
-- called by the operating system immediately. startServiceCtrlDispatcher
-- will block until the provided callback function returns.
--
-- MSDN documentation: Connects the main thread of a service process to the
-- service control manager, which causes the thread to be the service control
-- dispatcher thread for the calling process.
startServiceCtrlDispatcher :: String
    -- ^ The name of the service. According to MSDN documentation this
    -- argument is unused in WIN32_OWN_PROCESS type services, which is the
    -- only type supported by this binding. Even so, it is recommended
    -- that the name of the service be used.
    --
    -- MSDN description: The name of the service run by the calling thread.
    -- This is the service name that the service control program specified in
    -- the CreateService function when creating the service.
    -> DWORD
    -- ^
    -- [@waitHint@] The estimated time required for a pending start, stop,
    -- pause, or continue operation, in milliseconds.
    -> HandlerFunction
    -> ServiceMainFunction
    -- ^ This is a callback function that will be called by the operating
    -- system whenever the service is started. It should perform service
    -- initialization including the registration of a handler function.
    -- MSDN documentation gives conflicting advice as to whether this function
    -- should return before the service has entered the stopped state.
    -- In the official example the service main function blocks until the
    -- service is ready to stop.
    -> IO ()
    -- ^ An exception will be raised if the underlying Win32 call returns an
    -- error condition.
startServiceCtrlDispatcher name wh handler main =
    withTString name $ \lptstr ->
    bracket (toSMF main handler wh >>= smfToFunPtr) freeHaskellFunPtr $ \fpMain ->
    withArray [SERVICE_TABLE_ENTRY lptstr fpMain, nullSTE] $ \pSTE ->
    failIfFalse_ (unwords ["StartServiceCtrlDispatcher", name]) $ do
    c_StartServiceCtrlDispatcher pSTE

toSMF :: ServiceMainFunction -> HandlerFunction -> DWORD -> IO SERVICE_MAIN_FUNCTION
toSMF f handler wh = return $ \len pLPTSTR -> do
    lptstrx <- peekArray (fromIntegral len) pLPTSTR
    args <- mapM peekTString lptstrx
    -- MSDN guarantees args will have at least 1 member.
    let name = head args
    (h, fpHandler) <- registerServiceCtrlHandlerEx name handler
    setServiceStatus h $ SERVICE_STATUS SERVICE_WIN32_OWN_PROCESS SERVICE_START_PENDING [] nO_ERROR 0 0 wh
    f name (tail args) h
    freeHaskellFunPtr fpHandler

-- This was originally written with older style handle functions in mind.
-- I'm now using HandlerEx style functions, and need to add support for
-- the extra parameters here.
toHandlerEx :: HANDLE -> HandlerFunction -> HANDLER_FUNCTION_EX
toHandlerEx h f = \dwControl _ _ _ ->
    case SC.fromDWORD dwControl of
      Right control -> do
          handled <- f h control
          case control of
            INTERROGATE -> return nO_ERROR
            -- If we ever support extended control codes this will have to
            -- change. see "Dev Center - Desktop > Docs > Desktop app
            -- development documentation > System Services > Services >
            -- Service Reference > Service Functions > HandlerEx".
            _ -> return $ if handled then nO_ERROR
                                     else eRROR_CALL_NOT_IMPLEMENTED
      Left _ -> return eRROR_CALL_NOT_IMPLEMENTED

withHandle :: IO HANDLE -> (HANDLE -> IO a) -> IO a
withHandle before = bracket before closeServiceHandle

data EnumServiceStatus = EnumServiceStatus
  { enumServiceName   :: String
  , enumDisplayName   :: String
  , enumServiceStatus :: SERVICE_STATUS
  }

enumDependentServices :: HANDLE -> EnumServiceState -> IO [EnumServiceStatus]
enumDependentServices hService ess =
    alloca $ \pcbBytesNeeded ->
    alloca $ \lpServicesReturned -> do
      res <- c_EnumDependentServices hService (enumServiceStateToDWORD ess)
        nullPtr 0 pcbBytesNeeded lpServicesReturned
      if res
        then return [] -- The only case when call without a buffer succeeds is when no buffer is needed.
        else do
          lastErr <- getLastError
          unless (lastErr == eRROR_MORE_DATA) $
            failWith "EnumDependentServices" lastErr
          bytesNeeded <- peek pcbBytesNeeded
          allocaBytes (fromIntegral bytesNeeded) $ \lpServices -> do
            failIfFalse_ "EnumDependentServices" $ c_EnumDependentServices hService (enumServiceStateToDWORD ess)
              lpServices bytesNeeded pcbBytesNeeded lpServicesReturned
            actualServices <- peek lpServicesReturned
            rawStatuses <- peekArray (fromIntegral actualServices) lpServices
            mapM rawEssToEss rawStatuses
  where
    rawEssToEss rawEss = do
      svcName <- peekCWString $ rawESSServiceName rawEss
      dispName <- peekCWString $ rawESSDisplayName rawEss
      return EnumServiceStatus
        { enumServiceName = svcName
        , enumDisplayName = dispName
        , enumServiceStatus = rawESSServiceStatus rawEss
        }
