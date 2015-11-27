module System.Win32.SystemServices.Services.SERVICE_OPTIONAL
(
  SERVICE_OPTIONAL(..)
, toDWORD
)

where
import System.Win32.Types

data SERVICE_OPTIONAL =
  SERVICE_NO_CHANGE
  deriving (Show)

toDWORD :: SERVICE_OPTIONAL -> DWORD
toDWORD SERVICE_NO_CHANGE = 0xffffffff
