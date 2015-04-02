module System.Win32.SystemServices.Services.SCM_ACCESS_RIGHTS
  ( SCM_ACCESS_RIGHTS (..)
  , toDWORD
  , peekSCManagerAccess
  , pokeSCManagerAccess
  ) where

import Control.Applicative
import Data.Bits
import Data.Maybe
import Foreign
import System.Win32.Types
import Text.Printf

data SCM_ACCESS_RIGHTS
  = ALL_ACCESS
  | CREATE_SERVICE
  | CONNECT
  | ENUMERATE_SERVICE
  | LOCK
  | MODIFY_BOOT_CONFIG
  | QUERY_LOCK_STATUS
  deriving (Show)

peekSCManagerAccess :: Ptr DWORD -> IO [SCM_ACCESS_RIGHTS]
peekSCManagerAccess ptr = unflag <$> peek ptr

pokeSCManagerAccess :: Ptr DWORD -> [SCM_ACCESS_RIGHTS] -> IO ()
pokeSCManagerAccess ptr smas = poke ptr . flag $ smas

toDWORD :: SCM_ACCESS_RIGHTS -> DWORD
toDWORD ALL_ACCESS         = 0x000F003F
toDWORD CREATE_SERVICE     = 0x00000002
toDWORD CONNECT            = 0x00000001
toDWORD ENUMERATE_SERVICE  = 0x00000004
toDWORD LOCK               = 0x00000008
toDWORD MODIFY_BOOT_CONFIG = 0x00000020
toDWORD QUERY_LOCK_STATUS  = 0x00000010

fromDWORD :: DWORD -> Either String SCM_ACCESS_RIGHTS
fromDWORD 0x000F003F = Right ALL_ACCESS
fromDWORD 0x00000002 = Right CREATE_SERVICE
fromDWORD 0x00000001 = Right CONNECT
fromDWORD 0x00000004 = Right ENUMERATE_SERVICE
fromDWORD 0x00000008 = Right LOCK
fromDWORD 0x00000020 = Right MODIFY_BOOT_CONFIG
fromDWORD 0x00000010 = Right QUERY_LOCK_STATUS
fromDWORD x = Left $ "The " ++ printf "%x" x ++ " control code is unsupported by this binding."

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

unflag :: DWORD -> [SCM_ACCESS_RIGHTS]
unflag f = mapMaybe (hush . fromDWORD . (.&. f)) masks
  where
    masks = take 32 $ iterate (`shiftL` 1) 1

flag :: [SCM_ACCESS_RIGHTS] -> DWORD
flag = foldl (\flag' f -> flag' .|. toDWORD f) 0
