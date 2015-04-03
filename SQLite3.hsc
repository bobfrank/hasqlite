-- vim: set filetype=haskell :
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.HasQLite where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as DBS
import Data.ByteString.Unsafe (unsafePackCString)
--import Data.Text

#include <sqlite3.h>

sqlite_version = #const_str SQLITE_VERSION
sqlite_version_number = #const SQLITE_VERSION_NUMBER
sqlite_source_id = #const_str SQLITE_SOURCE_ID

type SQLite3 = Ptr ()
type SQLite3Statement = Ptr ()
type SQLite3Context = Ptr ()
type SQLite3Value = Ptr ()
type SQLite3ModulePtr = Ptr SQLite3Module
type SQLite3VTab = Ptr ()
type SQLite3VTabCursor = Ptr ()
type SQLite3IndexInfo = Ptr ()
type SQLite3Backup = Ptr ()
type SQLite3Blob = Ptr ()
type SQLite3File = Ptr SQLite3IOMethods
type SQLite3VfsPtr = Ptr SQLite3Vfs

newtype SQLite3Flag = SQLite3Flag { unSQLite3Flag :: CInt } deriving (Eq,Show)
#{enum SQLite3Flag, SQLite3Flag
  , sqlite_open_readonly        = SQLITE_OPEN_READONLY
  , sqlite_open_readwrite       = SQLITE_OPEN_READWRITE
  , sqlite_open_create          = SQLITE_OPEN_CREATE
  , sqlite_open_deleteonclose   = SQLITE_OPEN_DELETEONCLOSE
  , sqlite_open_exclusive       = SQLITE_OPEN_EXCLUSIVE
  , sqlite_open_autoproxy       = SQLITE_OPEN_AUTOPROXY
  , sqlite_open_uri             = SQLITE_OPEN_URI
  , sqlite_open_memory          = SQLITE_OPEN_MEMORY
  , sqlite_open_main_db         = SQLITE_OPEN_MAIN_DB
  , sqlite_open_temp_db         = SQLITE_OPEN_TEMP_DB
  , sqlite_open_transient_db    = SQLITE_OPEN_TRANSIENT_DB
  , sqlite_open_main_journal    = SQLITE_OPEN_MAIN_JOURNAL
  , sqlite_open_temp_journal    = SQLITE_OPEN_TEMP_JOURNAL
  , sqlite_open_subjournal      = SQLITE_OPEN_SUBJOURNAL
  , sqlite_open_master_journal  = SQLITE_OPEN_MASTER_JOURNAL
  , sqlite_open_nomutex         = SQLITE_OPEN_NOMUTEX
  , sqlite_open_fullmutex       = SQLITE_OPEN_FULLMUTEX
  , sqlite_open_sharedcache     = SQLITE_OPEN_SHAREDCACHE
  , sqlite_open_privatecache    = SQLITE_OPEN_PRIVATECACHE
  , sqlite_open_wal             = SQLITE_OPEN_WAL
}

newtype SQLite3VTabConstraints = SQLite3VTabConstraints { unSQLite3VTabConstraints :: CInt } deriving (Eq,Show)
#{enum SQLite3VTabConstraints, SQLite3VTabConstraints
  , sqlite_index_constraint_eq      = SQLITE_INDEX_CONSTRAINT_EQ
  , sqlite_index_constraint_gt      = SQLITE_INDEX_CONSTRAINT_GT
  , sqlite_index_constraint_le      = SQLITE_INDEX_CONSTRAINT_LE
  , sqlite_index_constraint_lt      = SQLITE_INDEX_CONSTRAINT_LT
  , sqlite_index_constraint_ge      = SQLITE_INDEX_CONSTRAINT_GE
  , sqlite_index_constraint_match   = SQLITE_INDEX_CONSTRAINT_MATCH
}

newtype SQLite3Result = SQLite3Result { unSQLite3Result :: CInt } deriving (Eq,Show)
#{enum SQLite3Result, SQLite3Result
  , sqlite_ok           = SQLITE_OK
  , sqlite_error        = SQLITE_ERROR
  , sqlite_internal     = SQLITE_INTERNAL
  , sqlite_perm         = SQLITE_PERM
  , sqlite_abort        = SQLITE_ABORT
  , sqlite_busy         = SQLITE_BUSY
  , sqlite_locked       = SQLITE_LOCKED
  , sqlite_nomem        = SQLITE_NOMEM
  , sqlite_readonly     = SQLITE_READONLY
  , sqlite_interrupt    = SQLITE_INTERRUPT
  , sqlite_ioerr        = SQLITE_IOERR
  , sqlite_corrupt      = SQLITE_CORRUPT
  , sqlite_notfound     = SQLITE_NOTFOUND
  , sqlite_full         = SQLITE_FULL
  , sqlite_cantopen     = SQLITE_CANTOPEN
  , sqlite_protocol     = SQLITE_PROTOCOL
  , sqlite_empty        = SQLITE_EMPTY
  , sqlite_schema       = SQLITE_SCHEMA
  , sqlite_toobig       = SQLITE_TOOBIG
  , sqlite_constraint   = SQLITE_CONSTRAINT
  , sqlite_mismatch     = SQLITE_MISMATCH
  , sqlite_misuse       = SQLITE_MISUSE
  , sqlite_nolfs        = SQLITE_NOLFS
  , sqlite_auth         = SQLITE_AUTH
  , sqlite_format       = SQLITE_FORMAT
  , sqlite_range        = SQLITE_RANGE
  , sqlite_notadb       = SQLITE_NOTADB
  , sqlite_notice       = SQLITE_NOTICE
  , sqlite_warning      = SQLITE_WARNING
  , sqlite_row          = SQLITE_ROW
  , sqlite_done         = SQLITE_DONE
}

newtype SQLite3Encoding = SQLite3Encoding { unSQLite3Encoding :: CInt } deriving (Eq,Show)
#{enum SQLite3Encoding, SQLite3Encoding
  , sqlite_utf8             = SQLITE_UTF8
  , sqlite_utf16le          = SQLITE_UTF16LE
  , sqlite_utf16be          = SQLITE_UTF16BE
  , sqlite_utf16            = SQLITE_UTF16
  , sqlite_any              = SQLITE_ANY
  , sqlite_utf16_aligned    = SQLITE_UTF16_ALIGNED
}

newtype SQLite3Type = SQLite3Type { unSQLite3Type :: CInt } deriving (Eq,Show)
#{enum SQLite3Type, SQLite3Type
  , sqlite_integer  = SQLITE_INTEGER
  , sqlite_float    = SQLITE_FLOAT
  , sqlite_blob     = SQLITE_BLOB
  , sqlite_text     = SQLITE3_TEXT
}

newtype SQLite3AuthCode = SQLite3AuthCode { unSQLite3AuthCode :: CInt } deriving (Eq,Show)
#{enum SQLite3AuthCode, SQLite3AuthCode
  , sqlite_create_index         = SQLITE_CREATE_INDEX
  , sqlite_create_table         = SQLITE_CREATE_TABLE
  , sqlite_create_temp_index    = SQLITE_CREATE_TEMP_INDEX
  , sqlite_create_temp_table    = SQLITE_CREATE_TEMP_TABLE
  , sqlite_create_temp_trigger  = SQLITE_CREATE_TEMP_TRIGGER
  , sqlite_create_temp_view     = SQLITE_CREATE_TEMP_VIEW
  , sqlite_create_view          = SQLITE_CREATE_VIEW
  , sqlite_delete               = SQLITE_DELETE
  , sqlite_drop_index           = SQLITE_DROP_INDEX
  , sqlite_drop_table           = SQLITE_DROP_TABLE
  , sqlite_drop_temp_index      = SQLITE_DROP_TEMP_INDEX
  , sqlite_drop_temp_table      = SQLITE_DROP_TEMP_TABLE
  , sqlite_drop_temp_trigger    = SQLITE_DROP_TEMP_TRIGGER
  , sqlite_drop_trigger         = SQLITE_DROP_TRIGGER
  , sqlite_drop_view            = SQLITE_DROP_VIEW
  , sqlite_insert               = SQLITE_INSERT
  , sqlite_pragma               = SQLITE_PRAGMA
  , sqlite_read                 = SQLITE_READ
  , sqlite_select               = SQLITE_SELECT
  , sqlite_transaction          = SQLITE_TRANSACTION
  , sqlite_update               = SQLITE_UPDATE
  , sqlite_attach               = SQLITE_ATTACH
  , sqlite_detach               = SQLITE_DETACH
  , sqlite_alter_table          = SQLITE_ALTER_TABLE
  , sqlite_reindex              = SQLITE_REINDEX
  , sqlite_analyze              = SQLITE_ANALYZE
  , sqlite_create_vtable        = SQLITE_CREATE_VTABLE
  , sqlite_drop_vtable          = SQLITE_DROP_VTABLE
  , sqlite_function             = SQLITE_FUNCTION
  , sqlite_savepoint            = SQLITE_SAVEPOINT
  , sqlite_copy                 = SQLITE_COPY
}
--  , sqlite_recursive = SQLITE_RECURSIVE

data SQLite3Module = SQLite3Module {
    sqlite3_module_iVersion         :: CInt,
    sqlite3_module_xCreate          :: FunPtr (SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt),
    sqlite3_module_xConnect         :: FunPtr (SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt),
    sqlite3_module_xBestIndex       :: FunPtr (SQLite3VTab -> SQLite3IndexInfo -> IO CInt),
    sqlite3_module_xDisconnect      :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xDestroy         :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xOpen            :: FunPtr (SQLite3VTab -> Ptr SQLite3VTabCursor -> IO CInt),
    sqlite3_module_xClose           :: FunPtr (SQLite3VTabCursor -> IO CInt),
    sqlite3_module_xFilter          :: FunPtr (SQLite3VTabCursor -> CInt -> CString -> Ptr SQLite3Value -> IO CInt),
    sqlite3_module_xNext            :: FunPtr (SQLite3VTabCursor -> IO CInt),
    sqlite3_module_xEof             :: FunPtr (SQLite3VTabCursor -> IO CInt),
    sqlite3_module_xColumn          :: FunPtr (SQLite3VTabCursor -> SQLite3Context -> CInt -> IO CInt),
    sqlite3_module_xRowid           :: FunPtr (SQLite3VTabCursor -> Ptr CLong -> IO CInt),
    sqlite3_module_xUpdate          :: FunPtr (SQLite3VTab -> CInt -> Ptr SQLite3Value -> Ptr CLong -> IO CInt),
    sqlite3_module_xBegin           :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xSync            :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xCommit          :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xRollback        :: FunPtr (SQLite3VTab -> IO CInt),
    sqlite3_module_xFindFunction    :: FunPtr (SQLite3VTab -> CInt -> CString -> FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO (Ptr ())) -> Ptr (Ptr ()) -> IO CInt),
    sqlite3_module_xRename          :: FunPtr (SQLite3VTab -> CString -> IO CInt),
    sqlite3_module_xSavepoint       :: FunPtr (SQLite3VTab -> CInt -> IO CInt),
    sqlite3_module_xRelease         :: FunPtr (SQLite3VTab -> CInt -> IO CInt),
    sqlite3_module_xRollbackTo      :: FunPtr (SQLite3VTab -> CInt -> IO CInt)
    } deriving (Show)

instance Storable SQLite3Module where
    sizeOf    _ = (#size struct sqlite3_module)
    alignment _ = alignment (undefined :: CUInt)
    peek ptr = error "Unimplemented"
    poke ptr smod = do
         (#poke struct sqlite3_module, iVersion)        ptr $ sqlite3_module_iVersion smod
         (#poke struct sqlite3_module, xCreate)         ptr $ sqlite3_module_xCreate smod
         (#poke struct sqlite3_module, xConnect)        ptr $ sqlite3_module_xConnect smod
         (#poke struct sqlite3_module, xBestIndex)      ptr $ sqlite3_module_xBestIndex smod
         (#poke struct sqlite3_module, xDisconnect)     ptr $ sqlite3_module_xDisconnect smod
         (#poke struct sqlite3_module, xDestroy)        ptr $ sqlite3_module_xDestroy smod
         (#poke struct sqlite3_module, xOpen)           ptr $ sqlite3_module_xOpen smod
         (#poke struct sqlite3_module, xClose)          ptr $ sqlite3_module_xClose smod
         (#poke struct sqlite3_module, xFilter)         ptr $ sqlite3_module_xFilter smod
         (#poke struct sqlite3_module, xNext)           ptr $ sqlite3_module_xNext smod
         (#poke struct sqlite3_module, xEof)            ptr $ sqlite3_module_xEof smod
         (#poke struct sqlite3_module, xColumn)         ptr $ sqlite3_module_xColumn smod
         (#poke struct sqlite3_module, xRowid)          ptr $ sqlite3_module_xRowid smod
         (#poke struct sqlite3_module, xUpdate)         ptr $ sqlite3_module_xUpdate smod
         (#poke struct sqlite3_module, xBegin)          ptr $ sqlite3_module_xBegin smod
         (#poke struct sqlite3_module, xSync)           ptr $ sqlite3_module_xSync smod
         (#poke struct sqlite3_module, xCommit)         ptr $ sqlite3_module_xCommit smod
         (#poke struct sqlite3_module, xRollback)       ptr $ sqlite3_module_xRollback smod
         (#poke struct sqlite3_module, xFindFunction)   ptr $ sqlite3_module_xFindFunction smod
         (#poke struct sqlite3_module, xRename)         ptr $ sqlite3_module_xRename smod
         (#poke struct sqlite3_module, xSavepoint)      ptr $ sqlite3_module_xSavepoint smod
         (#poke struct sqlite3_module, xRelease)        ptr $ sqlite3_module_xRelease smod
         (#poke struct sqlite3_module, xRollbackTo)     ptr $ sqlite3_module_xRollbackTo smod

data SQLite3IOMethods = SQLite3IOMethods {
    sqlite3_io_iVersion                 :: CInt,
    sqlite3_io_xClose                   :: FunPtr (SQLite3File -> IO CInt),
    sqlite3_io_xRead                    :: FunPtr (SQLite3File -> Ptr () -> CInt -> CLong -> IO CInt),
    sqlite3_io_xWrite                   :: FunPtr (SQLite3File -> Ptr () -> CInt -> CLong -> IO CInt),
    sqlite3_io_xTruncate                :: FunPtr (SQLite3File -> CLong -> IO CInt),
    sqlite3_io_xSync                    :: FunPtr (SQLite3File -> CInt -> IO CInt),
    sqlite3_io_xFileSize                :: FunPtr (SQLite3File -> CLong -> IO CInt),
    sqlite3_io_xLock                    :: FunPtr (SQLite3File -> CInt -> IO CInt),
    sqlite3_io_xUnlock                  :: FunPtr (SQLite3File -> CInt -> IO CInt),
    sqlite3_io_xCheckReservedLock       :: FunPtr (SQLite3File -> Ptr CInt -> IO CInt),
    sqlite3_io_xFileControl             :: FunPtr (SQLite3File -> CInt -> Ptr () -> IO CInt),
    sqlite3_io_xSectorSize              :: FunPtr (SQLite3File -> IO CInt),
    sqlite3_io_xDeviceCharacteristics   :: FunPtr (SQLite3File -> IO CInt),
    sqlite3_io_xShmMap                  :: FunPtr (SQLite3File -> CInt -> CInt -> CInt -> Ptr (Ptr ()) -> IO CInt),
    sqlite3_io_xShmLock                 :: FunPtr (SQLite3File -> CInt -> CInt -> CInt -> IO CInt),
    sqlite3_io_xShmBarrier              :: FunPtr (SQLite3File -> IO ()),
    sqlite3_io_xShmUnmap                :: FunPtr (SQLite3File -> CInt -> IO CInt),
    sqlite3_io_xFetch                   :: FunPtr (SQLite3File -> CLong -> CInt -> Ptr (Ptr ()) -> IO CInt),
    sqlite3_io_xUnfetch                 :: FunPtr (SQLite3File -> CLong -> Ptr () -> IO CInt)
    } deriving (Show)

instance Storable SQLite3IOMethods where
    sizeOf    _ = (#size struct sqlite3_io_methods)
    alignment _ = alignment (undefined :: CUInt)
    peek ptr = error "Unimplemented"
    poke ptr iom = do
         (#poke struct sqlite3_io_methods, iVersion) ptr $ sqlite3_io_iVersion iom
         (#poke struct sqlite3_io_methods, xClose) ptr $ sqlite3_io_xClose iom
         (#poke struct sqlite3_io_methods, xRead) ptr $ sqlite3_io_xRead iom
         (#poke struct sqlite3_io_methods, xWrite) ptr $ sqlite3_io_xWrite iom
         (#poke struct sqlite3_io_methods, xTruncate) ptr $ sqlite3_io_xTruncate iom
         (#poke struct sqlite3_io_methods, xSync) ptr $ sqlite3_io_xSync iom
         (#poke struct sqlite3_io_methods, xFileSize) ptr $ sqlite3_io_xFileSize iom
         (#poke struct sqlite3_io_methods, xLock) ptr $ sqlite3_io_xLock iom
         (#poke struct sqlite3_io_methods, xUnlock) ptr $ sqlite3_io_xUnlock iom
         (#poke struct sqlite3_io_methods, xCheckReservedLock) ptr $ sqlite3_io_xCheckReservedLock iom
         (#poke struct sqlite3_io_methods, xFileControl) ptr $ sqlite3_io_xFileControl iom
         (#poke struct sqlite3_io_methods, xSectorSize) ptr $ sqlite3_io_xSectorSize iom
         (#poke struct sqlite3_io_methods, xDeviceCharacteristics) ptr $ sqlite3_io_xDeviceCharacteristics iom
         (#poke struct sqlite3_io_methods, xShmMap) ptr $ sqlite3_io_xShmMap iom
         (#poke struct sqlite3_io_methods, xShmLock) ptr $ sqlite3_io_xShmLock iom
         (#poke struct sqlite3_io_methods, xShmBarrier) ptr $ sqlite3_io_xShmBarrier iom
         (#poke struct sqlite3_io_methods, xShmUnmap) ptr $ sqlite3_io_xShmUnmap iom
         (#poke struct sqlite3_io_methods, xFetch) ptr $ sqlite3_io_xFetch iom
         (#poke struct sqlite3_io_methods, xUnfetch) ptr $ sqlite3_io_xUnfetch iom

data SQLite3Vfs = SQLite3Vfs {
    sqlite3_vfs_iVersion            :: CInt,
    sqlite3_vfs_szOsFile            :: CInt,
    sqlite3_vfs_mxPathname          :: CInt,
    sqlite3_vfs_pNext               :: SQLite3VfsPtr,
    sqlite3_vfs_zName               :: CString,
    sqlite3_vfs_pAppData            :: Ptr (),
    sqlite3_vfs_xOpen               :: FunPtr (SQLite3VfsPtr -> CString -> SQLite3File -> CInt -> Ptr CInt -> IO CInt),
    sqlite3_vfs_xDelete             :: FunPtr (SQLite3VfsPtr -> CString -> CInt -> IO CInt),
    sqlite3_vfs_xAccess             :: FunPtr (SQLite3VfsPtr -> CString -> CInt -> Ptr CInt -> IO CInt),
    sqlite3_vfs_xFullPathname       :: FunPtr (SQLite3VfsPtr -> CString -> CInt -> CString -> IO CInt),
    sqlite3_vfs_xDlOpen             :: FunPtr (SQLite3VfsPtr -> CString -> IO (Ptr ())),
    sqlite3_vfs_xDlError            :: FunPtr (SQLite3VfsPtr -> CInt -> CString -> IO ()),
    sqlite3_vfs_xDlSym              :: FunPtr (SQLite3VfsPtr -> Ptr () -> CString -> IO (Ptr ())),
    sqlite3_vfs_xDlClose            :: FunPtr (SQLite3VfsPtr -> Ptr () -> IO CInt),
    sqlite3_vfs_xRandomness         :: FunPtr (SQLite3VfsPtr -> CInt -> CString -> IO CInt),
    sqlite3_vfs_xSleep              :: FunPtr (SQLite3VfsPtr -> CInt -> IO CInt),
    sqlite3_vfs_xCurrentTime        :: FunPtr (SQLite3VfsPtr -> Ptr Double -> IO CInt),
    sqlite3_vfs_xGetLastError       :: FunPtr (SQLite3VfsPtr -> CInt -> CString -> IO CInt),
    sqlite3_vfs_xCurrentTimeInt64   :: FunPtr (SQLite3VfsPtr -> Ptr CLong -> IO CInt),
    sqlite3_vfs_xSetSystemCall      :: FunPtr (SQLite3VfsPtr -> CString -> FunPtr (IO ()) -> IO CInt),
    sqlite3_vfs_xGetSystemCall      :: FunPtr (SQLite3VfsPtr -> CString -> IO (FunPtr (IO ()))),
    sqlite3_vfs_xNextSystemCall     :: FunPtr (SQLite3VfsPtr -> CString -> IO CString)
    } deriving (Show)

instance Storable SQLite3Vfs where
    sizeOf    _ = (#size struct sqlite3_vfs)
    alignment _ = alignment (undefined :: CUInt)
    peek ptr = error "Unimplemented"
    poke ptr vfs = do
         (#poke struct sqlite3_vfs, iVersion)           ptr $ sqlite3_vfs_iVersion vfs
         (#poke struct sqlite3_vfs, szOsFile)           ptr $ sqlite3_vfs_szOsFile vfs
         (#poke struct sqlite3_vfs, mxPathname)         ptr $ sqlite3_vfs_mxPathname vfs
         (#poke struct sqlite3_vfs, pNext)              ptr $ sqlite3_vfs_pNext vfs
         (#poke struct sqlite3_vfs, zName)              ptr $ sqlite3_vfs_zName vfs
         (#poke struct sqlite3_vfs, pAppData)           ptr $ sqlite3_vfs_pAppData vfs
         (#poke struct sqlite3_vfs, xOpen)              ptr $ sqlite3_vfs_xOpen vfs
         (#poke struct sqlite3_vfs, xDelete)            ptr $ sqlite3_vfs_xDelete vfs
         (#poke struct sqlite3_vfs, xAccess)            ptr $ sqlite3_vfs_xAccess vfs
         (#poke struct sqlite3_vfs, xFullPathname)      ptr $ sqlite3_vfs_xFullPathname vfs
         (#poke struct sqlite3_vfs, xDlOpen)            ptr $ sqlite3_vfs_xDlOpen vfs
         (#poke struct sqlite3_vfs, xDlError)           ptr $ sqlite3_vfs_xDlError vfs
         (#poke struct sqlite3_vfs, xDlSym)             ptr $ sqlite3_vfs_xDlSym vfs
         (#poke struct sqlite3_vfs, xDlClose)           ptr $ sqlite3_vfs_xDlClose vfs
         (#poke struct sqlite3_vfs, xRandomness)        ptr $ sqlite3_vfs_xRandomness vfs
         (#poke struct sqlite3_vfs, xSleep)             ptr $ sqlite3_vfs_xSleep vfs
         (#poke struct sqlite3_vfs, xCurrentTime)       ptr $ sqlite3_vfs_xCurrentTime vfs
         (#poke struct sqlite3_vfs, xGetLastError)      ptr $ sqlite3_vfs_xGetLastError vfs
         (#poke struct sqlite3_vfs, xCurrentTimeInt64)  ptr $ sqlite3_vfs_xCurrentTimeInt64 vfs
         (#poke struct sqlite3_vfs, xSetSystemCall)     ptr $ sqlite3_vfs_xSetSystemCall vfs
         (#poke struct sqlite3_vfs, xGetSystemCall)     ptr $ sqlite3_vfs_xGetSystemCall vfs
         (#poke struct sqlite3_vfs, xNextSystemCall)    ptr $ sqlite3_vfs_xNextSystemCall vfs

foreign import ccall sqlite3_open_v2 :: CString -> Ptr SQLite3 -> CInt -> CString -> IO CInt
foreign import ccall sqlite3_close_v2 :: SQLite3 -> IO CInt
foreign import ccall sqlite3_prepare_v2 :: SQLite3 -> CString -> CInt -> Ptr SQLite3Statement -> Ptr CString -> IO CInt
foreign import ccall sqlite3_step :: SQLite3Statement -> IO CInt
foreign import ccall sqlite3_finalize :: SQLite3Statement -> IO CInt
foreign import ccall sqlite3_exec :: SQLite3 -> CString -> FunPtr () -> Ptr () -> Ptr CString -> IO CInt
foreign import ccall sqlite3_reset :: SQLite3Statement -> IO CInt
foreign import ccall sqlite3_changes :: SQLite3 -> IO CInt
foreign import ccall sqlite3_errmsg :: SQLite3 -> IO CString
foreign import ccall sqlite3_bind_parameter_name :: SQLite3Statement -> CInt -> IO CString
foreign import ccall sqlite3_bind_parameter_index :: SQLite3Statement -> CString -> IO CInt
foreign import ccall sqlite3_bind_double :: SQLite3Statement -> CInt -> CDouble -> IO CInt
foreign import ccall sqlite3_bind_int :: SQLite3Statement -> CInt -> CInt -> IO CInt
foreign import ccall sqlite3_bind_int64 :: SQLite3Statement -> CInt -> CLong -> IO CInt
foreign import ccall sqlite3_bind_text :: SQLite3Statement -> CInt -> CString -> IO CInt
foreign import ccall sqlite3_bind_null :: SQLite3Statement -> CInt -> IO CInt
foreign import ccall sqlite3_bind_blob :: SQLite3Statement -> CInt -> Ptr () -> CInt -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall sqlite3_bind_value :: SQLite3Statement -> CInt -> SQLite3Value -> IO CInt
foreign import ccall sqlite3_bind_zeroblob :: SQLite3Statement -> CInt -> CInt -> IO CInt
foreign import ccall sqlite3_clear_bindings :: SQLite3Statement -> IO CInt
foreign import ccall sqlite3_get_table ::   SQLite3 -> CString -> Ptr (Ptr CString) -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO CInt
foreign import ccall sqlite3_free_table :: Ptr CString -> IO ()
foreign import ccall sqlite3_free :: CString -> IO ()
foreign import ccall sqlite3_column_count :: SQLite3Statement -> IO CInt
foreign import ccall sqlite3_column_double :: SQLite3Statement -> CInt -> IO CDouble
foreign import ccall sqlite3_column_int :: SQLite3Statement -> CInt -> IO CInt
foreign import ccall sqlite3_column_bytes :: SQLite3Statement -> CInt -> IO CInt
foreign import ccall sqlite3_column_int64 :: SQLite3Statement -> CInt -> IO CLong
foreign import ccall sqlite3_column_text :: SQLite3Statement -> CInt -> IO CString
foreign import ccall sqlite3_column_name :: SQLite3Statement -> CInt -> IO CString
foreign import ccall sqlite3_column_type :: SQLite3Statement -> CInt -> IO CInt
foreign import ccall sqlite3_column_decltype :: SQLite3Statement -> CInt -> IO CString
foreign import ccall sqlite3_column_blob :: SQLite3Statement -> CInt -> IO (Ptr ())
foreign import ccall sqlite3_column_value :: SQLite3Statement -> CInt -> IO SQLite3Value
foreign import ccall sqlite3_declare_vtab :: SQLite3 -> CString -> IO CInt
foreign import ccall sqlite3_user_data :: SQLite3Context -> IO (Ptr ())
foreign import ccall sqlite3_result_blob :: SQLite3Context -> Ptr () -> CInt -> FunPtr (Ptr () -> IO ()) -> IO ()
foreign import ccall sqlite3_result_double :: SQLite3Context -> CDouble -> IO ()
foreign import ccall sqlite3_result_error :: SQLite3Context -> CString -> CInt -> IO ()
foreign import ccall sqlite3_result_error_toobig :: SQLite3Context -> IO ()
foreign import ccall sqlite3_result_error_nomem :: SQLite3Context -> IO ()
foreign import ccall sqlite3_result_error_code :: SQLite3Context -> CInt -> IO ()
foreign import ccall sqlite3_result_int :: SQLite3Context -> CInt -> IO ()
foreign import ccall sqlite3_result_int64 :: SQLite3Context -> CLong -> IO ()
foreign import ccall sqlite3_result_null :: SQLite3Context -> IO ()
foreign import ccall sqlite3_result_text :: SQLite3Context -> CString -> CInt -> CInt -> IO ()
-- FunPtr (Ptr () -> IO ())
foreign import ccall sqlite3_result_value :: SQLite3Context -> SQLite3Value -> IO ()
foreign import ccall sqlite3_result_zeroblob :: SQLite3Context -> CInt -> IO ()
foreign import ccall sqlite3_wal_checkpoint_v2 :: SQLite3 -> CString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall sqlite3_set_authorizer :: SQLite3 -> FunPtr (Ptr () -> CInt -> CString -> CString -> CString -> CString -> IO CInt) -> IO CInt
foreign import ccall sqlite3_enable_load_extension :: SQLite3 -> CString -> CString -> Ptr CString -> IO CInt
foreign import ccall sqlite3_create_collation_v2 :: SQLite3 -> CString -> CInt -> Ptr () -> FunPtr (Ptr () -> CInt -> Ptr () -> CInt -> Ptr () -> IO CInt) -> FunPtr (Ptr () -> IO ()) -> IO CInt
foreign import ccall sqlite3_backup_init :: SQLite3 -> CString -> SQLite3 -> CString -> IO SQLite3Backup
foreign import ccall sqlite3_backup_step :: SQLite3Backup -> CInt -> IO CInt
foreign import ccall sqlite3_backup_finish :: SQLite3Backup -> IO CInt
foreign import ccall sqlite3_backup_remaining :: SQLite3Backup -> IO CInt
foreign import ccall sqlite3_backup_pagecount :: SQLite3Backup -> IO CInt
foreign import ccall sqlite3_blob_open :: SQLite3 -> CString -> CString -> CString -> CLong -> CInt -> Ptr SQLite3Blob -> IO CInt
foreign import ccall sqlite3_blob_read :: SQLite3Blob -> Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall sqlite3_blob_close :: SQLite3Blob -> IO CInt
foreign import ccall sqlite3_blob_bytes :: SQLite3Blob -> IO CInt
foreign import ccall sqlite3_blob_reopen :: SQLite3Blob -> CLong -> IO CInt
foreign import ccall sqlite3_blob_write :: SQLite3Blob -> Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall sqlite3_create_module_v2 :: SQLite3 -> CString -> SQLite3ModulePtr -> Ptr () -> FunPtr (SQLite3VTab -> IO CInt) -> IO CInt
foreign import ccall sqlite3_create_function_v2 :: SQLite3 -> CString -> CInt -> CInt -> Ptr () -> FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO ()) -> FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO ()) -> FunPtr (SQLite3Context -> IO ()) -> FunPtr (Ptr () -> IO ()) -> IO ()
foreign import ccall sqlite3_trace :: SQLite3 -> FunPtr (Ptr () -> CString -> IO ()) -> Ptr () -> IO (Ptr ())
foreign import ccall sqlite3_profile :: SQLite3 -> FunPtr (Ptr () -> CString -> CULong -> IO ()) -> Ptr () -> IO (Ptr ())
foreign import ccall sqlite3_value_blob :: SQLite3Value -> IO (Ptr ())
foreign import ccall sqlite3_value_bytes :: SQLite3Value -> IO CInt
foreign import ccall sqlite3_value_double :: SQLite3Value -> IO CDouble
foreign import ccall sqlite3_value_int :: SQLite3Value -> IO CInt
foreign import ccall sqlite3_value_int64 :: SQLite3Value -> IO CLong
foreign import ccall sqlite3_value_text :: SQLite3Value -> IO CString
foreign import ccall sqlite3_value_type :: SQLite3Value -> IO CInt
foreign import ccall sqlite3_value_numeric_type :: SQLite3Value -> IO CInt
foreign import ccall sqlite3_vfs_find :: CString -> IO (SQLite3VfsPtr)
foreign import ccall sqlite3_vfs_register :: SQLite3VfsPtr -> CInt -> IO CInt
foreign import ccall sqlite3_vfs_unregister :: SQLite3VfsPtr -> IO CInt


data SqliteValue = SqliteNull | SqliteText DBS.ByteString | SqliteInt Int | SqliteDouble Double deriving (Show)
type SQLite3Row = [SqliteValue]

fromSql :: SqliteValue a => SqliteValue -> a
fromSql 

extractTypeStatement stmt col (#const SQLITE_INTEGER) = sqlite3_column_int stmt col >>= return . SqliteInt . fromIntegral
extractTypeStatement stmt col (#const SQLITE_FLOAT) = sqlite3_column_double stmt col >>= return . SqliteDouble . realToFrac
extractTypeStatement stmt col (#const SQLITE_TEXT) = sqlite3_column_text stmt col >>= unsafePackCString >>= return . SqliteText

extractTypeValue val (#const SQLITE_INTEGER) = sqlite3_value_int val >>= return . SqliteInt . fromIntegral
extractTypeValue val (#const SQLITE_FLOAT) = sqlite3_value_double val >>= return . SqliteDouble . realToFrac
extractTypeValue val (#const SQLITE_TEXT) = sqlite3_value_text val >>= DBS.packCString >>= return . SqliteText
extractTypeValue val (#const SQLITE_NULL) = return $ SqliteNull
extractTypeValue _ typ = error $ show $ typ
pushResultValue ctx (SqliteInt i) = sqlite3_result_int ctx (fromIntegral i)
pushResultValue ctx (SqliteDouble d) = sqlite3_result_double ctx (realToFrac d)
--pushResultValue ctx (SqliteText t) = withCString t $ \ctext -> sqlite3_result_text ctext (fromIntegral (-1)) (fromIntegral (-1))

-- import Control.Exception(Exception,throw)
-- data SqlError = SqlError String Int String deriving (Show,Typeable)
-- instance Exception SqlError
-- -- handleSqlResult from http://code.haskell.org/HSQL/SQLite3/Database/HSQL/SQLite3.hsc
-- handleSqlResult :: SQLite3 -> CInt -> IO ()
-- handleSqlResult sqlite res
--      | SQLite3Result res == sqlite_ok = return ()
--      | otherwise = do
--             pMsg <- sqlite3_errmsg sqlite
--             msg <- peekCString pMsg
--             sqlite3_free pMsg
--             throw (SqlError "E" (fromIntegral res) msg)

sqlite3Open :: String -> [SQLite3Flag] -> Maybe String -> IO SQLite3
sqlite3Open filename flags vfs =
    let cflags = fromIntegral $ foldl (.|.) 0 $ map unSQLite3Flag flags
    in withCString filename $ \cfilename ->
    alloca $ \ppDb -> do
        rcode <- if isJust vfs
            then withCString (fromJust vfs) $ \cvfs ->
                sqlite3_open_v2 cfilename ppDb cflags cvfs
            else sqlite3_open_v2 cfilename ppDb cflags nullPtr
        peek ppDb

sqlite3Prepare :: SQLite3 -> String -> Int -> IO (SQLite3Statement,String)
sqlite3Prepare conn statement maxlen =
    withCString statement $ \cstatement ->
    alloca $ \pstmt ->
    alloca $ \cstatementRemain -> do
    rcode <- sqlite3_prepare_v2 conn cstatement (fromIntegral maxlen) pstmt cstatementRemain
    stmt <- peek pstmt
    remainPtr <- peek cstatementRemain
    remaining <- peekCString remainPtr
    return (stmt,remaining)

sqlite3Step :: SQLite3Statement -> IO Int
sqlite3Step stmt = sqlite3_step stmt >>= return . fromIntegral

sqlite3Finalize :: SQLite3Statement -> IO ()
sqlite3Finalize stmt = do
  rcode <- sqlite3_finalize stmt
  return ()

sqlite3Close :: SQLite3 -> IO ()
sqlite3Close pDb = do
    rcode <- sqlite3_close_v2 pDb
    return ()

sqlite3execute' stmt count types rcode =
    case rcode of
        (#const SQLITE_ROW) -> do
            row <- zipWithM (extractTypeStatement stmt) [0..(count-1)] types
            rest <- unsafeInterleaveIO $ sqlite3execute'' stmt count types
            return $ row:rest
        (#const SQLITE_DONE) -> sqlite3Finalize stmt >> return []
        _   -> error "uh oh"

sqlite3execute'' stmt count types = sqlite3Step stmt >>= return . fromIntegral >>= sqlite3execute' stmt count types

data Sqlite3Bind = Sqlite3BindName (String,SqliteValue) | Sqlite3BindIndex SqliteValue deriving (Show)

-- bindParam stmt idx (Sqlite3BindIndex (SqliteText t)) =
bindParam stmt idx (Sqlite3BindIndex (SqliteInt i)) = sqlite3_bind_int stmt idx (fromIntegral i)
bindParam stmt idx (Sqlite3BindIndex (SqliteDouble d)) = sqlite3_bind_double stmt idx (realToFrac d)
bindParam stmt _ (Sqlite3BindName (name,val)) = withCString name $ \cname -> do
    idx <- sqlite3_bind_parameter_index stmt cname >>= return . fromIntegral
    bindParam stmt idx (Sqlite3BindIndex val)
bindParams stmt params = zipWithM (bindParam stmt) [1..] params

sqlite3execute :: SQLite3 -> String -> [Sqlite3Bind] -> IO [SQLite3Row]
sqlite3execute conn query params = do
    (stmt,remaining) <- sqlite3Prepare conn query (length query)
    bindParams stmt params
    rcode <- sqlite3Step stmt >>= return . fromIntegral
    count <- sqlite3_column_count stmt >>= return . fromIntegral
    types <- mapM (sqlite3_column_type stmt) [0..(count-1)]
    sqlite3execute' stmt count types rcode

foreign import ccall "wrapper"
  xFuncWrap' :: (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO ()) -> IO (FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO ()))

xFuncWrap fun ctx argc argv = do
  values <- peekArray (fromIntegral argc) argv
  types <- mapM sqlite3_value_type values
  args <- zipWithM extractTypeValue values types
  res <- fun args
  pushResultValue ctx res

sqlite3createScalarFunction conn functionName nArg fun =
    withCString functionName $ \cFunctionName -> do
    let cnarg = fromIntegral $ if isJust nArg then fromJust nArg else -1
        eTextRep = unSQLite3Encoding sqlite_utf8
    xFunc <- xFuncWrap' $ xFuncWrap fun
    sqlite3_create_function_v2 conn cFunctionName cnarg eTextRep nullPtr xFunc nullFunPtr nullFunPtr nullFunPtr


-- foreign import ccall sqlite3_user_data :: SQLite3Context -> IO (Ptr ())
-- xFunc & xStep :: FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO ())
{- sqlite3_context* could be expressed as pCtx (the database connection)
   int could be expressed as nArgs (analogous to argc)
   sqlite3_value** could be expressed as apArgs (analogous to argv) -}
-- xFinal :: FunPtr (SQLite3Context -> IO ())
-- xDestroy :: FunPtr (Ptr () -> IO ())
