import SQLite3

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Bits
import Data.Typeable

xCreate :: SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt
xCreate conn app argc argv vtab name = do
    return $ fromIntegral 1
-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
foreign import ccall "wrapper"
  wrap :: ( SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt) -> IO (FunPtr ( SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt))
modtest = do
    cr <- wrap xCreate
    return $ SQLite3Module {
    sqlite3_module_iVersion=2,
    sqlite3_module_xCreate=cr
--     sqlite3_module_xConnect     :: FunPtr (SQLite3 -> Ptr () -> CInt -> Ptr CString -> Ptr SQLite3VTab -> Ptr CString -> IO CInt),
--     sqlite3_module_xBestIndex   :: FunPtr (SQLite3VTab -> SQLite3IndexInfo -> IO CInt),
--     sqlite3_module_xDisconnect  :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xDestroy     :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xOpen        :: FunPtr (SQLite3VTab -> Ptr SQLite3VTabCursor -> IO CInt),
--     sqlite3_module_xClose       :: FunPtr (SQLite3VTabCursor -> IO CInt),
--     sqlite3_module_xFilter      :: FunPtr (SQLite3VTabCursor -> CInt -> CString -> Ptr SQLite3Value -> IO CInt),
--     sqlite3_module_xNext        :: FunPtr (SQLite3VTabCursor -> IO CInt),
--     sqlite3_module_xEof         :: FunPtr (SQLite3VTabCursor -> IO CInt),
--     sqlite3_module_xColumn      :: FunPtr (SQLite3VTabCursor -> SQLite3Context -> CInt -> IO CInt),
--     sqlite3_module_xRowid       :: FunPtr (SQLite3VTabCursor -> Ptr CLong -> IO CInt),
--     sqlite3_module_xUpdate      :: FunPtr (SQLite3VTab -> CInt -> Ptr SQLite3Value -> Ptr CLong -> IO CInt),
--     sqlite3_module_xBegin       :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xSync        :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xCommit      :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xRollback    :: FunPtr (SQLite3VTab -> IO CInt),
--     sqlite3_module_xFindFunction :: FunPtr (SQLite3VTab -> CInt -> CString -> FunPtr (SQLite3Context -> CInt -> Ptr SQLite3Value -> IO (Ptr ())) -> Ptr (Ptr ()) -> IO CInt),
--     sqlite3_module_xRename      :: FunPtr (SQLite3VTab -> CString -> IO CInt),
--     sqlite3_module_xSavepoint   :: FunPtr (SQLite3VTab -> CInt -> IO CInt),
--     sqlite3_module_xRelease     :: FunPtr (SQLite3VTab -> CInt -> IO CInt),
--     sqlite3_module_xRollbackTo  :: FunPtr (SQLite3VTab -> CInt -> IO CInt)
    }

funTest [SqliteDouble d1,SqliteDouble d2] = return $ SqliteDouble (d1+d2)
main = do
  conn2 <- sqlite3Open ":memory:" [sqlite_open_readwrite] Nothing
  sqlite3createScalarFunction conn2 "funtest" (Just 2) funTest
  rows <- sqlite3execute conn2 "SELECT funTest(9.0,?),funTest(1.4,7.0);" [Sqlite3BindIndex (SqliteDouble 1.5)]
--   conn <- sqlite3Open "/home/bob/nas/data/finance/edgar/master_index.db" [sqlite_open_readwrite] Nothing
--   rows <- sqlite3execute conn "SELECT * FROM edgar_index" []
  putStrLn $ show $ rows
  --sqlite3Close conn
  sqlite3Close conn2

-- nullFunPtr
-- foreign import ccall sqlite3_create_module_v2 :: SQLite3 -> CString -> SQLite3ModulePtr -> Ptr () -> FunPtr (SQLite3VTab -> IO CInt) -> IO CInt
