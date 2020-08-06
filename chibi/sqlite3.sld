(define-library (chibi sqlite3)
  (export
   ;; constants
   SQLITE_ABORT SQLITE_AUTH SQLITE_BUSY SQLITE_CANTOPEN SQLITE_CONSTRAINT
   SQLITE_CORRUPT SQLITE_DONE SQLITE_EMPTY SQLITE_ERROR SQLITE_FORMAT
   SQLITE_FULL SQLITE_INTERNAL SQLITE_INTERRUPT SQLITE_IOERR SQLITE_LOCKED
   SQLITE_MISMATCH SQLITE_MISUSE SQLITE_NOLFS SQLITE_NOMEM SQLITE_NOTADB
   SQLITE_NOTFOUND SQLITE_NOTICE SQLITE_OK SQLITE_PERM SQLITE_PROTOCOL
   SQLITE_RANGE SQLITE_READONLY SQLITE_ROW SQLITE_SCHEMA SQLITE_TOOBIG
   SQLITE_WARNING
   SQLITE_INTEGER SQLITE_FLOAT SQLITE_TEXT SQLITE_BLOB SQLITE_NULL
   ;; basic api
   sqlite3-open sqlite3-errmsg sqlite3-exec sqlite3-prepare
   sqlite3-reset sqlite3-step
   sqlite3-bind-int sqlite3-bind-double sqlite3-bind-text
   sqlite3-column-count sqlite3-column-type sqlite3-column-int
   sqlite3-column-double sqlite3-column-text sqlite3-column-bytes
   ;; high-level utilities
   sqlite3-bind sqlite3-bind-all sqlite3-column sqlite3-columns
   sqlite3-select sqlite3-get sqlite3-do sqlite3-fold
   ;; ssql
   ssql->sql sqlite3-lambda sqlite3-loop
   )
  (import (scheme base) (scheme write) (srfi 130))
  (include-shared "sqlite3")
  (include "sqlite3.scm"))
