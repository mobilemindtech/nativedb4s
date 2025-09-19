package io.nativedb4s.mysql.conn

import io.nativedb4s.mysql.bindings.extern_functions.*
import io.nativedb4s.mysql.bindings.structs.MYSQL
import io.nativedb4s.api.conn.Connection
import io.nativedb4s.api.rs.{RowResult, RowResultSet}
import io.nativedb4s.api.stmt.PreparedStatement
import io.nativedb4s.api.types.{MySqlException, exn}
import io.nativedb4s.api.util.{QueryResult, ScalaTypes, |>}
import io.nativedb4s.mysql.util.*
import io.nativedb4s.mysql.rs.RowResultSetImpl
import io.nativedb4s.mysql.stmt.PreparedStatementImpl

import scala.compiletime.uninitialized
import scala.scalanative.unsafe.{CChar, CString, CUnsignedLongInt, Ptr, Zone, alloc}
import scala.scalanative.unsigned.UnsignedRichInt

/**
 * The MySQL connection
 */
class ConnectionImpl(using Zone) extends Connection:

  private var mysqlPtr: Ptr[MYSQL] | Null = uninitialized

  private def collectExn(message: String): MySqlException =
    exn(s"$message. Error: ${error()}", errno())

  /**
   * Try connect on MySQL server
   *
   * @param host     Hostname
   * @param user     Username
   * @param password Password
   * @param database Database to connect
   * @param port     Server port
   * @return A Closeable connection
   */
  def connect(host: String,
              user: String,
              password: String,
              database: String,
              port: Int = 3306): Unit =
    init()
    connect0(host, user, password, database, port)

  /**
   * Prepare Statement
   *
   * @param query SQL
   * @return A Closeable prepared statement
   */
  def prepare(query: String): PreparedStatement =
    val stmt = new PreparedStatementImpl(mysqlPtr)
    stmt.prepare(query)
    stmt

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args  SQL params
   * @return A Closeable [[RowResultSet]]
   */
  def executeQuery(query: String, args: ScalaTypes*): RowResultSet =
    val stmt = new PreparedStatementImpl(mysqlPtr)
    stmt.executeQuery(query, args *)

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args  SQL params
   * @return A seq of results
   */
  def rows(query: String, args: ScalaTypes*): Seq[RowResult] =
    val rs = executeQuery(query, args *)
    try rs.toSeq finally rs.close()


  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args  SQL params
   * @param f     A map function
   * @return A seq of mapped results
   */
  def rows[T](query: String, args: ScalaTypes*)(f: RowResult => T): Seq[T] =
    val rs = executeQuery(query, args *)
    try rs.map(f) finally rs.close()


  def rowsAs[T](query: String, args: ScalaTypes*): Seq[QueryResult[T]] =
    rows(query, args *).map(_.getAsQueryResult[T])
  
  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args  SQL params
   * @return A first result or None
   */
  def firstRow(query: String, args: ScalaTypes*): Option[RowResult] =
    val rs = executeQuery(query, args *)
    try
      rs.first
    finally rs.close()

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args  SQL params
   * @param f     A map function
   * @return A mapped first result or None
   */
  def firstRow[T](query: String, args: ScalaTypes*)(f: RowResult => T): Option[T] =
    val rs = executeQuery(query, args *)
    try rs.firstMap(f) finally rs.close()

  def firstRowAs[T](query: String, args: ScalaTypes*): Option[QueryResult[T]] =
    firstRow(query, args *).map(_.getAsQueryResult[T])

  /**
   * Execute a non query prepared statement
   *
   * @param query SQL
   * @param args  SQL args
   * @return Affected row count
   */
  def execute(query: String, args: ScalaTypes*): Int =
    val stmt = new PreparedStatementImpl(mysqlPtr)
    try stmt.execute(query, args *) finally stmt.close()

  /**
   * Execute a query
   *
   * @param query SQL
   * @return A Closeable [[RowResultSet]]
   */
  def realExecuteQuery(query: String): RowResultSet =
    val (scapedQuery, len) = queryScape(query)
    if mysql_real_query(mysqlPtr, scapedQuery, len) > 0
    then throw collectExn("Failed to execute query")
    else
      val result = mysql_store_result(mysqlPtr)
      new RowResultSetImpl(result)

  /**
   * Execute a non query
   *
   * @param query SQL
   * @return Affected rows count
   */
  def realExecute(query: String): Int =
    val (scapedQuery, len) = queryScape(query)
    if mysql_real_query(mysqlPtr, scapedQuery, len) == 0
    then affectedRows
    else throw collectExn("Failed to execute query")

  /**
   * Last generated ID
   *
   * @return
   */
  def lastInsertID: Int =
    mysql_insert_id(mysqlPtr).toInt

  /**
   * Affected rows count
   *
   * @return
   */
  def affectedRows: Int =
    mysql_affected_rows(mysqlPtr).toInt

  /**
   * Set connection auto-commit. Default is true
   *
   * @param mode true to auto commit
   * @return
   */
  def setAutoCommit(mode: Boolean): Unit =
    if mysql_autocommit(mysqlPtr, mode)
    then throw collectExn("Failed to set autocommit")
    else ()

  /**
   * MySQL transaction rollback
    */
  def commit(): Unit =
    if mysql_commit(mysqlPtr)
    then throw collectExn("Failed to commit")
    else ()

  /**
   * MySQL transaction commit
   */
  def rollback(): Unit =
    if mysql_rollback(mysqlPtr)
    then throw collectExn("Failed to rollback")
    else ()

  /**
   * MySQL error description
   *
   * @return
   */
  def error(): String =
    mysql_error(mysqlPtr) |> toStr

  /**
   * MySQL error code
   *
   * @return
   */
  def errno(): Int =
    mysql_errno(mysqlPtr).toInt

  /**
   * MySQL info
   *
   * @return
   */
  def info(): String =
    mysql_info(mysqlPtr) |> toStr

  /**
   * MySQL client info
   *
   * @return
   */
  def clientInfo(): String =
    mysql_get_client_info() |> toStr

  /**
   * MySQL Native Ptr
   *
   * @return
   */
  def driver(): Ptr[MYSQL] = mysqlPtr

  /**
   * Close connection
   */
  override def close(): Unit =
    if mysqlPtr != null
    then
      mysql_close(mysqlPtr)
      mysqlPtr = null

  private def queryScape(query: String): (CString, CUnsignedLongInt) =
    val ptr = query.c_str()
    val len = query.length
    val scapeSize = 2 * len + 1
    val chunk = alloc[CChar](scapeSize)
    val newLen = mysql_real_escape_string(mysqlPtr, chunk, ptr, len.toUInt)
    (chunk, newLen)

  private def init(): Unit =
    mysqlPtr = mysql_init(mysqlPtr)
    if mysqlPtr == null
    then throw collectExn("Failed to connect database")
    else ()

  private def connect0(host: String,
                       user: String,
                       password: String,
                       database: String,
                       port: Int): Unit =

    mysql_real_connect(
      mysqlPtr,
      host.c_str(),
      user.c_str(),
      password.c_str(),
      database.c_str(),
      port.toUShort,
      null,
      0.toUSize) match
      case null => throw collectExn("Failed to connect database")
      case _ => ()