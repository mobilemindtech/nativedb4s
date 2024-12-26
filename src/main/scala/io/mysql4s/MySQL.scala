package io.mysql4s

import io.mysql4s.bindings.extern_functions.*
import io.mysql4s.bindings.structs.*
import io.mysql4s.rs.{ResultSet, RowResult, RowResultSet}
import io.mysql4s.stmt.{PreparedStatement, PreparedStatementImpl}

import java.io.Closeable
import scala.compiletime.uninitialized
import scala.scalanative
import scala.scalanative.unsafe
import scala.scalanative.unsafe.{
  CChar,
  CString,
  CUnsignedLongInt,
  Ptr,
  Zone,
  alloc
}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try, Using}
//?=> CanThrow[MySqlException]
type CanThrowWithZone[T] = Zone  ?=> T

object MySQL:
  /**
    * Try create new MySQL connection
    *
    * @param host
    * @param user
    * @param password
    * @param database
    * @param port
    * @return A closeable MySQL connection or a failure
    */
  def connect(host: String,
              user: String,
              password: String,
              database: String,
              port: Int = 3306): TryWithZone[Connection] =
    val mysql = new Connection()
    for
      _ <- mysql.connect(host, user, password, database, port)
    yield mysql
  
  /**
    * Try create new MySQL connection
    *
    * @param host
    * @param user
    * @param password
    * @param database
    * @param port
    * @return A closeable MySQL connection or throw a exception
    */
  def unsafeConnect(host: String,
                 user: String,
                 password: String,
                 database: String,
                 port: Int = 3306): CanThrowWithZone[Connection] =
    connect(host, user, password, database, port) match
      case Success(conn) => conn
      case Failure(err) => throw err

/**
 * The MySQL connection
 */
class Connection extends Closeable:

  import MySqlException.exn

  private var mysqlPtr: Ptr[MYSQL] | Null = uninitialized

  private def collectExn(message: String): MySqlException =
    exn(s"$message. Error: ${error()}", errno())

  /**
   * Try connect on MySQL server
   *
   * @param host Hostname
   * @param user Username
   * @param password Password
   * @param database Database to connect
   * @param port Server port
   * @return A Closeable connection or a failure
   */
  private[mysql4s] def connect(host: String,
              user: String,
              password: String,
              database: String,
              port: Int = 3306): TryWithZone[Unit] =
    for
      _ <- init()
      _ <- connect0(host, user, password, database, port)
    yield ()

  /**
   * Prepare Statement
   *
   * @param query SQL
   * @return A Closeable prepared statement or a failure
   */
  def prepare(query: String): TryWithZone[PreparedStatement] =
    val stmt = new PreparedStatementImpl(this)
    for
      _ <- stmt.init()
      _ <- stmt.prepare(query)
    yield stmt

  def unsafePrepare(query: String): CanThrowWithZone[PreparedStatement] =
    prepare(query) match
      case Success(stmt) => stmt
      case Failure(exn) => throw exn

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A Closeable [[io.mysql4s.rs.RowResultSet]] or a failure
   */
  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet] =
    val stmt = new PreparedStatementImpl(this)
    for
      _ <- stmt.init()
      rows <- stmt.executeQuery(query, args *)
    yield rows

  def unsafeExecuteQuery(query: String, args: ScalaTypes*): CanThrowWithZone[RowResultSet] =
    executeQuery(query, args *) match
      case Success(value) => value
      case Failure(ex) => throw ex

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A seq of results or a failure
   */
  def rows(query: String, args: ScalaTypes*): TryWithZone[Seq[RowResult]] =
    UsingTryInOut(executeQuery(query, args*))(_.toSeq)

  def unsafeRows(query: String, args: ScalaTypes*): CanThrowWithZone[Seq[RowResult]] =
    rows(query, args*) match
      case Success(v) => v
      case Failure(ex) => throw ex

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @param f A map function
   * @return A seq of mapped results or a failure
   */
  def rows[T](query: String, args: ScalaTypes*)(f: RowResult => T): TryWithZone[Seq[T]] =
    UsingTryInOut(executeQuery(query, args*))(_.map(f))

  def unsafeRows[T](query: String, args: ScalaTypes*)(f: RowResult => T): CanThrowWithZone[Seq[T]] =
    rows(query, args*)(f) match
      case Success(v) => v
      case Failure(exception) => throw exception

  def rowsAs[T](query: String, args: ScalaTypes*): CanThrowWithZone[Seq[QueryResult[T]]] =
    unsafeRows(query, args *).map(_.getAsQueryResult[T])

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A first result or None on success case, or a failure
   */
  def firstRow(query: String, args: ScalaTypes*): TryWithZone[Option[RowResult]] =
    UsingTryInOut(executeQuery(query, args*))(_.first)

  def unsafeFirstRow(query: String, args: ScalaTypes*): CanThrowWithZone[Option[RowResult]] =
    firstRow(query, args*) match
      case Success(v) => v
      case Failure(ex) => throw ex

  def firstRowAs[T](query: String, args: ScalaTypes*): CanThrowWithZone[Option[QueryResult[T]]] =
    unsafeFirstRow(query, args*).map(_.getAsQueryResult[T])

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @param f A map function
   * @return A mapped first result or None on success case, or a failure
   */
  def firstRow[T](query: String, args: ScalaTypes*)(f: RowResult => T): TryWithZone[Option[T]] =
    UsingTryInOut(executeQuery(query, args*))(_.firstMap(f))

  def unsafeFirstRow[T](query: String, args: ScalaTypes*)(f: RowResult => T): CanThrowWithZone[Option[T]] =
    firstRow(query, args*)(f) match
      case Success(v) => v
      case Failure(ex) => throw ex


  /**
   * Execute a non query prepared statement
   *
   * @param query SQL
   * @param args SQL args
   * @return Affecteds row count or a failure
   */
  def execute(query: String, args: ScalaTypes*): TryWithZone[Int] =
    UsingTryOut(new PreparedStatementImpl(this)):
      stmt =>
        for
          _ <- stmt.init()
          affectedRows <- stmt.execute(query, args *)
        yield affectedRows

  def unsafeExecute(query: String, args: ScalaTypes*): CanThrowWithZone[Int] =
    execute(query, args*) match
      case Success(v) => v
      case Failure(ex) => throw ex

  /**
   * Execute a query
   *
   * @param query SQL
   * @return A Closeable [[io.mysql4s.rs.RowResultSet]] or failure
   */
  def realExecuteQuery(query: String): TryWithZone[RowResultSet] =
    val (scapedQuery, len) = queryScape(query)
    if mysql_real_query(mysqlPtr, scapedQuery, len) > 0
    then Failure(collectExn("Failed to execute query"))
    else
      val result = mysql_store_result(mysqlPtr)
      val rs = new ResultSet(result)
      rs.init()
      Success(rs)

  /**
   * Execute a non query
   *
   * @param query
   * @return Affected rows count or a failure
   */
  def realExecute(query: String): TryWithZone[Int] =
    val (scapedQuery, len) = queryScape(query)
    if mysql_real_query(mysqlPtr, scapedQuery, len) == 0
    then Success(affectedRows())
    else Failure(collectExn("Failed to execute query"))

  /**
   * Last generated ID
   *
   * @return
   */
  def lastInsertID: Int =
    mysql_insert_id(mysqlPtr).toInt

  /**
   * Affecteds rows count
   *
   * @return
   */
  def affectedRows(): Int =
    mysql_affected_rows(mysqlPtr).toInt

  /**
   * Set connection auto-commit. Default is true
   *
   * @param mode
   * @return
   */
  def setAutoCommit(mode: Boolean): Try[Unit] =
    if mysql_autocommit(mysqlPtr, mode)
    then Failure(collectExn("Failed to set autocommit"))
    else Success(())

  /**
   * MySQL error
   *
   * @return
   */
  def error(): String =
    mysql_error(mysqlPtr) |> toStr

  /**
   * MySQL errorno
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

  private def queryScape(query: String): CanThrowWithZone[(CString, CUnsignedLongInt)] =
    val ptr = query.c_str()
    val len = query.length
    val scapeSize = 2 * len + 1
    val chunk = alloc[CChar](scapeSize)
    val newLen = mysql_real_escape_string(mysqlPtr, chunk, ptr, len.toUInt)
    (chunk, newLen)

  private def init(): TryWithZone[Unit] =
    mysqlPtr = mysql_init(mysqlPtr)
    if mysqlPtr == null
    then Failure(collectExn("Failed to connect database"))
    else Success(())

  private def connect0(host: String,
                       user: String,
                       password: String,
                       database: String,
                       port: Int = 3306): TryWithZone[Unit] =

    mysql_real_connect(
      mysqlPtr,
      host.c_str(),
      user.c_str(),
      password.c_str(),
      database.c_str(),
      port.toUShort,
      null,
      0.toUSize) match
      case null => Failure(collectExn("Failed to connect database"))
      case _ => Success(())    