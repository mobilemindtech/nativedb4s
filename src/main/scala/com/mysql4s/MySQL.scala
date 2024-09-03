package com.mysql4s

import com.mysql4s.bindings.extern_functions.*
import com.mysql4s.bindings.structs.*
import com.mysql4s.rs.{ResultSet, RowResultSet}
import com.mysql4s.stmt.{PreparedStatement, PreparedStatementImpl}

import java.io.Closeable
import scala.compiletime.uninitialized
import scala.scalanative
import scala.scalanative.unsafe
import scala.scalanative.unsafe.{CChar, CString, CUnsignedLongInt, Ptr, Zone, alloc}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try}
import com.mysql4s.rs.RowResult

import scala.util.Using

type CanThrowZone[T] = Zone ?=> CanThrow[MySqlException] ?=> T

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
  def connectExn(host: String,
                 user: String,
                 password: String,
                 database: String,
                 port: Int = 3306): CanThrowZone[ConnectionExn] =
    connect(host, user, password, database, port) match
      case Success(mysql) => ConnectionExn(mysql)
      case Failure(err) => throw err

class ConnectionExn(conn: Connection) extends Closeable:

  private def unwrap[T](t: Try[T]): CanThrowZone[T] = t match
    case Failure(exception) => throw exception
    case Success(value) => value

  def prepare(query: String): CanThrowZone[PreparedStatement] =
    conn.prepare(query) |> unwrap

  def executeQuery(query: String, args: ScalaTypes*): CanThrowZone[RowResultSet] =
    conn.executeQuery(query, args *) |> unwrap

  def rows(query: String, args: ScalaTypes*): CanThrowZone[Seq[RowResult]] =
    conn.rows(query, args*) |> unwrap

  def close(): Unit =
    conn.close()

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

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A Closeable [[com.mysql4s.rs.RowResultSet]] or a failure
   */
  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet] =
    val stmt = new PreparedStatementImpl(this)
    for
      _ <- stmt.init()
      rows <- stmt.executeQuery(query, args *)
    yield rows

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A seq of results or a failure
   */
  def rows(query: String, args: ScalaTypes*): TryWithZone[Seq[RowResult]] =
    UsingTryInOut(executeQuery(query, args*))(_.toSeq)

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

  /**
   * Execute a prepared statement
   *
   * @param query SQL
   * @param args SQL params
   * @return A first result or None on success case, or a failure
   */
  def firstRow(query: String, args: ScalaTypes*): TryWithZone[Option[RowResult]] =
    UsingTryInOut(executeQuery(query, args*))(_.first)

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

  /**
   * Execute a query
   *
   * @param query SQL
   * @return A Closeable [[com.mysql4s.rs.RowResultSet]] or failure
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
    mysql_close(mysqlPtr)
  private def queryScape(query: String): WithZone[(CString, CUnsignedLongInt)] =
    val ptr = query.c_str()
    val len = query.length
    val scapedSize = 2 * len + 1
    val chunk = alloc[CChar](scapedSize)
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