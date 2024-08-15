package com.mysql4s

import com.mysql4s.bindings.extern_functions.*
import com.mysql4s.bindings.structs.*

import java.io.Closeable
import scala.compiletime.uninitialized
import scala.scalanative
import scala.scalanative.unsafe
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try}

object MySQL:
  def connect(host: String,
              user: String,
              password: String,
              database: String,
              port: Int = 3306): TryWithZone[Connection] =
    val mysql = new Connection()
    for
      _ <- mysql.connect(host, user, password, database, port)
    yield mysql

  def connectExn(host: String,
                 user: String,
                 password: String,
                 database: String,
                 port: Int = 3306): WithZone[Connection] =
    connect(host, user, password, database, port) match
      case Success(mysql) => mysql
      case Failure(err) => throw err

class Connection extends Closeable:

  import MySqlException.exn

  private var mysqlPtr: Ptr[MYSQL] | Null = uninitialized

  private def collectExn(message: String): MySqlException =
    exn(s"$message. Error: ${error()}", errno())

  def connect(host: String,
                               user: String,
                               password: String,
                               database: String,
                               port: Int = 3306): TryWithZone[Unit] =
    mysqlPtr = mysql_init(mysqlPtr)
    if mysqlPtr == null 
    then Failure(collectExn("Failed to connect database"))
    else
        val res = mysql_real_connect(
          mysqlPtr,
          host.c_str(),
          user.c_str(),
          password.c_str(),
          database.c_str(),
          port.toUShort,
          null,
          0.toUSize)
        if res == null
        then Failure(collectExn("Failed to connect database"))
        else Success(())

  def prepare(query: String): TryWithZone[PreparedStatement] =
    val stmt = new Statement(this)
    for
      _ <- stmt.init()
      _ <- stmt.prepare(query)
    yield stmt

  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet] =
    val stmt = new Statement(this)
    for
      _ <- stmt.init()
      rows <- stmt.executeQuery(query, args *)
    yield rows

  def execute(query: String, args: ScalaTypes*): TryWithZone[Int] =
    val stmt = new Statement(this)
    try
      for 
        _ <- stmt.init()
        affectedRows <- stmt.execute(query, args *)
      yield affectedRows
    finally 
      stmt.close()  

  def realQuery(query: String): TryWithZone[RowResultSet] =
    if mysql_real_query(mysqlPtr, query.c_str(), query.length.toUSize) > 0
    then Failure(collectExn("Failed to execute query"))
    else
      val result = mysql_store_result(mysqlPtr)
      val rs = new ResultSet(result)
      rs.init()
      Success(rs)


  def realQueryExec(query: String): TryWithZone[Int] =
    if mysql_real_query(mysqlPtr, query.c_str(), query.length.toUSize) == 0
    then Success(affectedRows())
    else Failure(collectExn("Failed to execute query"))

  def affectedRows(): Int =
    mysql_affected_rows(mysqlPtr).toInt

  def fieldCount(): Int =
    mysql_field_count(mysqlPtr).toInt

  def setAutoCommit(mode: Boolean): Try[Unit] =
    if mysql_autocommit(mysqlPtr, mode)
    then Failure(collectExn("Failed to set autocommit"))
    else Success(())

  def error(): String =
    mysql_error(mysqlPtr) |> toStr

  def errno(): Int =
    mysql_errno(mysqlPtr).toInt

  def info(): String =
      mysql_info(mysqlPtr) |> toStr

  def clientInfo(): String =
    mysql_get_client_info() |> toStr

  def driver(): Ptr[MYSQL] = mysqlPtr

  override def close(): Unit =
    mysql_close(mysqlPtr)
