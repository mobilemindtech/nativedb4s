package com.mysql4s

import com.mysql4s.MySqlException.exn
import com.mysql4s.Statement.collectStmtExn
import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.extern_functions.*
import com.mysql4s.bindings.structs.*

import java.util.Date
import scala.compiletime.uninitialized
import scala.scalanative
import scala.scalanative.unsafe.*
import scala.scalanative.unsafe.Tag.{Ptr as _, *}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try}

//type StmtTypes = String | Int | Long | Short | Double | Float | Boolean | Date

type ZoneUnit = Zone ?=> Unit

trait PreparedStatement extends AutoCloseable:
  def setString(index: Int, value: String | Null): ZoneUnit
  def setShort(index: Int, value: Short | Null): ZoneUnit
  def setInt(index: Int, value: Int | Null): ZoneUnit
  def setLong(index: Int, value: Long | Null): ZoneUnit
  def setFloat(index: Int, value: Float | Null): ZoneUnit
  def setDouble(index: Int, value: Double | Null): ZoneUnit
  def setBoolean(index: Int, value: Boolean | Null): ZoneUnit
  def setDate(index: Int, value: Date | Null): ZoneUnit
  def setDateTime(index: Int, value: Date | Null): ZoneUnit
  def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using TypeConverter[T]): ZoneUnit
  def execute(): TryWithZone[Int]
  def execute(query: String, args: ScalaTypes*): TryWithZone[Int]
  def executeQuery(): TryWithZone[RowResultSet]
  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet]
  def prepare(query: String): TryWithZone[Int]

private[mysql4s] object Statement:
  def collectStmtExn(message: String, stmt: Ptr[MYSQL_STMT]): MySqlException =
    val code = mysql_stmt_errno(stmt)
    val error = mysql_stmt_error(stmt)
    exn(s"$message. Error: ${toStr(error)}", code.toInt)

private[mysql4s] class Statement(mysql: MySql) extends PreparedStatement:

  private var stmtPtr: Ptr[MYSQL_STMT] = uninitialized
  private var bindPtr: Ptr[MYSQL_BIND] = uninitialized

  def init(): Try[Unit] =
    stmtPtr = mysql_stmt_init(mysql.driver())
    if stmtPtr == null
    then Failure(exn("can't init statement"))
    else Success(())

  override def execute(): TryWithZone[Int] =
    for
      _ <- bind()
      _ <- exec()
      affectedRows = mysql_stmt_affected_rows(stmtPtr)
    yield affectedRows.toInt

  override def execute(query: String, args: ScalaTypes*): TryWithZone[Int] =
    for
      count <- prepare(query)
      _ <-  if count != args.length
            then Failure(exn(s"expected ${count} args but has ${args.length}"))
            else Success(bindValues(args.toSeq))
      _ <- bind()
      _ <- exec()
      affectedRows = mysql_stmt_affected_rows(stmtPtr)
    yield affectedRows.toInt

  override def executeQuery(): TryWithZone[RowResultSet] =
    val resultSet = new ResultSet(stmtPtr)
    for
      _ <- bind()
      _ <- exec()
      _ <- resultSet.init()
    yield resultSet

  override def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet] = ???

  override def prepare(query: String): TryWithZone[Int] =
    if mysql_stmt_prepare(stmtPtr, query.c_str(), query.length.toUInt) > 0
    then Failure(collectStmtExn("Failed to prepare statement", stmtPtr))
    else
      val count = paramsCount()
      bindPtr = alloc[MYSQL_BIND](count)
      Success(count)

  private def exec(): Try[Unit] =
    if mysql_stmt_execute(stmtPtr) > 0
    then Failure(collectStmtExn("Failed to execute statement", stmtPtr))
    else Success(())

  override def close(): Unit =
    mysql_stmt_free_result(stmtPtr)
    val _ = mysql_stmt_close(stmtPtr)

  private def getNumFields(res: Ptr[MYSQL_RES]): Int =
    mysql_num_fields(res).toInt

  private def fieldCount(): Int =
    mysql_stmt_field_count(stmtPtr).toInt

  private def paramsCount(): Int =
    mysql_stmt_param_count(stmtPtr).toInt

  override def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using tc: TypeConverter[T]): ZoneUnit =
    val bind = bindPtr(index)
    bind.length = null
    bind.is_null = null
    value match
      case null =>
        val isNull = alloc[CBool]()
        !isNull = true
        bind.is_null = isNull
      case v: String =>
        val lenPtr = alloc[CUnsignedLongInt]()
        val len = v.length.toUInt
        !lenPtr = len
        bind.buffer = v.c_str()
        bind.buffer_length = len
        bind.length = lenPtr
      case v: Short =>
        val ptr = alloc[CShort]()
        !ptr = v
        bind.buffer = ptr
      case v: Int =>
        val ptr = alloc[CInt]()
        !ptr = v
        bind.buffer = ptr
      case v: Long =>
        val ptr = alloc[CLongLong]()
        !ptr = v
        bind.buffer = ptr
      case v: Float =>
        val ptr = alloc[CFloat]()
        !ptr = v
        bind.buffer = ptr
      case v: Double =>
        val ptr = alloc[CDouble]()
        !ptr = v
        bind.buffer = ptr
      case v: Boolean =>
        val ptr = alloc[CBool]()
        !ptr = v
        bind.buffer = ptr
      case _ =>
        println(s"invalid type $value")

    bind.buffer_type = tc.mysqlType

  override def setString(index: Int, value: String | Null): ZoneUnit =
    setAs[String](index, value)

  override def setInt(index: Int, value: Int | Null): ZoneUnit =
    setAs[Int](index, value)

  override def setLong(index: Int, value: Long | Null): ZoneUnit =
    setAs[Long](index, value)

  override def setShort(index: Int, value: Short | Null): ZoneUnit =
    setAs[Short](index, value)

  override def setDouble(index: Int, value: Double | Null): ZoneUnit =
    setAs[Double](index, value)

  override def setFloat(index: Int, value: Float | Null): ZoneUnit =
    setAs[Float](index, value)

  override def setBoolean(index: Int, value: Boolean | Null): ZoneUnit =
    setAs[Boolean](index, value)

  override def setDate(index: Int, value: Date | Null): ZoneUnit = ???

  override def setDateTime(index: Int, value: Date | Null): ZoneUnit = ???

  private def bindValues(values: Seq[ScalaTypes]): ZoneUnit =
    for i <- values.indices do
      values(i) match
        case (s: String) => setAs(i, s)
        case (s: Boolean) => setAs(i, s)
        case (s: Float) => setAs(i, s)
        case (s: Double) => setAs(i, s)
        case (s: Int) => setAs(i, s)
        case (s: Short) => setAs(i, s)
        case (s: Long) => setAs(i, s)
        //case (s: Date) => throw MySqlException("wrong bind type")

  def bind(): Try[Unit] =
    if mysql_stmt_bind_param(stmtPtr, bindPtr)
    then Failure(collectStmtExn("Failed to bind statement", stmtPtr))
    else Success(())
