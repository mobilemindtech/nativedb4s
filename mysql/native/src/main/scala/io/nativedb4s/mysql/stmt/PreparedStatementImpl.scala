package io.nativedb4s.mysql.stmt

import io.nativedb4s.api.rs.RowResultSet
import io.nativedb4s.api.stmt.PreparedStatement
import io.nativedb4s.api.types.exn
import io.nativedb4s.api.util.{QueryResult, ScalaTypes}
import io.nativedb4s.mysql.bindings.enumerations.enum_field_types
import io.nativedb4s.mysql.bindings.extern_functions.*
import io.nativedb4s.mysql.bindings.structs.{MYSQL, MYSQL_BIND, MYSQL_STMT}
import io.nativedb4s.mysql.types.TypeConverter
import io.nativedb4s.mysql.types.TypeConverter.given
import io.nativedb4s.mysql.util.*

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.compiletime.uninitialized
import scala.scalanative.unsafe.{CBool, CInt, Ptr, Zone, alloc}
import scala.scalanative.unsigned.UnsignedRichInt

extension (p: PreparedStatement)
  def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using
      tc: TypeConverter[T],
      z: Zone
  ): PreparedStatement =
    val bind = p.asInstanceOf[PreparedStatementImpl].bindPtr(index)
    bind.length = null
    bind.is_null = null
    value match
      case null =>
        val isNull = alloc[CBool]()
        !isNull = true
        bind.is_null = isNull
        bind.buffer_type = enum_field_types.MYSQL_TYPE_NULL
      case v =>

        tc.bindValue(v.asInstanceOf[T], bind)
    p

class PreparedStatementImpl(using Zone) extends PreparedStatement:

  private var stmtPtr: Ptr[MYSQL_STMT] = uninitialized
  private[stmt] var bindPtr: Ptr[MYSQL_BIND] = uninitialized

  def this(driver: Ptr[MYSQL])(using Zone) =
    this()
    stmtPtr = mysql_stmt_init(driver)
    if stmtPtr == null
    then throw exn("can't init statement")

  override def execute(): Int =
    bind()
    exec()
    val affectedRows = mysql_stmt_affected_rows(stmtPtr)
    affectedRows.toInt

  override def execute(query: String, args: ScalaTypes*): Int =
    val count = prepare(query)
    if count != args.length
    then throw exn(s"expected $count args but has ${args.length}")
    bindValues(args.toSeq)
    bind()
    exec()
    val affectedRows = mysql_stmt_affected_rows(stmtPtr)
    affectedRows.toInt

  override def executeQuery(): RowResultSet =
    val resultSet = new RowResultSetStmtImpl(stmtPtr)
    bind()
    exec()
    resultSet.init()
    resultSet

  override def executeQueryAs[T](): Seq[QueryResult[T]] =
    executeQuery().map(_.getAsQueryResult[T])

  override def executeQuery(query: String, args: ScalaTypes*): RowResultSet =
    val count = prepare(query)
    if count != args.length
    then throw exn(s"Query require $count args, but has ${args.length}")
    bindValues(args)
    executeQuery()

  override def executeQueryAs[T](
      query: String,
      args: ScalaTypes*
  ): Seq[QueryResult[T]] =
    executeQuery(query, args*).map(_.getAsQueryResult[T])

  override def prepare(query: String): Int =
    if mysql_stmt_prepare(stmtPtr, query.c_str(), query.length.toUInt) > 0
    then throw collectStmtExn("Failed to prepare statement", stmtPtr)
    else
      val count = paramsCount()
      bindPtr = alloc[MYSQL_BIND](count)
      count

  private def exec(): Unit =
    if mysql_stmt_execute(stmtPtr) > 0
    then throw collectStmtExn("Failed to execute statement", stmtPtr)

  override def close(): Unit =
    mysql_stmt_free_result(stmtPtr)
    val _ = mysql_stmt_close(stmtPtr)

  private def paramsCount(): Int =
    mysql_stmt_param_count(stmtPtr).toInt

  override def setString(index: Int, value: String | Null): PreparedStatement =
    this.setAs[String](index, value)

  override def setInt(index: Int, value: Int | Null): PreparedStatement =
    this.setAs[Int](index, value)

  override def setLong(index: Int, value: Long | Null): PreparedStatement =
    this.setAs[Long](index, value)

  override def setShort(index: Int, value: Short | Null): PreparedStatement =
    this.setAs[Short](index, value)

  override def setDouble(index: Int, value: Double | Null): PreparedStatement =
    this.setAs[Double](index, value)

  override def setFloat(index: Int, value: Float | Null): PreparedStatement =
    this.setAs[Float](index, value)

  override def setBoolean(
      index: Int,
      value: Boolean | Null
  ): PreparedStatement =
    this.setAs[Boolean](index, value)

  override def setTime(index: Int, value: LocalTime | Null): PreparedStatement =
    this.setAs[LocalTime](index, value)

  override def setDate(index: Int, value: LocalDate | Null): PreparedStatement =
    this.setAs[LocalDate](index, value)

  override def setDateTime(
      index: Int,
      value: LocalDateTime | Null
  ): PreparedStatement =
    this.setAs[LocalDateTime](index, value)

  override def setBytes(
      index: CInt,
      value: Array[Byte] | Null
  ): PreparedStatement =
    this.setAs[Array[Byte]](index, value)

  override def lastInsertID: Int =
    mysql_stmt_insert_id(stmtPtr).toInt

  private def bindValues(values: Seq[ScalaTypes]): Unit =
    for i <- values.indices do
      values(i) match
        case s: String        => this.setAs[String](i, s)
        case s: Boolean       => this.setAs[Boolean](i, s)
        case s: Float         => this.setAs[Float](i, s)
        case s: Double        => this.setAs[Double](i, s)
        case s: Int           => this.setAs[Int](i, s)
        case s: Short         => this.setAs[Short](i, s)
        case s: Long          => this.setAs[Long](i, s)
        case s: Array[Byte]   => this.setAs[Array[Byte]](i, s)
        case s: LocalTime     => this.setAs[LocalTime](i, s)
        case s: LocalDate     => this.setAs[LocalDate](i, s)
        case s: LocalDateTime => this.setAs[LocalDateTime](i, s)

  private def bind(): Unit =
    if mysql_stmt_bind_param(stmtPtr, bindPtr)
    then throw collectStmtExn("Failed to bind statement", stmtPtr)
    else ()
