package com.mysql4s.stmt

import com.mysql4s.MySqlException.exn
import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.extern_functions.*
import com.mysql4s.bindings.structs.{MYSQL_BIND, MYSQL_STMT}
import com.mysql4s.rs.{RowResultSet, StatementResultSet}
import com.mysql4s.types.TypeConverter
import com.mysql4s.*

import scala.compiletime.uninitialized
import scala.scalanative.unsafe.{CBool, CInt, Ptr, alloc}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try}

private[mysql4s] class PreparedStatementImpl(mysql: Connection) extends PreparedStatement:

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
    val resultSet = new StatementResultSet(stmtPtr)
    for
      _ <- bind()
      _ <- exec()
      _ <- resultSet.init()
    yield resultSet

  override def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet] =
    for
      count <- prepare(query)
      _ <-
        if count != args.length
        then Failure(MySqlException.exn(s"Query require ${count} args, but has ${args.length}"))
        else Success(bindValues(args))
      rs <- executeQuery()
    yield rs

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

  private def paramsCount(): Int =
    mysql_stmt_param_count(stmtPtr).toInt

  override def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using tc: TypeConverter[T]): WithZone[PreparedStatement] =
    val bind = bindPtr(index)
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
    this

  override def setString(index: Int, value: String | Null): WithZone[PreparedStatement] =
    setAs[String](index, value)

  override def setInt(index: Int, value: Int | Null): WithZone[PreparedStatement] =
    setAs[Int](index, value)

  override def setLong(index: Int, value: Long | Null): WithZone[PreparedStatement] =
    setAs[Long](index, value)

  override def setShort(index: Int, value: Short | Null): WithZone[PreparedStatement] =
    setAs[Short](index, value)

  override def setDouble(index: Int, value: Double | Null): WithZone[PreparedStatement] =
    setAs[Double](index, value)

  override def setFloat(index: Int, value: Float | Null): WithZone[PreparedStatement] =
    setAs[Float](index, value)

  override def setBoolean(index: Int, value: Boolean | Null): WithZone[PreparedStatement] =
    setAs[Boolean](index, value)

  override def setTime(index: Int, value: MysqlTime | Null): WithZone[PreparedStatement] =
    setAs[MysqlTime](index, value)

  override def setDate(index: Int, value: MysqlDate | Null): WithZone[PreparedStatement] =
    setAs[MysqlDate](index, value)

  override def setDateTime(index: Int, value: MysqlDateTime | Null): WithZone[PreparedStatement] =
    setAs[MysqlDateTime](index, value)

  override def setTimestamp(index: Int, value: MysqlTimestamp | Null): WithZone[PreparedStatement] =
    setAs[MysqlTimestamp](index, value)

  override def setBytes(index: CInt, value: Array[Byte] | Null): WithZone[PreparedStatement] =
    setAs[Array[Byte]](index, value)

  override def lastInsertID: Int =
    mysql_stmt_insert_id(stmtPtr).toInt

  private def bindValues(values: Seq[ScalaTypes]): WithZone[Unit] =
    for i <- values.indices do
      values(i) match
        case s: String => setAs[String](i, s)
        case s: Boolean => setAs[Boolean](i, s)
        case s: Float => setAs[Float](i, s)
        case s: Double => setAs[Double](i, s)
        case s: Int => setAs[Int](i, s)
        case s: Short => setAs[Short](i, s)
        case s: Long => setAs[Long](i, s)
        case s: Array[Byte] => setAs[Array[Byte]](i, s)
        case s: MysqlTime => setAs[MysqlTime](i, s)
        case s: MysqlDate => setAs[MysqlDate](i, s)
        case s: MysqlDateTime => setAs[MysqlDateTime](i, s)
        case s: MysqlTimestamp => setAs[MysqlTimestamp](i, s)

  private def bind(): Try[Unit] =
    if mysql_stmt_bind_param(stmtPtr, bindPtr)
    then Failure(collectStmtExn("Failed to bind statement", stmtPtr))
    else Success(())

