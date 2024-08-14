package com.mysql4s

import com.mysql4s.Statement.collectStmtExn
import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.extern_functions.*
import com.mysql4s.bindings.structs.{MYSQL_BIND, MYSQL_RES, MYSQL_STMT, MYSQL_TIME}

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.scalanative.unsafe.{CBool, CChar, CDouble, CFloat, CInt, CLongLong, CShort, CString, CUnsignedLongInt, CVoidPtr, Ptr, alloc}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.{Failure, Success, Try}

trait RowResult:
  def getString(index: Int | String): Option[String]
  def getInt(index: Int | String): Option[Int]
  def getShort(index: Int | String): Option[Short]
  def getLong(index: Int | String): Option[Long]
  def getFloat(index: Int | String): Option[Float]
  def getDouble(index: Int | String): Option[Double]
  def getBoolean(index: Int | String): Option[Boolean]
  def getAs[T <: ScalaTypes](index: Int | String)(using TypeConverter[T]): Option[T]

trait RowResultSet extends AutoCloseable:
  def count: Int
  def hasNext: Boolean
  def next(): TryWithZone[Option[RowResult]]

private[mysql4s] case class Column(name: String,
                  index: Int,
                  typ: enum_field_types,
                  isNull: Boolean = false,
                  error: Boolean = false,
                  length: Int = 0,
                  ptr: CVoidPtr = null)

class Result(columns: Seq[Column]) extends RowResult:

  private def col(index: Int | String): Option[Column] =
    index match
      case i: Int => columns.lift(i)
      case s: String => columns.find(_.name == s)

  def getAs[T <: ScalaTypes](index: Int | String)(using nc: TypeConverter[T]): Option[T] =
    col(index) match
      case Some(col) => nc.fromNative(col.ptr) |> Some.apply
      case None => None

  def getString(index: Int | String): Option[String] = getAs[String](index)

  def getShort(index: Int | String): Option[Short] = getAs[Short](index)

  def getInt(index: Int | String): Option[Int] = getAs[Int](index)

  def getLong(index: Int | String): Option[Long] = getAs[Long](index)

  def getFloat(index: Int | String): Option[Float] = getAs[Float](index)

  def getDouble(index: Int | String): Option[Double] = getAs[Double](index)

  def getBoolean(index: Int | String): Option[Boolean] = getAs[Boolean](index)


class ResultSet(stmtPtr: Ptr[MYSQL_STMT]) extends RowResultSet:

  private var resPtr: Ptr[MYSQL_RES] = uninitialized

  private var bindsPtr: Ptr[MYSQL_BIND] = uninitialized
  private var isNullPtr: Ptr[CBool] = uninitialized
  private var errorPtr: Ptr[CBool] = uninitialized
  private var lengthPtr: Ptr[CUnsignedLongInt] = uninitialized

  private val columns = mutable.ListBuffer[Column]()

  private var numRows: Int = 0
  private var numFields: Int = 0
  private var currRowIndex: Int = 0

   def init(): TryWithZone[Unit] =
    for
      _ <- getMetadataResult()
      _ = bindFields()
      _ = initBinds()
      _ <- bindResult()
      _ <- storeResult()
      _ = getNumRows()
    yield ()

  private def bindFields(): ZoneUnit =
    numFields = mysql_stmt_field_count(stmtPtr).toInt
    bindsPtr = alloc[MYSQL_BIND](numFields)

  private def getNumRows(): Unit =
    numRows = mysql_stmt_num_rows(stmtPtr).toInt

  private def initBinds(): ZoneUnit =
    val res = !resPtr
    val fieldsPtr = res.fields
    isNullPtr = alloc[CBool](numFields)
    errorPtr = alloc[CBool](numFields)
    lengthPtr = alloc[CUnsignedLongInt](numFields)
    for i <- 0 until numFields do
      val field = fieldsPtr(i)
      val bind = bindsPtr(i)
      val typ = field.`type`
      bind.buffer_type = typ
      bind.is_null = isNullPtr + i
      bind.length = lengthPtr + i
      bind.error = errorPtr + i
      bind.buffer = allocBufferType(typ)
      bind.buffer_length = 0.toUInt
      val col = Column(toStr(field.name), i, typ)
      columns.append(col)

  def count: Int = numRows

  def hasNext: Boolean = currRowIndex < numRows

  def next(): TryWithZone[Option[Result]] =
    if hasNext
    then next0()
    else Success(None)

  private def next0(): TryWithZone[Option[Result]] =
    mysql_stmt_fetch(stmtPtr) match
      case 0 | 101 => // 101 = MYSQL_DATA_TRUNCATED
        val cols = mutable.ListBuffer[Column]()
        for col <- columns do
          val valPtr =
            if isCString(col.typ)
            then allocColumnString(col)
            else bindsPtr(col.index).buffer

          cols.append(
            Column(
              name = col.name,
              index = col.index,
              typ = col.typ,
              isNull = isNullPtr(col.index),
              length = lengthPtr(col.index).toInt,
              error = errorPtr(col.index),
              ptr = valPtr))

        currRowIndex += 1
        Result(cols.toSeq) |> Some |> Success
      case 100 => // MYSQL_NO_DATA
        Success(None)
      case 1 =>
        Failure(collectStmtExn("Failed to get statement result metadata", stmtPtr))
      case v =>
        println(s"result?=$v, ${collectStmtExn("Failed to store statement result", stmtPtr)}")
        Failure(collectStmtExn("Failed to fetch statement", stmtPtr))

  private def allocColumnString(col: Column): WithZone[CVoidPtr] =
    val realLen = lengthPtr(col.index).toInt + 1
    val buffer = alloc[CChar](realLen)
    val bind = bindsPtr + col.index
    (!bind).buffer = buffer
    (!bind).buffer_length = realLen.toUInt
    if mysql_stmt_fetch_column(stmtPtr, bind, col.index.toUInt, 0.toUInt) > 0
    then println(s"${collectStmtExn("error to fetch column", stmtPtr)}")
    buffer(realLen - 1) = '\u0000'.toByte
    buffer

  private def allocBufferType(typ: enum_field_types): WithZone[CVoidPtr] =
    val valPtr: CVoidPtr = typ match
      case enum_field_types.MYSQL_TYPE_VAR_STRING | enum_field_types.MYSQL_TYPE_STRING | enum_field_types.MYSQL_TYPE_BLOB | enum_field_types.MYSQL_TYPE_NULL =>
        alloc[CChar]()
      case enum_field_types.MYSQL_TYPE_LONG =>
        alloc[CInt]()
      case enum_field_types.MYSQL_TYPE_LONGLONG =>
        alloc[CLongLong]()
      case enum_field_types.MYSQL_TYPE_SHORT =>
        alloc[CShort]()
      case enum_field_types.MYSQL_TYPE_DOUBLE | enum_field_types.MYSQL_TYPE_NEWDECIMAL =>
        alloc[CDouble]()
      case enum_field_types.MYSQL_TYPE_FLOAT =>
        alloc[CFloat]()
      case enum_field_types.MYSQL_TYPE_TINY =>
        alloc[CBool]()
      case enum_field_types.MYSQL_TYPE_TIME | enum_field_types.MYSQL_TYPE_DATE | enum_field_types.MYSQL_TYPE_DATETIME | enum_field_types.MYSQL_TYPE_TIMESTAMP =>
        alloc[MYSQL_TIME]()
      case _ =>
        println(s"type? $typ")
        alloc[CString]()
    valPtr

  private def bindResult(): Try[Unit] =
    if mysql_stmt_bind_result(stmtPtr, bindsPtr)
    then Failure(collectStmtExn("Failed to bind statement result", stmtPtr))
    else Success(())

  private def storeResult(): Try[Unit] =
    if mysql_stmt_store_result(stmtPtr).toInt > 0
    then Failure(collectStmtExn("Failed to store statement result", stmtPtr))
    else
      Success(())

  private def getMetadataResult(): Try[Unit] =
    resPtr = mysql_stmt_result_metadata(stmtPtr)
    if resPtr== null
    then Failure(collectStmtExn("Failed to get statement result metadata", stmtPtr))
    else Success(())

  def close(): Unit =
    mysql_free_result(resPtr)
    val _ = mysql_stmt_close(stmtPtr)

