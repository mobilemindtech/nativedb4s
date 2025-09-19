package io.nativedb4s.mysql.stmt

import io.nativedb4s.api.rs.{RowResult, RowResultSet}
import io.nativedb4s.api.util.|>
import io.nativedb4s.mysql.bindings.enumerations.enum_field_types
import io.nativedb4s.mysql.bindings.extern_functions.*
import io.nativedb4s.mysql.bindings.structs.{MYSQL_BIND, MYSQL_RES, MYSQL_STMT, MYSQL_TIME}
import io.nativedb4s.mysql.rs.{Column, RowResultImpl}
import io.nativedb4s.mysql.util.*

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.scalanative.unsafe.{CBool, CChar, CDouble, CFloat, CInt, CLongLong, CShort, CUnsignedLongInt, CVoidPtr, Ptr, Zone, alloc}
import scala.scalanative.unsigned.UnsignedRichInt

private[mysql] class RowResultSetStmtImpl(stmtPtr: Ptr[MYSQL_STMT])(using Zone) extends RowResultSet:

  private var resPtr: Ptr[MYSQL_RES] = uninitialized

  private var bindsPtr: Ptr[MYSQL_BIND] = uninitialized
  private var isNullPtr: Ptr[CBool] = uninitialized
  private var errorPtr: Ptr[CBool] = uninitialized
  private var lengthPtr: Ptr[CUnsignedLongInt] = uninitialized

  private val columns = mutable.ListBuffer[Column]()

  private var numRows: Int = 0
  private var numFields: Int = 0
  private var currRowIndex: Int = 0
  
  def init(): Unit =
    readMetadataResult()
    bindFields()
    initBinds()
    bindResult()
    storeResult()
    readNumRows()

  private def bindFields(): Unit =
    numFields = mysql_stmt_field_count(stmtPtr).toInt
    bindsPtr = alloc[MYSQL_BIND](numFields)
    ()

  private def readNumRows(): Unit = {
    numRows = mysql_stmt_num_rows(stmtPtr).toInt
    ()
  }

  private def fetchFields(): Unit = {
    mysql_num_fields(resPtr)
    ()
  }

  private def initBinds(): Unit =
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

  override def seek(): Unit =
    currRowIndex = 0
    mysql_stmt_data_seek(stmtPtr, 0.toUInt)

  def next(): Option[RowResult] =
    if hasNext
    then next0()
    else None

  private def next0(): Option[RowResult] =
    mysql_stmt_fetch(stmtPtr) match
      case 0 | 101 => // 101 = MYSQL_DATA_TRUNCATED
        val cols = mutable.ListBuffer[Column]()
        for col <- columns do

          val valPtr =
            if isMysqlString(col.typ)  // decimal return char[]
            then allocColumnString(col)
            else if isMysqlBytes(col.typ) || isMysqlDecimal(col.typ)
            then allocColumnBytes(col)
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
        RowResultImpl(cols.toSeq) |> Some.apply
      case 100 => // MYSQL_NO_DATA
        None
      case 1 =>
        throw collectStmtExn("Failed to get statement result metadata", stmtPtr)
      case _ =>
        throw collectStmtExn("Failed to fetch statement", stmtPtr)

  private def allocColumnString(col: Column): CVoidPtr =
    val realLen = lengthPtr(col.index).toInt + 1
    val buffer = alloc[CChar](realLen)
    val bind = bindsPtr + col.index
    (!bind).buffer = buffer
    (!bind).buffer_length = realLen.toUInt
    if mysql_stmt_fetch_column(stmtPtr, bind, col.index.toUInt, 0.toUInt) > 0
    then println(s"${collectStmtExn("error to fetch column", stmtPtr)}")
    buffer(realLen - 1) = '\u0000'.toByte
    buffer

  private def allocColumnBytes(col: Column): CVoidPtr =
    val realLen = lengthPtr(col.index).toInt
    val buffer = alloc[CChar](realLen)
    val bind = bindsPtr + col.index
    (!bind).buffer = buffer
    (!bind).buffer_length = realLen.toUInt
    if mysql_stmt_fetch_column(stmtPtr, bind, col.index.toUInt, 0.toUInt) > 0
    then println(s"${collectStmtExn("error to fetch column", stmtPtr)}")
    buffer

  private def allocBufferType(typ: enum_field_types): CVoidPtr =
    val valPtr: CVoidPtr = typ match
      case enum_field_types.MYSQL_TYPE_LONG =>
        alloc[CInt]()
      case enum_field_types.MYSQL_TYPE_LONGLONG =>
        alloc[CLongLong]()
      case enum_field_types.MYSQL_TYPE_SHORT =>
        alloc[CShort]()
      case enum_field_types.MYSQL_TYPE_DOUBLE  =>
        alloc[CDouble]()
      case enum_field_types.MYSQL_TYPE_FLOAT =>
        alloc[CFloat]()
      case enum_field_types.MYSQL_TYPE_TINY =>
        alloc[CBool]()
      case enum_field_types.MYSQL_TYPE_TIME |
           enum_field_types.MYSQL_TYPE_DATE |
           enum_field_types.MYSQL_TYPE_DATETIME |
           enum_field_types.MYSQL_TYPE_TIMESTAMP =>
        alloc[MYSQL_TIME]()
      case _ =>
        alloc[CChar]()
    valPtr

  private def bindResult(): Unit =
    if mysql_stmt_bind_result(stmtPtr, bindsPtr)
    then throw collectStmtExn("Failed to bind statement result", stmtPtr)
    
  private def storeResult(): Unit =
    if mysql_stmt_store_result(stmtPtr) > 0
    then throw collectStmtExn("Failed to store statement result", stmtPtr)
    
  private def readMetadataResult(): Unit =
    resPtr = mysql_stmt_result_metadata(stmtPtr)
    if resPtr == null
    then throw collectStmtExn("Failed to get statement result metadata", stmtPtr)

  def close(): Unit =
    mysql_free_result(resPtr)
    val _ = mysql_stmt_close(stmtPtr)
