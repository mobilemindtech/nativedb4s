package com.mysql4s.rs

import com.mysql4s.{MySqlException, TryWithZone, isMysqlBytes, isMysqlString, toStr, |>}
import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.extern_functions.{mysql_data_seek, mysql_fetch_fields, mysql_fetch_lengths, mysql_fetch_row, mysql_free_result, mysql_num_fields, mysql_num_rows}
import com.mysql4s.bindings.structs.{MYSQL_RES, MYSQL_TIME}

import scala.collection.mutable
import scala.scalanative.libc.stdlib.{atof, atoi, atoll}
import scala.scalanative.unsafe.{CBool, CDouble, CInt, CLongLong, CShort, CVoidPtr, Ptr, alloc}
import scala.scalanative.unsigned.UnsignedRichInt
import scala.util.Success


class ResultSet(resPtr: Ptr[MYSQL_RES]) extends RowResultSet:

  private var numFields: Int = 0
  private var numRows: Int = 0
  private var currRowIndex = 0
  private val columns = mutable.ListBuffer[Column]()

  
  def init(): Unit =
    numRows = mysql_num_rows(resPtr).toInt
    numFields = mysql_num_fields(resPtr).toInt
    val cols = mysql_fetch_fields(resPtr)
    for i <- 0 until numFields do
      val col = cols(i)
      columns.append(
        Column(
          name = toStr(col.name),
          index = i,
          typ = col.`type`,
          precision = col.length.toInt - (if col.decimals.toInt > 0 then 2 else 1),
          decimals = col.decimals.toInt))

  override def count: Int = numRows

  override def hasNext: Boolean = currRowIndex < numRows

  override def seek(): Unit =
    currRowIndex = 0
    mysql_data_seek(resPtr, 0.toUInt)

  override def next(): TryWithZone[Option[RowResult]] =
    if hasNext
    then next0()
    else Success(None)

  private def next0(): TryWithZone[Option[RowResult]] =
    val row = mysql_fetch_row(resPtr)
    val lengths = mysql_fetch_lengths(resPtr)

    val cols = mutable.ListBuffer[Column]()
    for col <- columns do
      //println(s"${col.name}, ${col.typ}")
      val valRow = row.value(col.index)
      val valPtr: CVoidPtr = col.typ match
        case enum_field_types.MYSQL_TYPE_LONG =>
          val ptr = alloc[CInt]()
          !ptr = atoi(valRow)
          ptr
        case enum_field_types.MYSQL_TYPE_LONGLONG =>
          val ptr = alloc[CLongLong]()
          !ptr = atoll(valRow)
          ptr
        case enum_field_types.MYSQL_TYPE_SHORT =>
          val ptr = alloc[CShort]()
          !ptr = atoi(valRow).toShort
          ptr
        case enum_field_types.MYSQL_TYPE_DOUBLE |
             enum_field_types.MYSQL_TYPE_NEWDECIMAL |
             enum_field_types.MYSQL_TYPE_DECIMAL =>
          val ptr = alloc[CDouble]()
          !ptr = atof(valRow)
          ptr
        case enum_field_types.MYSQL_TYPE_FLOAT =>
          val ptr = alloc[CDouble]()
          !ptr = atof(valRow).toFloat
          ptr
        case enum_field_types.MYSQL_TYPE_TINY =>
          val ptr = alloc[CBool]()
          !ptr = atoi(valRow) == 1
          ptr
        case enum_field_types.MYSQL_TYPE_TIME |
             enum_field_types.MYSQL_TYPE_DATE |
             enum_field_types.MYSQL_TYPE_DATETIME |
             enum_field_types.MYSQL_TYPE_TIMESTAMP =>
          val ptr = alloc[MYSQL_TIME]()
          ptr
        case _ =>
          if isMysqlString(col.typ) || isMysqlBytes(col.typ)
          then valRow
          else
            throw MySqlException(s"type ${col.typ} not handled")

      cols.append(
        Column(
          name = col.name,
          index = col.index,
          typ = col.typ,
          isNull = valPtr == null,
          length = lengths(col.index).toInt,
          ptr = valPtr))

    currRowIndex += 1
    Row(cols.toSeq) |> Some.apply |> Success.apply

  override def close(): Unit =
    if resPtr != null then mysql_free_result(resPtr)