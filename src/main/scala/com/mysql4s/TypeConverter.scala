package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.structs.{MYSQL_BIND, MYSQL_TIME}

import scala.collection.mutable
import scala.scalanative.libc.stdlib.atof
import scala.scalanative.posix.time.tm
import scala.scalanative.unsafe.{CBool, CChar, CDouble, CFloat, CInt, CLongLong, CShort, CString, CUnsignedLongInt, CVoidPtr, Ptr, Tag, alloc}
import scala.scalanative.unsigned.UnsignedRichInt
import com.time4s.TIME.*
import com.time4s.Date

private[mysql4s] trait TypeConverter[SType <: ScalaTypes]:
  def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[SType]
  def bindValue(v: SType, bind: MYSQL_BIND): WithZone[Unit]

private[mysql4s] object TypeConverter:
  given StringConverter: TypeConverter[String] with
    def fromNative(str: CVoidPtr, typ: enum_field_types, len: Int): WithZone[String]  = str.asInstanceOf[CString] |> toStr
    def bindValue(v: String, bind: MYSQL_BIND): WithZone[Unit] =
      val lenPtr = alloc[CUnsignedLongInt]()
      val len = v.length.toUInt
      !lenPtr = len
      bind.buffer = v.c_str()
      bind.buffer_length = len
      bind.length = lenPtr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_STRING


  given IntConverter: TypeConverter[Int] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Int] = !v.asInstanceOf[Ptr[CInt]]
    def bindValue(v: Int, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CInt]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_LONG

  given ShortConverter: TypeConverter[Short] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Short] = !v.asInstanceOf[Ptr[CShort]]
    def bindValue(v: Short, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CShort]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_SHORT

  given LongConverter: TypeConverter[Long] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Long] = !v.asInstanceOf[Ptr[CLongLong]]
    def bindValue(v: Long, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CLongLong]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_LONGLONG

  given FloatConverter: TypeConverter[Float] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Float] = !v.asInstanceOf[Ptr[CFloat]]
    def bindValue(v: Float, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CFloat]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_FLOAT

  given DoubleConverter: TypeConverter[Double] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Double] =
      if isMysqlDecimal(typ)
      then atof(v.asInstanceOf[CString])
      else !v.asInstanceOf[CString]

    def bindValue(v: Double, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CDouble]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DOUBLE

  given BooleanConverter: TypeConverter[Boolean] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Boolean] = !v.asInstanceOf[Ptr[CBool]]

    def bindValue(v: Boolean, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CBool]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_TINY

  given BytesConverter: TypeConverter[Array[Byte]] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Array[Byte]] =
      val bytes = v.asInstanceOf[CString]
      val array = mutable.ArrayBuffer[Byte]()
      for i <- 0 until len do
        array.append(bytes(i))
      array.toArray

    def bindValue(v: Array[Byte], bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CChar](v.length)
      for i <- v.indices do
        ptr(i) = v(i)
      bind.buffer = ptr
      bind.buffer_length = v.length.toUInt
      bind.buffer_type = enum_field_types.MYSQL_TYPE_BLOB

  given TimeConverter: TypeConverter[MysqlTime] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val tmPtr = alloc[tm]()
      tmPtr(0).hour_=(timePtr(0).hour.toInt)
      tmPtr(0).minute_=(timePtr(0).minute.toInt)
      tmPtr(0).second_=(timePtr(0).second.toInt)
      MysqlTime(Date(tmPtr))

    def bindValue(v: MysqlTime, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      ptr(0).hour_=(v.date.hours.toUInt)
      ptr(0).minute_=(v.date.minutes.toUInt)
      ptr(0).second_=(v.date.seconds.toUInt)
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_TIME

  given DateConverter: TypeConverter[MysqlDate] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlDate] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val tmPtr = alloc[tm]()
      tmPtr(0).year_=(timePtr(0).year.toInt)
      tmPtr(0).month_=(timePtr(0).month.toInt)
      tmPtr(0).day_=(timePtr(0).day.toInt)
      MysqlDate(Date(tmPtr))

    def bindValue(v: MysqlDate, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      ptr(0).year_=(v.date.year.toUInt)
      ptr(0).month_=(v.date.month.toUInt)
      ptr(0).day_=(v.date.day.toUInt)
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DATE

  given DateTimeConverter: TypeConverter[MysqlDateTime] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlDateTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val tmPtr = alloc[tm]()
      tmPtr(0).year_=(timePtr(0).year.toInt)
      tmPtr(0).month_=(timePtr(0).month.toInt)
      tmPtr(0).day_=(timePtr(0).day.toInt)
      tmPtr(0).hour_=(timePtr(0).hour.toInt)
      tmPtr(0).minute_=(timePtr(0).minute.toInt)
      tmPtr(0).second_=(timePtr(0).second.toInt)
      MysqlDateTime(Date(tmPtr))

    def bindValue(v: MysqlDateTime, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      ptr(0).hour_=(v.date.hours.toUInt)
      ptr(0).minute_=(v.date.minutes.toUInt)
      ptr(0).second_=(v.date.seconds.toUInt)
      ptr(0).year_=(v.date.year.toUInt)
      ptr(0).month_=(v.date.month.toUInt)
      ptr(0).day_=(v.date.day.toUInt)
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DATETIME

  given TimestampConverter: TypeConverter[MysqlTimestamp] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlTimestamp] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val tmPtr = alloc[tm]()
      tmPtr(0).year_=(timePtr(0).year.toInt)
      tmPtr(0).month_=(timePtr(0).month.toInt)
      tmPtr(0).day_=(timePtr(0).day.toInt)
      tmPtr(0).hour_=(timePtr(0).hour.toInt)
      tmPtr(0).minute_=(timePtr(0).minute.toInt)
      tmPtr(0).second_=(timePtr(0).second.toInt)
      MysqlTimestamp(Date(tmPtr))

    def bindValue(v: MysqlTimestamp, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      ptr(0).hour_=(v.date.hours.toUInt)
      ptr(0).minute_=(v.date.minutes.toUInt)
      ptr(0).second_=(v.date.seconds.toUInt)
      ptr(0).year_=(v.date.year.toUInt)
      ptr(0).month_=(v.date.month.toUInt)
      ptr(0).day_=(v.date.day.toUInt)
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DATETIME