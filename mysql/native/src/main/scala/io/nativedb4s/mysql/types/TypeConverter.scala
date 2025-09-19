package io.nativedb4s.mysql.types

import io.nativedb4s.mysql.bindings.enumerations.enum_field_types
import io.nativedb4s.mysql.bindings.structs.{MYSQL_BIND, MYSQL_TIME}
import io.nativedb4s.api.util.{ScalaTypes, |>}
import io.nativedb4s.mysql.util.*

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.collection.mutable
import scala.scalanative.libc.stdlib.atof
import scala.scalanative.unsafe.{CBool, CChar, CDouble, CFloat, CInt, CLongLong, CShort, CString, CUnsignedLongInt, CVoidPtr, Ptr, Tag, Zone, alloc}
import scala.scalanative.unsigned.UnsignedRichInt


type WithZone[T] = Zone ?=> T

private[mysql] trait TypeConverter[SType <: ScalaTypes]:
  def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[SType]
  def bindValue(v: SType, bind: MYSQL_BIND): WithZone[Unit]

private[mysql] object TypeConverter:

  def apply[T <: ScalaTypes](using tc: TypeConverter[T]): TypeConverter[T] = tc

  given TypeConverter[String]:
    def fromNative(str: CVoidPtr, typ: enum_field_types, len: Int): WithZone[String]  = str.asInstanceOf[CString] |> toStr
    def bindValue(v: String, bind: MYSQL_BIND): WithZone[Unit] =
      val lenPtr = alloc[CUnsignedLongInt]()
      val len = v.length.toUInt
      !lenPtr = len
      bind.buffer = v.c_str()
      bind.buffer_length = len
      bind.length = lenPtr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_STRING

  given TypeConverter[Int]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Int] = !v.asInstanceOf[Ptr[CInt]]
    def bindValue(v: Int, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CInt]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_LONG

  given TypeConverter[Short]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Short] = !v.asInstanceOf[Ptr[CShort]]
    def bindValue(v: Short, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CShort]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_SHORT

  given TypeConverter[Long]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Long] = !v.asInstanceOf[Ptr[CLongLong]]
    def bindValue(v: Long, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CLongLong]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_LONGLONG

  given TypeConverter[Float]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Float] = !v.asInstanceOf[Ptr[CFloat]]
    def bindValue(v: Float, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CFloat]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_FLOAT

  given TypeConverter[Double]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Double] =
      if isMysqlDecimal(typ)
      then atof(v.asInstanceOf[CString])
      else !v.asInstanceOf[Ptr[CDouble]]

    def bindValue(v: Double, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CDouble]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DOUBLE

  given TypeConverter[Boolean]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Boolean] = !v.asInstanceOf[Ptr[CBool]]

    def bindValue(v: Boolean, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[CBool]()
      !ptr = v
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_TINY

  given TypeConverter[Array[Byte]]:
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

  given TypeConverter[LocalTime]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[LocalTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val hour = timePtr(0).hour.toInt
      val minute = timePtr(0).minute.toInt
      val second = timePtr(0).second.toInt
      LocalTime.of(hour, minute, second)

    def bindValue(v: LocalTime, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      (!ptr).hour = v.getHour.toUInt
      (!ptr).minute = v.getMinute.toUInt
      (!ptr).second = v.getSecond.toUInt
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_TIME

  given TypeConverter[LocalDate]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[LocalDate] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val year = timePtr(0).year.toInt
      val month = timePtr(0).month.toInt
      val day = timePtr(0).day.toInt
      LocalDate.of(year, month, day)

    def bindValue(v: LocalDate, bind: MYSQL_BIND): WithZone[Unit] =
      val ptr = alloc[MYSQL_TIME]()
      (!ptr).year = v.getYear.toUInt
      (!ptr).month = v.getMonthValue.toUInt
      (!ptr).day = v.getDayOfMonth.toUInt
      bind.buffer = ptr
      bind.buffer_type = enum_field_types.MYSQL_TYPE_DATE

  given TypeConverter[LocalDateTime]:
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[LocalDateTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val year = timePtr(0).year.toInt
      val month = timePtr(0).month.toInt
      val day = timePtr(0).day.toInt
      val hour = timePtr(0).hour.toInt
      val minute = timePtr(0).minute.toInt
      val second = timePtr(0).second.toInt
      LocalDateTime.of(year, month, day, hour, minute, second)

    def bindValue(v: LocalDateTime, bind: MYSQL_BIND): WithZone[Unit] =
        val ptr = alloc[MYSQL_TIME]()
        (!ptr).hour = v.getHour.toUInt
        (!ptr).minute = v.getMinute.toUInt
        (!ptr).second = v.getSecond.toUInt
        (!ptr).year = v.getYear.toUInt
        (!ptr).month = v.getMonthValue.toUInt
        (!ptr).day = v.getDayOfMonth.toUInt
        bind.buffer = ptr
        bind.buffer_type = enum_field_types.MYSQL_TYPE_DATETIME