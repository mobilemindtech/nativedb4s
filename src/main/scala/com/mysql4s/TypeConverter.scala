package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types
import com.mysql4s.bindings.structs.{MYSQL_DATA, MYSQL_TIME}

import java.util.Calendar
import scala.collection.mutable
import scala.scalanative.libc.stdlib.atof
import scala.scalanative.unsafe.{CBool, CDouble, CFloat, CInt, CLongLong, CShort, CString, CVoidPtr, Ptr, Tag, alloc}


private[mysql4s] trait TypeConverter[SType <: ScalaTypes]:
  def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[SType]

private[mysql4s] object TypeConverter:
  given StringConverter: TypeConverter[String] with
    def fromNative(str: CVoidPtr, typ: enum_field_types, len: Int): WithZone[String]  = str.asInstanceOf[CType] |> toStr

  given IntConverter: TypeConverter[Int] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Int] = !v.asInstanceOf[CType]

  given ShortConverter: TypeConverter[Short] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Short] = !v.asInstanceOf[CType]

  given LongConverter: TypeConverter[Long] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Long] = !v.asInstanceOf[CType]

  given FloatConverter: TypeConverter[Float] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Float] = !v.asInstanceOf[CType]

  given DoubleConverter: TypeConverter[Double] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Double] =
      if isMysqlDecimal(typ)
      then atof(v.asInstanceOf[CString])
      else !v.asInstanceOf[CType]

  given BooleanConverter: TypeConverter[Boolean] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Boolean] = !v.asInstanceOf[CType]

  given BytesConverter: TypeConverter[Array[Byte]] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Array[Byte]] =
      val bytes = v.asInstanceOf[CString]
      val array = mutable.ArrayBuffer[Byte]()
      for i <- 0 until len do
        array.append(bytes(i))
      array.toArray

  given TimeConverter: TypeConverter[MysqlTime] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val cal = Calendar.getInstance()
      cal.set(Calendar.HOUR, timePtr(0).hour.toInt)
      cal.set(Calendar.MINUTE, timePtr(0).minute.toInt)
      cal.set(Calendar.SECOND, timePtr(0).second.toInt)
      MysqlTime(cal.getTime)

  given DateConverter: TypeConverter[MysqlDate] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlDate] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val cal = Calendar.getInstance()
      cal.set(Calendar.YEAR, timePtr(0).year.toInt)
      cal.set(Calendar.MONTH, timePtr(0).month.toInt)
      cal.set(Calendar.DATE, timePtr(0).day.toInt)
      MysqlDate(cal.getTime)

  given DateTimeConverter: TypeConverter[MysqlDateTime] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlDateTime] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val cal = Calendar.getInstance()
      cal.set(Calendar.YEAR, timePtr(0).year.toInt)
      cal.set(Calendar.MONTH, timePtr(0).month.toInt)
      cal.set(Calendar.DATE, timePtr(0).day.toInt)
      cal.set(Calendar.HOUR, timePtr(0).hour.toInt)
      cal.set(Calendar.MINUTE, timePtr(0).minute.toInt)
      cal.set(Calendar.SECOND, timePtr(0).second.toInt)
      MysqlDateTime(cal.getTime)

  given TimestampConverter: TypeConverter[MysqlTimestamp] with
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[MysqlTimestamp] =
      val timePtr = v.asInstanceOf[Ptr[MYSQL_TIME]]
      val cal = Calendar.getInstance()
      cal.set(Calendar.YEAR, timePtr(0).year.toInt)
      cal.set(Calendar.MONTH, timePtr(0).month.toInt)
      cal.set(Calendar.DATE, timePtr(0).day.toInt)
      cal.set(Calendar.HOUR, timePtr(0).hour.toInt)
      cal.set(Calendar.MINUTE, timePtr(0).minute.toInt)
      cal.set(Calendar.SECOND, timePtr(0).second.toInt)
      MysqlTimestamp(cal.getTime)
