package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types

import scala.collection.mutable
import scala.scalanative.libc.stdlib.atof
import scala.scalanative.unsafe.{CBool, CDouble, CFloat, CInt, CLongLong, CShort, CString, CVoidPtr, Ptr, Tag, alloc}


private[mysql4s] trait TypeConverter[SType <: ScalaTypes]:
  type CType <: MysqlTypesPtr
  def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[SType]

private[mysql4s] object TypeConverter:
  given StringConverter: TypeConverter[String] with
    type CType = CString
    def fromNative(str: CVoidPtr, typ: enum_field_types, len: Int): WithZone[String]  = str.asInstanceOf[CType] |> toStr

  given IntConverter: TypeConverter[Int] with
    type CType = Ptr[CInt]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Int] = !v.asInstanceOf[CType]

  given ShortConverter: TypeConverter[Short] with
    type CType = Ptr[CShort]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Short] = !v.asInstanceOf[CType]

  given LongConverter: TypeConverter[Long] with
    type CType = Ptr[CLongLong]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Long] = !v.asInstanceOf[CType]

  given FloatConverter: TypeConverter[Float] with
    type CType = Ptr[CFloat]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Float] = !v.asInstanceOf[CType]

  given DoubleConverter: TypeConverter[Double] with
    type CType = Ptr[CDouble]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Double] =
      if isMysqlDecimal(typ)
      then atof(v.asInstanceOf[CString])
      else !v.asInstanceOf[CType]

  given BooleanConverter: TypeConverter[Boolean] with
    type CType = Ptr[CBool]
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Boolean] = !v.asInstanceOf[CType]

  given BytesConverter: TypeConverter[Array[Byte]] with
    type CType = CString
    def fromNative(v: CVoidPtr, typ: enum_field_types, len: Int): WithZone[Array[Byte]] =
      val bytes = v.asInstanceOf[CString]
      val array = mutable.ArrayBuffer[Byte]()
      for i <- 0 until len do
        array.append(bytes(i))
      array.toArray  
