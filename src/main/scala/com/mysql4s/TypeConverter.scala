package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types

import scala.scalanative.unsafe.{CBool, CDouble, CFloat, CInt, CLongLong, CShort, CString, CVoidPtr, Ptr, Tag, alloc}

private[mysql4s] trait TypeConverter[SType <: ScalaTypes]:
  type CType <: MysqlTypesPtr
  def fromNative(v: CVoidPtr): SType
  def mysqlType: enum_field_types

private[mysql4s] object TypeConverter:
  given StringConverter: TypeConverter[String] with
    type CType = CString
    def fromNative(str: CVoidPtr): String  = str.asInstanceOf[CType] |> toStr
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_STRING

  given IntConverter: TypeConverter[Int] with
    type CType = Ptr[CInt]
    def fromNative(v: CVoidPtr): Int = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_LONG

  given ShortConverter: TypeConverter[Short] with
    type CType = Ptr[CShort]
    def fromNative(v: CVoidPtr): Short = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_SHORT

  given LongConverter: TypeConverter[Long] with
    type CType = Ptr[CLongLong]
    def fromNative(v: CVoidPtr): Long = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_LONGLONG

  given FloatConverter: TypeConverter[Float] with
    type CType = Ptr[CFloat]
    def fromNative(v: CVoidPtr): Float = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_FLOAT

  given DoubleConverter: TypeConverter[Double] with
    type CType = Ptr[CDouble]
    def fromNative(v: CVoidPtr): Double = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_DOUBLE

  given BooleanConverter: TypeConverter[Boolean] with
    type CType = Ptr[CBool]
    def fromNative(v: CVoidPtr): Boolean = !v.asInstanceOf[CType]
    def mysqlType: enum_field_types = enum_field_types.MYSQL_TYPE_TINY

