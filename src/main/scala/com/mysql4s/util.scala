package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types

import scala.scalanative.unsafe.{CBool, CDouble, CFloat, CInt, CLongLong, CShort, CString, Ptr, Zone, fromCString, toCString}
import scala.util.Try

extension[A, B] (a: A)
  infix def |>(f: A => B): B = f(a)

extension (s: String)(using Zone)
  def c_str(): CString = toCString(s)

def toStr(value: CString): String = fromCString(value)

type WithZone[T] = Zone ?=> T

type TryWithZone[T] = WithZone[Try[T]]

def ignore[U](f: => U):U = null.asInstanceOf[U]

private[mysql4s] def isMysqlString(typ: enum_field_types): Boolean =
  typ == enum_field_types.MYSQL_TYPE_STRING || typ == enum_field_types.MYSQL_TYPE_VAR_STRING || typ == enum_field_types.MYSQL_TYPE_VARCHAR

private[mysql4s] def isMysqlDecimal(typ: enum_field_types): Boolean =
  typ == enum_field_types.MYSQL_TYPE_DECIMAL || typ == enum_field_types.MYSQL_TYPE_NEWDECIMAL

type ScalaTypes = String | Int | Short | Long | Float | Double | Boolean
type MysqlTypes = CString | CInt | CShort | CLongLong | CFloat | CDouble | CBool
type MysqlTypesPtr = CString | Ptr[CInt] | Ptr[CShort] | Ptr[CLongLong] | Ptr[CFloat] | Ptr[CDouble] | Ptr[CBool]

