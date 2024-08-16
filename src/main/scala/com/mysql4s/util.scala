package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types

import java.util.Date
import scala.scalanative.unsafe.{CBool, CDouble, CFloat, CInt, CLongLong, CShort, CString, Ptr, Zone, fromCString, toCString}
import scala.util.Try

extension[A, B] (a: A)
  infix def |>(f: A => B): B = f(a)

extension (s: String)(using Zone)
  def c_str(): CString = toCString(s)

def toStr(value: CString): String = fromCString(value)

type WithZone[T] = Zone ?=> T
type TryWithZone[T] = WithZone[Try[T]]

opaque type MysqlTime = Date | Null
object MysqlTime:
  def apply(d: Date): MysqlTime = d
  extension (d: MysqlTime)
    def toDate: Date = d

opaque type MysqlDate = Date | Null
object MysqlDate:
  def apply(d: Date): MysqlDate = d
  extension (d: MysqlDate)
    def toDate: Date = d

opaque type MysqlDateTime = Date | Null
object MysqlDateTime:
  def apply(d: Date): MysqlDateTime = d
  extension (d: MysqlDateTime)
    def toDate: Date = d

opaque type MysqlTimestamp = Date | Null
object MysqlTimestamp:
  def apply(d: Date): MysqlTimestamp = d
  extension (d: MysqlTimestamp)
    def toDate: Date = d

private[mysql4s] def isMysqlString(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_STRING |
         enum_field_types.MYSQL_TYPE_VAR_STRING |
         enum_field_types.MYSQL_TYPE_VARCHAR => true
    case _ => false

private[mysql4s] def isMysqlBytes(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_TINY_BLOB |
         enum_field_types.MYSQL_TYPE_BLOB |
         enum_field_types.MYSQL_TYPE_MEDIUM_BLOB |
         enum_field_types.MYSQL_TYPE_LONG_BLOB => true
    case _ => false

private[mysql4s] def isMysqlDecimal(typ: enum_field_types): Boolean =
  typ == enum_field_types.MYSQL_TYPE_DECIMAL || typ == enum_field_types.MYSQL_TYPE_NEWDECIMAL




type ScalaTypes = String | Int | Short | Long | Float | Double | Boolean | Array[Byte] | MysqlDate | MysqlTime | MysqlDateTime | MysqlTimestamp
type MysqlTypesPtr = CString | Ptr[CInt] | Ptr[CShort] | Ptr[CLongLong] | Ptr[CFloat] | Ptr[CDouble] | Ptr[CBool]

extension (x: Any)
  inline def discard: Unit = ()

def ignore[T](a: => T): Unit =
  val _ = a