package com.mysql4s

import com.mysql4s.bindings.enumerations.enum_field_types
import com.time4s.Date

import scala.scalanative.unsafe.{CString, Zone, fromCString, toCString}
import scala.util.Try

extension[A, B] (a: A)
  infix def |>(f: A => B): B = f(a)

extension (s: String)(using Zone)
  def c_str(): CString = toCString(s)

def toStr(value: CString): String = fromCString(value)

type WithZone[T] = Zone ?=> T
type TryWithZone[T] = WithZone[Try[T]]

sealed trait MyDate:
  def getDate: Date
case class MysqlTime(date: Date) extends MyDate:
  override def getDate: Date = date
case class MysqlDate(date: Date) extends MyDate:
  override def getDate: Date = date
case class MysqlDateTime(date: Date) extends MyDate:
  override def getDate: Date = date
case class MysqlTimestamp(date: Date) extends MyDate:
  override def getDate: Date = date

object MysqlTime:
  def now: Zone ?=> MysqlTime = MysqlTime(Date.now)

object MysqlDate:
  def now: Zone ?=> MysqlDate = MysqlDate(Date.now)

object MysqlDateTime:
  def now: Zone ?=> MysqlDateTime = MysqlDateTime(Date.now)

object MysqlTimestamp:
  def now: Zone ?=> MysqlTimestamp = MysqlTimestamp(Date.now)


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


extension (x: Any)
  inline def discard: Unit = ()

def ignore[T](a: => T): Unit =
  val _ = a