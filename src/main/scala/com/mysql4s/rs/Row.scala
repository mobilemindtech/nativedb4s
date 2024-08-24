package com.mysql4s.rs

import com.mysql4s.types.TypeConverter
import com.mysql4s.{MysqlDate, MysqlDateTime, MysqlTime, MysqlTimestamp, ScalaTypes, WithZone, |>}
import com.time4s.Date

class Row(columns: Seq[Column]) extends RowResult:

  private def col(index: Int | String): Option[Column] =
    index match
      case i: Int => columns.lift(i)
      case s: String => columns.find(_.name == s)

  def getAs[T <: ScalaTypes](index: Int | String)(using nc: TypeConverter[T]): WithZone[Option[T]] =
    col(index) match
      case Some(col) => nc.fromNative(col.ptr, col.typ, col.length) |> Some.apply
      case None => None

  def getString(index: Int | String): WithZone[Option[String]] = getAs[String](index)

  def getShort(index: Int | String): WithZone[Option[Short]] = getAs[Short](index)

  def getInt(index: Int | String): WithZone[Option[Int]] = getAs[Int](index)

  def getLong(index: Int | String): WithZone[Option[Long]] = getAs[Long](index)

  def getFloat(index: Int | String): WithZone[Option[Float]] = getAs[Float](index)

  def getDouble(index: Int | String): WithZone[Option[Double]] = getAs[Double](index)

  def getBoolean(index: Int | String): WithZone[Option[Boolean]] = getAs[Boolean](index)

  def getBytes(index: Int | String): WithZone[Option[Array[Byte]]] = getAs[Array[Byte]](index)

  def getTime(index: Int | String): WithZone[Option[Date]] = getAs[MysqlTime](index) match
    case None => None
    case Some(d) => Some(d.date)

  def getDate(index: Int | String): WithZone[Option[Date]] = getAs[MysqlDate](index) match
    case None => None
    case Some(d) => Some(d.date)

  def getDateTime(index: Int | String): WithZone[Option[Date]] = getAs[MysqlDateTime](index) match
    case None => None
    case Some(d) => Some(d.date)

  def getTimestamp(index: Int | String): WithZone[Option[Date]] = getAs[MysqlTimestamp](index) match
    case None => None
    case Some(d) => Some(d.date)