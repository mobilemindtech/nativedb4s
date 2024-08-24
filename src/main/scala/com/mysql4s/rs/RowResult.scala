package com.mysql4s.rs

import com.mysql4s.types.TypeConverter
import com.mysql4s.{ScalaTypes, WithZone}
import com.time4s.Date

trait RowResult:
  def getString(index: Int | String): WithZone[Option[String]]
  def getInt(index: Int | String): WithZone[Option[Int]]
  def getShort(index: Int | String): WithZone[Option[Short]]
  def getLong(index: Int | String): WithZone[Option[Long]]
  def getFloat(index: Int | String): WithZone[Option[Float]]
  def getDouble(index: Int | String): WithZone[Option[Double]]
  def getBoolean(index: Int | String): WithZone[Option[Boolean]]
  def getBytes(index: Int | String): WithZone[Option[Array[Byte]]]
  def getTime(index: Int | String): WithZone[Option[Date]]
  def getDate(index: Int | String): WithZone[Option[Date]]
  def getDateTime(index: Int | String): WithZone[Option[Date]]
  def getTimestamp(index: Int | String): WithZone[Option[Date]]
  def getAs[T <: ScalaTypes](index: Int | String)(using TypeConverter[T]): WithZone[Option[T]]

