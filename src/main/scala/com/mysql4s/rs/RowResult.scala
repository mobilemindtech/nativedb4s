package com.mysql4s.rs

import com.mysql4s.types.TypeConverter
import com.mysql4s.{ScalaTypes, WithZone}
import com.time4s.Date

/**
  * The row result
  */
trait RowResult:
  /**
    * Get result as String
    *
    * @param index Index 0 based or column name
    * @return A String or None
    */
  def getString(index: Int | String): WithZone[Option[String]]
  /**
    * Get result as Int
    *
    * @param index Index 0 based or column name
    * @return A Int or None
    */
  def getInt(index: Int | String): WithZone[Option[Int]]
  /**
    * Get result as Short
    *
    * @param index Index 0 based or column name
    * @return A Short or None
    */
  def getShort(index: Int | String): WithZone[Option[Short]]
  /**
    * Get result as Lobg
    *
    * @param index Index 0 based or column name
    * @return A Lobg or None
    */
  def getLong(index: Int | String): WithZone[Option[Long]]
  /**
    * Get result as Float
    *
    * @param index Index 0 based or column name
    * @return A Float or None
    */
  def getFloat(index: Int | String): WithZone[Option[Float]]
  /**
    * Get result as Double
    *
    * @param index Index 0 based or column name
    * @return A Double or None
    */
  def getDouble(index: Int | String): WithZone[Option[Double]]
  /**
    * Get result as Boolean
    *
    * @param index Index 0 based or column name
    * @return A Boolean or None
    */
  def getBoolean(index: Int | String): WithZone[Option[Boolean]]
  /**
    * Get result as Byte Array
    *
    * @param index Index 0 based or column name
    * @return A Byte Array or None
    */
  def getBytes(index: Int | String): WithZone[Option[Array[Byte]]]
  /**
    * Get result as Time
    *
    * @param index Index 0 based or column name
    * @return A Time or None
    */
  def getTime(index: Int | String): WithZone[Option[Date]]
  /**
    * Get result as Date
    *
    * @param index Index 0 based or column name
    * @return A Date or None
    */
  def getDate(index: Int | String): WithZone[Option[Date]]
  /**
    * Get result as DateTime
    *
    * @param index Index 0 based or column name
    * @return A DateTime or None
    */
  def getDateTime(index: Int | String): WithZone[Option[Date]]
  /**
    * Get result as Timestamp
    *
    * @param index Index 0 based or column name
    * @return A Timestamp or None
    */
  def getTimestamp(index: Int | String): WithZone[Option[Date]]
  /**
    * Get result as T
    *
    * @param index Index 0 based or column name
    * @return A T or None
    */
  def getAs[T <: ScalaTypes](index: Int | String)(using TypeConverter[T]): WithZone[Option[T]]

