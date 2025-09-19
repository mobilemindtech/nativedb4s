package io.nativedb4s.api.rs

import io.nativedb4s.api.util.{QueryResult, ScalaTypes}

import java.time.{LocalDate, LocalDateTime, LocalTime}

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
  def getString(index: Int | String): Option[String]
  /**
    * Get result as Int
    *
    * @param index Index 0 based or column name
    * @return A Int or None
    */
  def getInt(index: Int | String): Option[Int]
  /**
    * Get result as Short
    *
    * @param index Index 0 based or column name
    * @return A Short or None
    */
  def getShort(index: Int | String): Option[Short]
  /**
    * Get result as Lobg
    *
    * @param index Index 0 based or column name
    * @return A Lobg or None
    */
  def getLong(index: Int | String): Option[Long]
  /**
    * Get result as Float
    *
    * @param index Index 0 based or column name
    * @return A Float or None
    */
  def getFloat(index: Int | String): Option[Float]
  /**
    * Get result as Double
    *
    * @param index Index 0 based or column name
    * @return A Double or None
    */
  def getDouble(index: Int | String): Option[Double]
  /**
    * Get result as Boolean
    *
    * @param index Index 0 based or column name
    * @return A Boolean or None
    */
  def getBoolean(index: Int | String): Option[Boolean]
  /**
    * Get result as Byte Array
    *
    * @param index Index 0 based or column name
    * @return A Byte Array or None
    */
  def getBytes(index: Int | String): Option[Array[Byte]]
  /**
    * Get result as Time
    *
    * @param index Index 0 based or column name
    * @return A Time or None
    */
  def getTime(index: Int | String): Option[LocalTime]
  /**
    * Get result as Date
    *
    * @param index Index 0 based or column name
    * @return A Date or None
    */
  def getDate(index: Int | String): Option[LocalDate]
  /**
    * Get result as DateTime
    *
    * @param index Index 0 based or column name
    * @return A DateTime or None
    */
  def getDateTime(index: Int | String): Option[LocalDateTime]

  def getAsQueryResult[T]: QueryResult[T]


  /**
    * Get result as T
    *
    * @param index Index 0 based or column name
    * @return A T or None
    */
  //def getAs[T <: ScalaTypes](index: Int | String): Option[T]
  /*
  def unsafeAs[T <: ScalaTypes](index: Int | String): T | Null =
    getAs[T](index) match
      case Some(v) => v
      case None => null
  */
