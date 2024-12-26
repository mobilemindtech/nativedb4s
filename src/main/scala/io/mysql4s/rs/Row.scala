package io.mysql4s.rs

import io.mysql4s.bindings.enumerations.enum_field_types
import io.mysql4s.types.TypeConverter
import io.mysql4s.{MySqlException, QueryResult, ScalaTypes, WithZone, isMysqlBytes, isMysqlString, |>}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.collection.mutable


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

  def getTime(index: Int | String): WithZone[Option[LocalTime]] = getAs[LocalTime](index)

  def getDate(index: Int | String): WithZone[Option[LocalDate]] = getAs[LocalDate](index)

  def getDateTime(index: Int | String): WithZone[Option[LocalDateTime]] = getAs[LocalDateTime](index)

  def getAsQueryResult[T]: WithZone[QueryResult[T]] =
    val data = mutable.Map[String, Any]()
    for i <- columns.indices do
      val col = columns(i)
      col.typ match
        case enum_field_types.MYSQL_TYPE_LONG =>
          data.addOne(col.name -> unsafeAs[Int](i))
        case enum_field_types.MYSQL_TYPE_LONGLONG =>
          data.addOne(col.name -> unsafeAs[Long](i))
        case enum_field_types.MYSQL_TYPE_SHORT =>
          data.addOne(col.name -> unsafeAs[Short](i))
        case enum_field_types.MYSQL_TYPE_DOUBLE |
             enum_field_types.MYSQL_TYPE_NEWDECIMAL |
             enum_field_types.MYSQL_TYPE_DECIMAL =>
          data.addOne(col.name -> unsafeAs[Double](i))
        case enum_field_types.MYSQL_TYPE_FLOAT =>
          data.addOne(col.name -> unsafeAs[Float](i))
        case enum_field_types.MYSQL_TYPE_TINY  =>
          data.addOne(col.name -> unsafeAs[Boolean](i))
        case enum_field_types.MYSQL_TYPE_TIME =>
          data.addOne(col.name -> unsafeAs[LocalTime](i))
        case enum_field_types.MYSQL_TYPE_DATE =>
          data.addOne(col.name -> unsafeAs[LocalDate](i))
        case enum_field_types.MYSQL_TYPE_DATETIME =>
          data.addOne(col.name -> unsafeAs[LocalDateTime](i))
        case enum_field_types.MYSQL_TYPE_TIMESTAMP =>
          data.addOne(col.name -> unsafeAs[LocalDateTime](i))
        case _ =>
          if isMysqlString(col.typ) || isMysqlBytes(col.typ)
          then data.addOne(col.name -> unsafeAs[String](i))
          else throw MySqlException(s"wrong mysql data type ${col.typ} for column ${col.name}", -1)

    new QueryResult[T](data.toMap)

