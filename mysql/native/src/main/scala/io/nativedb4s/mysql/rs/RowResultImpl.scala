package io.nativedb4s.mysql.rs

import io.nativedb4s.mysql.bindings.enumerations.enum_field_types
import io.nativedb4s.mysql.types.TypeConverter
import io.nativedb4s.api.rs.RowResult
import io.nativedb4s.api.types.MySqlException
import io.nativedb4s.api.util.{QueryResult, ScalaTypes, |>}
import io.nativedb4s.mysql.util.*
import io.nativedb4s.mysql.types.TypeConverter.given

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.collection.mutable
import scala.scalanative.unsafe.Zone

extension (r: RowResult)
  def getOrNull[T <: ScalaTypes](index: Int | String)(using TypeConverter[T], Zone): T | Null =
    getAs[T](index) match
      case Some(v) => v
      case None => null

  def getAs[T <: ScalaTypes](index: Int | String)(using tc: TypeConverter[T], z: Zone): Option[T] =
    r.asInstanceOf[RowResultImpl].col(index) match
      case Some(col) =>
          tc.fromNative(col.ptr, col.typ, col.length) |> Some.apply
      case None => None

class RowResultImpl(columns: Seq[Column])(using Zone) extends RowResult:

  private[rs] def col(index: Int | String): Option[Column] =
    index match
      case i: Int => columns.lift(i)
      case s: String => columns.find(_.name == s)

  def getString(index: Int | String): Option[String] = this.getAs[String](index)

  def getShort(index: Int | String): Option[Short] = this.getAs[Short](index)

  def getInt(index: Int | String): Option[Int] = this.getAs[Int](index)

  def getLong(index: Int | String): Option[Long] = this.getAs[Long](index)

  def getFloat(index: Int | String): Option[Float] = this.getAs[Float](index)

  def getDouble(index: Int | String): Option[Double] = this.getAs[Double](index)

  def getBoolean(index: Int | String): Option[Boolean] = this.getAs[Boolean](index)

  def getBytes(index: Int | String): Option[Array[Byte]] = this.getAs[Array[Byte]](index)

  def getTime(index: Int | String): Option[LocalTime] = this.getAs[LocalTime](index)

  def getDate(index: Int | String): Option[LocalDate] = this.getAs[LocalDate](index)

  def getDateTime(index: Int | String): Option[LocalDateTime] = this.getAs[LocalDateTime](index)

  def getAsQueryResult[T]: QueryResult[T] =
    val data = mutable.Map[String, Any]()
    for i <- columns.indices do
      val col = columns(i)
      col.typ match
        case enum_field_types.MYSQL_TYPE_LONG =>
          data.addOne(col.name -> this.getOrNull[Int](i))
        case enum_field_types.MYSQL_TYPE_LONGLONG =>
          data.addOne(col.name -> this.getOrNull[Long](i))
        case enum_field_types.MYSQL_TYPE_SHORT =>
          data.addOne(col.name -> this.getOrNull[Short](i))
        case enum_field_types.MYSQL_TYPE_DOUBLE |
             enum_field_types.MYSQL_TYPE_NEWDECIMAL |
             enum_field_types.MYSQL_TYPE_DECIMAL =>
          data.addOne(col.name -> this.getOrNull[Double](i))
        case enum_field_types.MYSQL_TYPE_FLOAT =>
          data.addOne(col.name -> this.getOrNull[Float](i))
        case enum_field_types.MYSQL_TYPE_TINY  =>
          data.addOne(col.name -> this.getOrNull[Boolean](i))
        case enum_field_types.MYSQL_TYPE_TIME =>
          data.addOne(col.name -> this.getOrNull[LocalTime](i))
        case enum_field_types.MYSQL_TYPE_DATE =>
          data.addOne(col.name -> this.getOrNull[LocalDate](i))
        case enum_field_types.MYSQL_TYPE_DATETIME =>
          data.addOne(col.name -> this.getOrNull[LocalDateTime](i))
        case enum_field_types.MYSQL_TYPE_TIMESTAMP =>
          data.addOne(col.name -> this.getOrNull[LocalDateTime](i))
        case _ =>
          if isMysqlString(col.typ) || isMysqlBytes(col.typ)
          then data.addOne(col.name -> this.getOrNull[String](i))
          else throw MySqlException(s"wrong mysql data type ${col.typ} for column ${col.name}", -1)

    new QueryResult[T](data.toMap)

