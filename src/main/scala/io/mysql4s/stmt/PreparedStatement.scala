package io.mysql4s.stmt

import io.mysql4s.*
import io.mysql4s.rs.RowResultSet
import io.mysql4s.types.TypeConverter

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.scalanative.unsafe.*


type ZoneUnit = Zone ?=> Unit

trait PreparedStatement extends AutoCloseable:
  def setString(index: Int, value: String | Null): WithZone[PreparedStatement]
  def setShort(index: Int, value: Short | Null): WithZone[PreparedStatement]
  def setInt(index: Int, value: Int | Null): WithZone[PreparedStatement]
  def setLong(index: Int, value: Long | Null): WithZone[PreparedStatement]
  def setFloat(index: Int, value: Float | Null): WithZone[PreparedStatement]
  def setDouble(index: Int, value: Double | Null): WithZone[PreparedStatement]
  def setBoolean(index: Int, value: Boolean | Null): WithZone[PreparedStatement]
  def setDate(index: Int, value: LocalDate | Null): WithZone[PreparedStatement]
  def setDateTime(index: Int, value: LocalDateTime | Null): WithZone[PreparedStatement]
  def setTime(index: Int, value: LocalTime | Null): WithZone[PreparedStatement]
  def setBytes(index: Int, value: Array[Byte] | Null): WithZone[PreparedStatement]
  def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using TypeConverter[T]): WithZone[PreparedStatement]
  def execute(): TryWithZone[Int]
  def execute(query: String, args: ScalaTypes*): TryWithZone[Int]
  def executeQuery(): TryWithZone[RowResultSet]
  def unsafeExecuteQuery(): CanThrowWithZone[RowResultSet]
  def executeQueryAs[T](): CanThrowWithZone[Seq[QueryResult[T]]]
  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet]
  def unsafeExecuteQuery(query: String, args: ScalaTypes*): CanThrowWithZone[RowResultSet]
  def executeQueryAs[T](query: String, args: ScalaTypes*): CanThrowWithZone[Seq[QueryResult[T]]]
  def prepare(query: String): TryWithZone[Int]
  def lastInsertID: Int


