package io.nativedb4s.api.stmt

import io.nativedb4s.api.rs.RowResultSet
import io.nativedb4s.api.util.{QueryResult, ScalaTypes}

import java.time.{LocalDate, LocalDateTime, LocalTime}

trait PreparedStatement extends AutoCloseable:
  def setString(index: Int, value: String | Null): PreparedStatement
  def setShort(index: Int, value: Short | Null): PreparedStatement
  def setInt(index: Int, value: Int | Null): PreparedStatement
  def setLong(index: Int, value: Long | Null): PreparedStatement
  def setFloat(index: Int, value: Float | Null): PreparedStatement
  def setDouble(index: Int, value: Double | Null): PreparedStatement
  def setBoolean(index: Int, value: Boolean | Null): PreparedStatement
  def setDate(index: Int, value: LocalDate | Null): PreparedStatement
  def setDateTime(index: Int, value: LocalDateTime | Null): PreparedStatement
  def setTime(index: Int, value: LocalTime | Null): PreparedStatement
  def setBytes(index: Int, value: Array[Byte] | Null): PreparedStatement
 // def setAs[T <: ScalaTypes](index: Int, value: T | Null): PreparedStatement
  def execute(): Int
  def execute(query: String, args: ScalaTypes*): Int
  def executeQuery(): RowResultSet
  def executeQueryAs[T](): Seq[QueryResult[T]]
  def executeQuery(query: String, args: ScalaTypes*): RowResultSet
  def executeQueryAs[T](query: String, args: ScalaTypes*): Seq[QueryResult[T]]
  def prepare(query: String): Int
  def lastInsertID: Int


