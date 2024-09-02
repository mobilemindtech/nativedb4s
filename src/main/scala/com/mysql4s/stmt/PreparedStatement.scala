package com.mysql4s.stmt

import com.mysql4s.*
import com.mysql4s.rs.RowResultSet
import com.mysql4s.types.TypeConverter

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
  def setDate(index: Int, value: MysqlDate | Null): WithZone[PreparedStatement]
  def setDateTime(index: Int, value: MysqlDateTime | Null): WithZone[PreparedStatement]
  def setTime(index: Int, value: MysqlTime | Null): WithZone[PreparedStatement]
  def setTimestamp(index: Int, value: MysqlTimestamp | Null): WithZone[PreparedStatement]
  def setBytes(index: Int, value: Array[Byte] | Null): WithZone[PreparedStatement]
  def setAs[T <: ScalaTypes](index: Int, value: T | Null)(using TypeConverter[T]): WithZone[PreparedStatement]
  def execute(): TryWithZone[Int]
  def execute(query: String, args: ScalaTypes*): TryWithZone[Int]
  def executeQuery(): TryWithZone[RowResultSet]
  def executeQuery(query: String, args: ScalaTypes*): TryWithZone[RowResultSet]
  def prepare(query: String): TryWithZone[Int]
  def lastInsertID: Int


