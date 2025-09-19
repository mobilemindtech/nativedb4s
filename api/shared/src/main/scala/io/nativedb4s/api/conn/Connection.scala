package io.nativedb4s.api.conn

import io.nativedb4s.api.rs.{RowResult, RowResultSet}
import io.nativedb4s.api.stmt.PreparedStatement
import io.nativedb4s.api.util.{QueryResult, ScalaTypes}

import java.io.Closeable

trait Connection extends Closeable:
  def prepare(query: String): PreparedStatement
  def executeQuery(query: String, args: ScalaTypes*): RowResultSet
  def execute(query: String, args: ScalaTypes*): Int
  def rows(query: String, args: ScalaTypes*): Seq[RowResult]
  def rows[T](query: String, args: ScalaTypes*)(f: RowResult => T): Seq[T]
  def rowsAs[T](query: String, args: ScalaTypes*): Seq[QueryResult[T]]
  def firstRow(query: String, args: ScalaTypes*): Option[RowResult]
  def firstRow[T](query: String, args: ScalaTypes*)(f: RowResult => T): Option[T]
  def firstRowAs[T](query: String, args: ScalaTypes*): Option[QueryResult[T]]
  def lastInsertID: Int
  def affectedRows: Int
  def setAutoCommit(mode: Boolean): Unit
  def commit(): Unit
  def rollback(): Unit