package com.mysql4s.rs

import com.mysql4s.TryWithZone

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait RowResultSet extends AutoCloseable:
  def count: Int
  def hasNext: Boolean
  def next(): TryWithZone[Option[RowResult]]
  def isEmpty: Boolean = count == 0
  def isNotEmpty: Boolean = !isEmpty
  def seek(): Unit

  def toSeq: TryWithZone[Seq[RowResult]] =
    seek()
    val items = mutable.ArrayBuffer[RowResult]()
    @tailrec
    def each(): Try[Seq[RowResult]] = next() match
      case Success(Some(row)) =>
        items.appended(row)
        each()
      case Success(None) => Success(items.toSeq)
      case Failure(err) => Failure(err)
    each()

  def map[T](f: RowResult => T): TryWithZone[Seq[T]] =
    seek()
    val items = mutable.ArrayBuffer[T]()
    @tailrec
    def each(): Try[Seq[T]] = next() match
      case Success(Some(row)) =>
        items.append(f(row))
        each()
      case Success(None) => Success(items.toSeq)
      case Failure(err) => Failure(err)
    each()

  def foreach(f: RowResult => Unit): TryWithZone[Unit] =
    seek()
    @tailrec
    def each(): Try[Unit] = next() match
      case Success(Some(row)) =>
        f(row)
        each()
      case Success(None) => Success(())
      case Failure(err) => Failure(err)
    each()

  def first: TryWithZone[Option[RowResult]] =
    seek()
    next()

  def firstMap[T](f: RowResult => T): TryWithZone[Option[T]] =
    seek()
    first match
      case Success(Some(row)) => Success(Some(f(row)))
      case Success(None) => Success(None)
      case Failure(err) => Failure(err)