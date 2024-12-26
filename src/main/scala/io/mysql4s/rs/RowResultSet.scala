package io.mysql4s.rs

import io.mysql4s.TryWithZone

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * The MySQL execution result set
  */
trait RowResultSet extends AutoCloseable:
  def count: Int
  def hasNext: Boolean
  def next(): TryWithZone[Option[RowResult]]
  def isEmpty: Boolean = count == 0
  def isNotEmpty: Boolean = !isEmpty
  def seek(): Unit

  /**
    * Converts [[RowResultSet]] to Seq of [[RowResult]]
    *
    * @return A seq of row result
    */
  def toSeq: TryWithZone[Seq[RowResult]] =
    seek()
    val items = mutable.ArrayBuffer[RowResult]()
    @tailrec
    def each(): Try[Seq[RowResult]] = next() match
      case Success(Some(row)) =>
        items.addOne(row)
        each()
      case Success(None) => Success(items.toSeq)
      case Failure(err) => Failure(err)
    each()

  /**
    * Map results 
    *
    * @param f map function
    * @return A seq of mapped reuslts 
    */
  def map[T](f: RowResult => T): TryWithZone[Seq[T]] =
    seek()
    val items = mutable.ArrayBuffer[T]()
    @tailrec
    def each(): Try[Seq[T]] = next() match
      case Success(Some(row)) =>
        items.addOne(f(row))
        each()
      case Success(None) => Success(items.toSeq)
      case Failure(err) => Failure(err)
    each()

  /**
    * Foreach in results
    *
    * @param f A foreach function 
    * @return 
    */
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

  /**
    * Get first result
    *
    * @return First result or None
    */
  def first: TryWithZone[Option[RowResult]] =
    seek()
    next()

  /**
    * Map first result
    *
    * @param f A map function 
    * @return A mapped first result or None
    */
  def firstMap[T](f: RowResult => T): TryWithZone[Option[T]] =
    seek()
    first match
      case Success(Some(row)) => Success(Some(f(row)))
      case Success(None) => Success(None)
      case Failure(err) => Failure(err)