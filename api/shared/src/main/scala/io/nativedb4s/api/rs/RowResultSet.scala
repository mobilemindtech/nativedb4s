package io.nativedb4s.api.rs

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * The MySQL execution result set
  */
trait RowResultSet extends AutoCloseable:
  def count: Int
  def hasNext: Boolean
  def next(): Option[RowResult]
  def isEmpty: Boolean = count == 0
  def isNotEmpty: Boolean = !isEmpty
  def seek(): Unit

  /**
    * Converts [[RowResultSet] to Seq of [[RowResult]
    *
    * @return A seq of row result
    */
  def toSeq: Seq[RowResult] =
    seek()
    val items = mutable.ArrayBuffer[RowResult]()
    @tailrec
    def each(): Seq[RowResult] = 
      next() match
        case Some(row) =>
          items.addOne(row)
          each()
        case None => items.toSeq
    each()

  /**
    * Map results 
    *
    * @param f map function
    * @return A seq of mapped reuslts 
    */
  def map[T](f: RowResult => T): Seq[T] =
    seek()
    val items = mutable.ArrayBuffer[T]()
    @tailrec
    def each(): Seq[T] = 
      next() match
        case Some(row) =>
          items.addOne(f(row))
          each()
        case None => items.toSeq
    each()

  /**
    * Foreach in results
    *
    * @param f A foreach function 
    * @return 
    */
  def foreach(f: RowResult => Unit): Unit =
    seek()
    @tailrec
    def each(): Unit = 
      next() match
        case Some(row) =>
          f(row)
          each()
        case None => ()
      
    each()

  /**
    * Get first result
    *
    * @return First result or None
    */
  def first: Option[RowResult] =
    seek()
    next()

  /**
    * Map first result
    *
    * @param f A map function 
    * @return A mapped first result or None
    */
  def firstMap[T](f: RowResult => T): Option[T] =
    seek()
    first.map(f)
      