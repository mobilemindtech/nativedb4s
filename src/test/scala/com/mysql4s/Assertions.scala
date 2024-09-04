package com.mysql4s

import com.time4s.Date
import org.junit.Assert.*

import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

object Assertions:

  enum MysqlDateType:
    case MTime, MDate, MDateTime, MTimestamp

  import MysqlDateType.*

  def assertOption[T <: Matchable](expected: T)(value: Option[T]): T =
    value match
      case None =>
        fail(s"$value != $expected")
        null.asInstanceOf[T]
      case Some(v: Array[Byte]) =>
        assertArrayEquals(expected.asInstanceOf[Array[Byte]], v.asInstanceOf[Array[Byte]])
        null.asInstanceOf[T]
      case Some(v) =>
        assertEquals(expected, v)
        null.asInstanceOf[T]

  def assertOptionDate(expected: MyDate, typ: MysqlDateType)(value: Option[Date]): Date =
    value match
      case None =>
        fail(s"$value != $expected")
        null.asInstanceOf[Date]
      case Some(date) =>

        typ match
          case MTime =>
            assertEquals("assert hours", expected.getDate.hours, date.hours)
            assertEquals("assert minutes", expected.getDate.minutes, date.minutes)
            assertEquals("assert seconds", expected.getDate.seconds, date.seconds)
          case MDate =>
            assertEquals("assert day", expected.getDate.day, date.day)
            assertEquals("assert month", expected.getDate.month, date.month)
            assertEquals("assert year", expected.getDate.year, date.year)
          case MDateTime =>
            assertEquals("assert hours", expected.getDate.hours, date.hours)
            assertEquals("assert minutes", expected.getDate.minutes, date.minutes)
            assertEquals("assert seconds", expected.getDate.seconds, date.seconds)
            assertEquals("assert day", expected.getDate.day, date.day)
            assertEquals("assert month", expected.getDate.month, date.month)
            assertEquals("assert year", expected.getDate.year, date.year)
          case MTimestamp =>
            assertEquals("assert hours", expected.getDate.hours, date.hours)
            assertEquals("assert minutes", expected.getDate.minutes, date.minutes)
            assertEquals("assert seconds", expected.getDate.seconds, date.seconds)
            assertEquals("assert day", expected.getDate.day, date.day)
            assertEquals("assert month", expected.getDate.month, date.month)
            assertEquals("assert year", expected.getDate.year, date.year)
        date

  def assertTryOption[T](expected: T)(value: Try[Option[T]]): T =
    value match
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[T]
      case Success(None) =>
        fail(s"$value != $expected")
        null.asInstanceOf[T]
      case Success(Some(v)) =>
        assertEquals(expected, v)
        v

  def assertTry[T](expected: T)(value: Try[T]): T =
    value match
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[T]
      case Success(v) =>
        assertEquals(expected, v)
        v

  def assertSuccess[T](v: Try[T]): T =
    v match
      case Success(value) => value
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[T]

  def assertOptionSuccess[T](v: Try[Option[T]]): T =
    v match
      case Success(Some(value)) => value
      case Success(None) =>
        fail("value expected, but receive None")
        null.asInstanceOf[T]
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[T]

  def using[T: Releasable, A](res: T)(f: T => A)(using releasable: Releasable[T]): A =
    try
      f(res)
    catch
      case err: Throwable =>
        fail(err.getMessage)
        null.asInstanceOf[A]
    finally
      releasable.release(res)

  def usingTry[T: Releasable, A](t: Try[T])(f: T => A): A =
    t match
      case Success(value) => using(value)(f)
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[A]

