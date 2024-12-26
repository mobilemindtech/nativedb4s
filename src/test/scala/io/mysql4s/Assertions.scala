package io.mysql4s

import org.junit.Assert.*

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

object Assertions:


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

  def assertOptionDate[T <: LocalDate | LocalTime | LocalDateTime](expected: T)(value: Option[T]): T =
    value match
      case None =>
        fail(s"$value != $expected")
        null.asInstanceOf[T]
      case Some(date) =>

        (expected, date) match
          case (v1: LocalTime, v2: LocalTime) =>
            assertEquals("assert hours", v1.getHour, v2.getHour)
            assertEquals("assert minutes", v1.getMinute, v2.getMinute)
            assertEquals("assert seconds", v1.getSecond, v2.getSecond)
          case (v1: LocalDate, v2: LocalDate) =>
            assertEquals("assert day", v1.getDayOfMonth, v2.getDayOfMonth)
            assertEquals("assert month", v1.getMonthValue, v2.getMonthValue)
            assertEquals("assert year", v1.getYear, v2.getYear)
          case (v1: LocalDateTime, v2: LocalDateTime) =>
            assertEquals("assert hours", v1.getHour, v2.getHour)
            assertEquals("assert minutes", v1.getMinute, v2.getMinute)
            assertEquals("assert seconds", v1.getSecond, v2.getSecond)
            assertEquals("assert day", v1.getDayOfMonth, v2.getDayOfMonth)
            assertEquals("assert month", v1.getMonthValue, v2.getMonthValue)
            assertEquals("assert year", v1.getYear, v2.getYear)
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

