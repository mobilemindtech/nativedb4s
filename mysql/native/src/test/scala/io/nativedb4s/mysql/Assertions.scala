package io.nativedb4s.mysql

import org.junit.Assert.*

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try}

object Assertions:

  def assertF[T](expected: T)(value: T): T =
    assertEquals(expected, value)
    value

  def assertOption[T <: Matchable](expected: T)(value: Option[T]): T =
    value match
      case None =>
        fail(s"$value != $expected")
        null.asInstanceOf[T]
      case Some(v: Array[Byte]) =>
        assertArrayEquals(
          expected.asInstanceOf[Array[Byte]],
          v.asInstanceOf[Array[Byte]]
        )
        null.asInstanceOf[T]
      case Some(v) =>
        assertEquals(expected, v)
        null.asInstanceOf[T]

  def assertOptionDate[T <: LocalDate | LocalTime | LocalDateTime](expected: T)(
      value: Option[T]
  ): T =
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
