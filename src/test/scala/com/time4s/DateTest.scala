package com.time4s

import org.junit.Test
import org.junit.Assert.*
import scala.scalanative.unsafe.Zone

class DateTest:

  @Test
  def dateTest() =
    Zone:

      // Thu Aug 22 2024 10:19:11 GMT-0300
      val dateMillis = 1724332751407L

      //val date = Date.now
      //assertEquals(2024, date.getYear)

      val date = Date(dateMillis)
      assertEquals(2024, date.year)
      assertEquals(8, date.month)
      assertEquals(22, date.day)
      assertEquals(10, date.hours)
      assertEquals(19, date.minutes)
      assertEquals(11, date.seconds)
      assertEquals(-3, date.timeOffset)



