package com.time4s

import com.time4s.TIME.*

import scala.scalanative.posix.time
import scala.scalanative.posix.time.{localtime, mktime, time_t, tm}
import scala.scalanative.unsafe.{CInt, Ptr, Tag, UnsafeRichLong, Zone, alloc, extern}


object TIME:
  given _tag: Tag[tm] = Tag.materializeCStruct9Tag[CInt, CInt, CInt, CInt, CInt, CInt, CInt, CInt, CInt]
  def apply()(using Zone): Ptr[tm] = scala.scalanative.unsafe.alloc[tm](1)
  def apply(year : CInt, month : CInt, day : CInt, hour : CInt, minute : CInt, second : CInt, weekDay: CInt, yearDay: CInt, isdst: CInt)(using Zone): Ptr[tm] =
    val ____ptr = apply()
    (!____ptr).year = year
    (!____ptr).month = month
    (!____ptr).day = day
    (!____ptr).hour = hour
    (!____ptr).minute = minute
    (!____ptr).second = second
    (!____ptr).weekDay =  weekDay
    (!____ptr).yearDay = yearDay
    (!____ptr).isdst = isdst
    ____ptr
  extension (struct: tm)
    def year : CInt = struct._6
    def year_=(value: CInt): Unit = !struct.at6 = value
    def month: CInt = struct._5
    def month_=(value: CInt): Unit = !struct.at5 = value
    def day: CInt = struct._4
    def day_=(value: CInt): Unit = !struct.at4 = value
    def hour: CInt = struct._3
    def hour_=(value: CInt): Unit = !struct.at3 = value
    def minute: CInt = struct._2
    def minute_=(value: CInt): Unit = !struct.at2 = value
    def second: CInt = struct._1
    def second_=(value: CInt): Unit = !struct.at1 = value
    def weekDay: CInt = struct._7
    def weekDay_=(value: CInt): Unit = !struct.at7 = value
    def yearDay: CInt = struct._8
    def yearDay_=(value: CInt): Unit = !struct.at8 = value
    def isdst: CInt = struct._9
    def isdst_=(value: CInt): Unit = !struct.at9 = value


class Date(tmPtr: Ptr[tm]):
  val day: Int = tmPtr(0).day
  val month: Int = tmPtr(0).month + 1
  val year: Int = tmPtr(0).year + 1900
  val hours: Int = tmPtr(0).hour
  val minutes: Int = tmPtr(0).minute
  val seconds: Int = tmPtr(0).second
  val millisec: Long = mktime(tmPtr) * 1000L
  val weekDay: Int = tmPtr(0).weekDay + 1
  val yearDay: Int = tmPtr(0).yearDay + 1
  def toDate: java.util.Date = new java.util.Date(millisec)
  def timeOffset: Int = util.time_offset()

@extern
object util:
  def time_offset(): Int = extern

object Date:
  def apply(dt: java.util.Date): Zone ?=> Date =
    Date(dt.getTime)

  def apply(mills: Long): Zone ?=> Date =
    val rawtime = alloc[time_t]()
    !rawtime =  if mills > 0 then (mills / 1000).toSize else 0.toSize
    if mills == 0 then
      val _ = time.time(rawtime)
    val ptm = localtime(rawtime)
    Date(ptm)

  def apply(tm: Ptr[tm]): Zone ?=> Date =
    new Date(tm)

  def now: Zone ?=> Date = Date(0L)


