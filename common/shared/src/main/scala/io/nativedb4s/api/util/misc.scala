package io.nativedb4s.api.util

import java.time.{LocalDate, LocalDateTime, LocalTime}

extension [A, B](a: A) infix def |>(f: A => B): B = f(a)

extension (x: Any) inline def discard: Unit = ()

def ignore[T](a: => T): Unit =
  val _ = a

type ScalaTypes = String | Int | Short | Long | Float | Double | Boolean |
  Array[Byte] | LocalDate | LocalTime | LocalDateTime

class QueryResult[T](rawValues: Map[String, Any]) extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], Option]
  def selectDynamic(fieldName: String) = rawValues.get(fieldName)
