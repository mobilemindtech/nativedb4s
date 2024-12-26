package io.mysql4s

import io.mysql4s.MySqlException.exn
import io.mysql4s.bindings.enumerations.enum_field_types
import io.mysql4s.bindings.extern_functions.{mysql_stmt_errno, mysql_stmt_error}
import io.mysql4s.bindings.structs.MYSQL_STMT
import scala.util.Using.Releasable
import scala.scalanative.unsafe.{CString, Ptr, Zone, fromCString, toCString}
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.time.{LocalDate, LocalDateTime, LocalTime}

extension[A, B] (a: A)
  infix def |>(f: A => B): B = f(a)

extension (s: String)(using Zone)
  def c_str(): CString = toCString(s)

def toStr(value: CString): String = fromCString(value)

type WithZone[T] = Zone ?=> T
type TryWithZone[T] = WithZone[Try[T]]


private[mysql4s] def isMysqlString(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_STRING |
         enum_field_types.MYSQL_TYPE_VAR_STRING |
         enum_field_types.MYSQL_TYPE_VARCHAR => true
    case _ => false

private[mysql4s] def isMysqlBytes(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_TINY_BLOB |
         enum_field_types.MYSQL_TYPE_BLOB |
         enum_field_types.MYSQL_TYPE_MEDIUM_BLOB |
         enum_field_types.MYSQL_TYPE_LONG_BLOB => true
    case _ => false

private[mysql4s] def isMysqlDecimal(typ: enum_field_types): Boolean =
  typ == enum_field_types.MYSQL_TYPE_DECIMAL || typ == enum_field_types.MYSQL_TYPE_NEWDECIMAL




type ScalaTypes = String | Int | Short | Long | Float | Double | Boolean | Array[Byte] | LocalDate | LocalTime | LocalDateTime


extension (x: Any)
  inline def discard: Unit = ()

def ignore[T](a: => T): Unit =
  val _ = a

private[mysql4s]  def collectStmtExn(message: String, stmt: Ptr[MYSQL_STMT]): MySqlException =
  val code = mysql_stmt_errno(stmt)
  val error = mysql_stmt_error(stmt)
  exn(s"$message. Error: ${toStr(error)}", code.toInt)

def UsingTryInOut[R : Releasable, A](res: Try[R])(f: R => Try[A])(using releasable: Releasable[R]): Try[A] =
  res match
    case Success(r) =>  
      try
        f(r)
      finally
        releasable.release(r)
    case Failure(exception) => Failure(exception)

def UsingTryIn[R: Releasable, A](res: Try[R])(f: R => A)(using releasable: Releasable[R]): A =
  res match
    case Success(r) =>
      try
        f(r)
      finally
        releasable.release(r)
    case Failure(exception) => throw exception

def UsingTryOut[R : Releasable, A](res: R)(f: R => Try[A])(using releasable: Releasable[R]): Try[A] =
  try
    f(res)
  finally
    releasable.release(res)


class QueryResult[T](rawValues: Map[String, Any]) extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], Option]
  def selectDynamic(fieldName: String) = rawValues.get(fieldName)  