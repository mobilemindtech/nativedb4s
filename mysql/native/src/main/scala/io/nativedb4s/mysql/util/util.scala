package io.nativedb4s.mysql.util

import io.nativedb4s.api.types.{MySqlException, exn}
import io.nativedb4s.mysql.bindings.enumerations.enum_field_types
import io.nativedb4s.mysql.bindings.extern_functions.{mysql_stmt_errno, mysql_stmt_error}
import io.nativedb4s.mysql.bindings.structs.MYSQL_STMT

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.scalanative.unsafe.{CString, Ptr, Zone, fromCString, toCString}

extension (s: String)(using Zone)
  def c_str(): CString = toCString(s)

def toStr(value: CString): String = fromCString(value)

private[nativedb4s] def isMysqlString(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_STRING |
         enum_field_types.MYSQL_TYPE_VAR_STRING |
         enum_field_types.MYSQL_TYPE_VARCHAR => true
    case _ => false

private[nativedb4s] def isMysqlBytes(typ: enum_field_types): Boolean =
  typ match
    case enum_field_types.MYSQL_TYPE_TINY_BLOB |
         enum_field_types.MYSQL_TYPE_BLOB |
         enum_field_types.MYSQL_TYPE_MEDIUM_BLOB |
         enum_field_types.MYSQL_TYPE_LONG_BLOB => true
    case _ => false

private[nativedb4s] def isMysqlDecimal(typ: enum_field_types): Boolean =
  typ == enum_field_types.MYSQL_TYPE_DECIMAL || typ == enum_field_types.MYSQL_TYPE_NEWDECIMAL

private[nativedb4s]  def collectStmtExn(message: String, stmt: Ptr[MYSQL_STMT]): MySqlException =
  val code = mysql_stmt_errno(stmt)
  val error = mysql_stmt_error(stmt)
  exn(s"$message. Error: ${toStr(error)}", code.toInt)