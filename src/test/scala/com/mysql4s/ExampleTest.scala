package com.mysql4s

import org.junit.Test
import org.junit.Assert.*

import scala.scalanative.unsafe.Zone
import scala.util.{Failure, Success, Try}

extension [T](op: Option[T])
  def toTry: Try[T] = op match
    case Some(value) => Success(value)
    case None => Failure(Exception("none"))

class ExampleTest:

  @Test
  def simpleQuery(): Unit =
    val result =
      Zone:
        UsingTryIn(MySQL.connect("127.0.0.1", "test", "test", "test")):
          conn =>
            for
              _ <- conn.execute("drop table if exists users")
              _ <- conn.execute(
                      """
                        |create table users(
                        |   id int auto_increment primary key,
                        |   name varchar(50),
                        |   username varchar(50)
                        |)""".stripMargin)
              _ <- conn.execute(
                        """
                          |insert into users (name, username) values (?, ?)
                          |""".stripMargin, "Ricardo", "ricardo@mobilemind")
              row <- conn.firstRow(
                "select name, username from users where id = ?", conn.lastInsertID)
              name <- row.toTry.map(_.getString("name"))
              rows <- conn.rows("select name, username from users")
              _ = assertEquals("expect 1 row", 1, rows.size)
              _ = assertEquals("expect ricardo", Some("Ricardo"), name)
              _ = conn.close()
            yield ()
    result match
      case Failure(exception) => fail(exception.getMessage)
      case _ => ()
