package com.mysql4s

import org.junit.{Before, After, BeforeClass, AfterClass, Test}
import org.junit.Assert.*
import scala.compiletime.uninitialized
import scala.scalanative.unsafe.Zone
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}

object MySQLTest:

  given zone: Zone = Zone.open()
  var connection: Connection = uninitialized

  @BeforeClass
  def beforeAll(): Unit =
    connection = MySQL.connectExn("127.0.0.1", "test", "test", "test")

  @AfterClass
  def afterAll(): Unit =
    connection.close()
    zone.close()

class MySQLTest:

  import MySQLTest.{connection, given }

  def cleanup(mysql: Connection)(using Zone) =
    mysql.execute("DROP TABLE IF EXISTS mysql_types")

  def setup(mysql: Connection)(using Zone) =
    mysql.execute(
      """
        |CREATE TABLE mysql_types (
        |   id int auto_increment primary key,
        |   varchar_value varchar(50) not null,
        |   char_value char(50) not null,
        |   tinyint_value tinyint not null,
        |   smallint_value smallint not null,
        |   int_value int not null,
        |   bigint_value bigint not null,
        |   float_value float not null,
        |   double_value double not null,
        |   decimal_value decimal(12,2) not null
        |)
        |""".stripMargin)

  def assertT[T](expected: T)(value: T) =
    assertEquals(expected, value)

  def assertOption[T](expected: T)(value: Option[T]): T =
    value match
      case None =>
        fail(s"$value != $expected")
        null.asInstanceOf[T]
      case Some(v) =>
        assertEquals(expected, v)
        null.asInstanceOf[T]

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

  def using[T : Releasable, A](res: T)(f: T => A)(using releasable: Releasable[T]): A =
    try
      f(res)
    catch
      case err: Throwable =>
        fail(err.getMessage)
        null.asInstanceOf[A]
    finally
      releasable.release(res)

  def usingTry[T : Releasable, A](t: Try[T])(f: T => A): A =
    t match
      case Success(value) => using(value)(f)
      case Failure(err) =>
        fail(err.getMessage)
        null.asInstanceOf[A]

  @Before
  def beforeEach(): Unit =
    val  _ = setup(connection) |> assertSuccess

  @After
  def afterEach(): Unit =
    val _ = cleanup(connection) |> assertSuccess

  @Test
  def checkMySqlTypes() =

    val varchar_value = "varchar value"
    val char_value = "char value"
    val tinyint_value = 3.toShort
    val smallint_value = 4.toShort
    val int_value = 467
    val bigint_value = 3456L
    val float_value = 35.31F
    val double_value = 555456.91
    val decimal_value = 5748754.78

    val fields = List("varchar_value", "char_value", "tinyint_value", "smallint_value", "int_value", "bigint_value", "float_value", "double_value", "decimal_value")
    val values = List(varchar_value, char_value, tinyint_value, smallint_value, int_value, bigint_value, float_value, double_value, decimal_value)

    val mysql = connection

    mysql.execute(
      s"""
         |INSERT INTO mysql_types
         |   (${fields.mkString(", ")})
         |   values (${fields.map(_ => "?").mkString(", ")})
         |""".stripMargin, values *) |> assertTry(1)

    val stmt = mysql.prepare(
      s"""
        |SELECT
        |   id, ${fields.mkString(", ")}
        |FROM
        |   mysql_types
        |WHERE id = ?
        |""".stripMargin)

    usingTry(stmt):
      prepared =>
        val rs =
          prepared
            .setAs(0, 1)
            .executeQuery() |> assertSuccess
        val row = rs.first |> assertOptionSuccess

        //val fields = List(
        // "varchar_value",
        // "char_value",
        // "tinyint_value",
        // "smallint_value",
        // "int_value",
        // "bigint_value",
        // "float_value",
        // "double_value",
        // "decimal_value")
        row.getAs[Int]("id") |> assertOption(1)
        row.getAs[String](fields(0)) |> assertOption(values(0))
        row.getAs[String](fields(1)) |> assertOption(values(1))
        row.getAs[Short](fields(2)) |> assertOption(values(2))
        row.getAs[Short](fields(3)) |> assertOption(values(3))
        row.getAs[Int](fields(4)) |> assertOption(values(4))
        row.getAs[Long](fields(5)) |> assertOption(values(5))
        row.getAs[Float](fields(6)) |> assertOption(values(6))
        row.getAs[Double](fields(7)) |> assertOption(values(7))
        row.getAs[Double](fields(8)) |> assertOption(values(8))
    |> ignore

  end checkMySqlTypes


