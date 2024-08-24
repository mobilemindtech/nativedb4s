package com.mysql4s

import org.junit.{After, AfterClass, Before, BeforeClass, Test}
import org.junit.Assert.*

import scala.compiletime.{asMatchable, uninitialized}
import scala.scalanative.unsafe.Zone
import scala.util.Using.Releasable
import scala.util.{Failure, Success, Try, Using}
import MySQLTest.MysqlDateType.*
import com.mysql4s.MySQLTest.MysqlDateType
import com.mysql4s.rs.RowResultSet
import com.mysql4s.stmt.PreparedStatement
import com.time4s.Date

object MySQLTest:

  enum MysqlDateType:
    case MTime, MDate, MDateTime, MTimestamp

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
        |CREATE TABLE IF NOT EXISTS mysql_types (
        |   id int auto_increment primary key,
        |   varchar_value varchar(50) not null,
        |   char_value char(50) not null,
        |   tiny_text_value tinytext not null,
        |   text_value text not null,
        |   medium_text_value mediumtext not null,
        |   long_text_value longtext not null,
        |   tiny_int_value tinyint not null,
        |   small_int_value smallint not null,
        |   int_value int not null,
        |   bigint_value bigint not null,
        |   float_value float not null,
        |   double_value double not null,
        |   decimal_value decimal(12,2) not null,
        |   boolean_value boolean not null,
        |   tiny_blob_value tinyblob not null,
        |   time_value time not null,
        |   date_value date not null,
        |   datetime_value datetime not null,
        |   timestamp_value timestamp not null
        |)
        |""".stripMargin)

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
            assertEquals("assert hours",expected.getDate.hours, date.hours)
            assertEquals("assert minutes",expected.getDate.minutes, date.minutes)
            assertEquals("assert seconds",expected.getDate.seconds, date.seconds)
          case MDate =>
            assertEquals("assert day",expected.getDate.day, date.day)
            assertEquals("assert month",expected.getDate.month, date.month)
            assertEquals("assert year",expected.getDate.year, date.year)
          case MDateTime =>
            assertEquals("assert hours",expected.getDate.hours, date.hours)
            assertEquals("assert minutes",expected.getDate.minutes, date.minutes)
            assertEquals("assert seconds",expected.getDate.seconds, date.seconds)
            assertEquals("assert day",expected.getDate.day, date.day)
            assertEquals("assert month",expected.getDate.month, date.month)
            assertEquals("assert year",expected.getDate.year, date.year)
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
    val tiny_text_value = "tiny text value"
    val text_value = "text value"
    val medium_text_value = "medium text value"
    val long_text_value = "long text value"
    val tiny_int_value = 3.toShort
    val small_int_value = 4.toShort
    val int_value = 467
    val bigint_value = 3456L
    val float_value = 35.31F
    val double_value = 555456.91
    val decimal_value = 5748754.78
    val boolean_value = true
    val tiny_blob_value = Array[Byte]('A', 'B', 'C', 'D')
    val time_value = MysqlTime.now
    val date_value = MysqlDate.now
    val datetime_value = MysqlDateTime.now
    val timestamp_value = MysqlTimestamp.now


    val fields = List(
      "varchar_value",
      "char_value",
      "tiny_text_value",
      "text_value",
      "medium_text_value",
      "long_text_value",
      "tiny_int_value",
      "small_int_value",
      "int_value",
      "bigint_value",
      "float_value",
      "double_value",
      "decimal_value",
      "boolean_value",
      "tiny_blob_value",
      "time_value",
      "date_value",
      "datetime_value",
      "timestamp_value")

    val values = List(
      varchar_value,
      char_value,
      tiny_text_value,
      text_value,
      medium_text_value,
      long_text_value,
      tiny_int_value,
      small_int_value,
      int_value,
      bigint_value,
      float_value,
      double_value,
      decimal_value,
      boolean_value,
      tiny_blob_value,
      time_value,
      date_value,
      datetime_value,
      timestamp_value)

    val mysql = connection

    for _ <- 0 until 5 do
      mysql.execute(
        s"""
           |INSERT INTO mysql_types
           |   (${fields.mkString(", ")})
           |   values (${fields.map(_ => "?").mkString(", ")})
           |""".stripMargin, values *) |> assertTry(1)

    val stmtAll = mysql.executeQuery(
      s"""
         |SELECT
         |   id, ${fields.mkString(", ")}
         |FROM
         |   mysql_types
         |""".stripMargin
    )
    usingTry(stmtAll):
      rs =>
        assertEquals(5, rs.count)

    val stmt = mysql.prepare(
      s"""
        |SELECT
        |   id, ${fields.mkString(", ")}
        |FROM
        |   mysql_types
        |WHERE id = ?
        |""".stripMargin)

    val lastID = mysql.lastInsertID

    assert(lastID > 0)

    val _ = usingTry(stmt):
      prepared =>
        val rs =
          prepared
            .setAs(0, lastID)
            .executeQuery() |> assertSuccess

        rs.foreach(_.getString("varchar_value") |> println)

        val row = rs.first |> assertOptionSuccess

        var index = 0
        def inc: Int =
          index = index + 1
          index

        row.getAs[Int]("id") |> assertOption(lastID)
        row.getAs[String](fields(index)) |> assertOption(values(index)) //varchar
        row.getAs[String](fields(inc)) |> assertOption(values(index)) //char

        row.getAs[String](fields(inc)) |> assertOption(values(index)) //tinytext
        row.getAs[String](fields(inc)) |> assertOption(values(index)) //text
        row.getAs[String](fields(inc)) |> assertOption(values(index)) //mediumtext
        row.getAs[String](fields(inc)) |> assertOption(values(index)) //longtext

        row.getAs[Short](fields(inc)) |> assertOption(values(index))
        row.getAs[Short](fields(inc)) |> assertOption(values(index))
        row.getAs[Int](fields(inc)) |> assertOption(values(index))
        row.getAs[Long](fields(inc)) |> assertOption(values(index))
        row.getAs[Float](fields(inc)) |> assertOption(values(index))
        row.getAs[Double](fields(inc)) |> assertOption(values(index))
        row.getAs[Double](fields(inc)) |> assertOption(values(index)) //decimal
        row.getAs[Boolean](fields(inc)) |> assertOption(values(index))

        row.getAs[Array[Byte]](fields(inc)) |> assertOption(values(index)) // binary

        row.getTime(fields(inc)) |> assertOptionDate(values(index).asInstanceOf[MyDate], MTime) // time
        row.getDate(fields(inc)) |> assertOptionDate(values(index).asInstanceOf[MyDate], MDate) // date
        row.getDateTime(fields(inc)) |> assertOptionDate(values(index).asInstanceOf[MyDate], MDateTime) // datetime
        row.getTimestamp(fields(inc)) |> assertOptionDate(values(index).asInstanceOf[MyDate], MTimestamp) // timestamp

  end checkMySqlTypes


