package io.nativedb4s.mysql

import org.junit.Assert.*
import org.junit.*

import scala.compiletime.uninitialized
import scala.scalanative.unsafe.Zone
import java.time.{LocalDate, LocalDateTime, LocalTime}
import nativedb4s.*
import nativedb4s.mysql.*
import syntax.*
import Assertions.*

case class User(
    id: Int,
    name: String,
    username: String,
    password: Boolean,
    enabled: Boolean
)

object MySqlTest:

  given zone: Zone = Zone.open()
  var connection: Connection = uninitialized

  @BeforeClass
  def beforeAll(): Unit =
    connection = MySqlNative.connect(
      DBHost("127.0.0.1"),
      DBUser("test"),
      DBPassword("test"),
      DBDatabase("test")
    )

  @AfterClass
  def afterAll(): Unit =
    connection.close()
    zone.close()

  def cleanup(mysql: Connection)(using Zone) =
    mysql.execute("DROP TABLE IF EXISTS mysql_types")
    mysql.execute("DROP TABLE IF EXISTS user")

  def setup(mysql: Connection)(using Zone) =
    mysql.execute("""
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

  @Before
  def beforeEach(): Unit =
    Zone:
      val _ = setup(connection)

  @After
  def afterEach(): Unit =
    Zone:
      val _ = cleanup(connection)

  @Test
  def checkMySqlTypes(): Unit =

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
    val float_value = 35.31f
    val double_value = 555456.91
    val decimal_value = 5748754.78
    val boolean_value = true
    val tiny_blob_value = Array[Byte]('A', 'B', 'C', 'D')
    val time_value = LocalTime.now()
    val date_value = LocalDate.now()
    val datetime_value = LocalDateTime.now()
    val timestamp_value = LocalDateTime.now()

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
      "timestamp_value"
    )

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
      timestamp_value
    )

    val mysql = connection

    for _ <- 0 `until` 5 do
      mysql.execute(
        s"""
           |INSERT INTO mysql_types
           |   (${fields.mkString(", ")})
           |   values (${fields.map(_ => "?").mkString(", ")})
           |""".stripMargin,
        values*
      ) |> assertF(1)

    val rs = mysql.executeQuery(
      s"""
         |SELECT
         |   id, ${fields.mkString(", ")}
         |FROM
         |   mysql_types
         |""".stripMargin
    )
    assertEquals(5, rs.count)

    val seqRows =
      mysql.rows(
        s"""
          |SELECT
          |   id, ${fields.mkString(", ")}
          |FROM
          |   mysql_types
          |""".stripMargin
      )

    assertEquals(5, seqRows.size)

    mysql.firstRow(
      s"""
        |SELECT
        |   id, ${fields.mkString(", ")}
        |FROM
        |   mysql_types
        |""".stripMargin
    ) match
      case Some(row: RowResult) =>
        row.getString("varchar_value") |> assertOption(varchar_value)
      case _ => fail("row expected")

    val stmt = mysql.prepare(s"""
        |SELECT
        |   id, ${fields.mkString(", ")}
        |FROM
        |   mysql_types
        |WHERE id = ?
        |""".stripMargin)

    val lastID = mysql.lastInsertID

    assert(lastID > 0)

    val rs1 =
      stmt
        .setAs(0, lastID)
        .executeQuery()

    rs1.foreach(_.getString("varchar_value") |> println)

    val row = rs1.first.get

    var index = 0
    def inc: Int =
      index = index + 1
      index

    row.getAs[Int]("id") |> assertOption(lastID)
    row.getAs[String](fields(index)) |> assertOption(values(index)) // varchar
    row.getAs[String](fields(inc)) |> assertOption(values(index)) // char

    row.getAs[String](fields(inc)) |> assertOption(values(index)) // tinytext
    row.getAs[String](fields(inc)) |> assertOption(values(index)) // text
    row.getAs[String](fields(inc)) |> assertOption(values(index)) // mediumtext
    row.getAs[String](fields(inc)) |> assertOption(values(index)) // longtext

    row.getAs[Short](fields(inc)) |> assertOption(values(index))
    row.getAs[Short](fields(inc)) |> assertOption(values(index))
    row.getAs[Int](fields(inc)) |> assertOption(values(index))
    row.getAs[Long](fields(inc)) |> assertOption(values(index))
    row.getAs[Float](fields(inc)) |> assertOption(values(index))
    row.getAs[Double](fields(inc)) |> assertOption(values(index))
    row.getAs[Double](fields(inc)) |> assertOption(values(index)) // decimal
    row.getAs[Boolean](fields(inc)) |> assertOption(values(index))

    row.getAs[Array[Byte]](fields(inc)) |> assertOption(values(index)) // binary

    row.getTime(fields(inc)) |> assertOptionDate(
      values(index).asInstanceOf[LocalTime]
    ) // time
    row.getDate(fields(inc)) |> assertOptionDate(
      values(index).asInstanceOf[LocalDate]
    ) // date
    row.getDateTime(fields(inc)) |> assertOptionDate(
      values(index).asInstanceOf[LocalDateTime]
    ) // datetime
    row.getDateTime(fields(inc)) |> assertOptionDate(
      values(index).asInstanceOf[LocalDateTime]
    ) // timestamp

    ()
  end checkMySqlTypes

  @Test
  def testUnsafeOperations(): Unit =

    connection.execute("""
        |create table user (
        |   id int primary key auto_increment,
        |   name varchar(50) not null,
        |   username varchar(50) not null,
        |   `password` varchar(50) not null,
        |   enabled tinyint not null)
        |""".stripMargin)

    connection.execute("""
        |insert into user (name, username, `password`, enabled)
        |values ('Ricardo Bocchi', 'ricardo', '@1234', true)
        |""".stripMargin)

    connection.firstRowAs[User](
      "select * from user where username = 'ricardo'"
    ) match
      case Some(row) =>
        assertTrue(
          s"expected name, but ${row.name}",
          row.name.contains("Ricardo Bocchi")
        )
        assertTrue(
          s"expected username, but ${row.username}",
          row.username.contains("ricardo")
        )
        assertTrue(
          s"expected enabled, but ${row.enabled}",
          row.enabled.contains(true)
        )
      case _ =>
        fail("query result expected")

  end testUnsafeOperations
