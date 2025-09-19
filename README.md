# mysql4s
Scala Native MySQL

### Example

```scala 3
import com.mysql4s.*
import scala.scalanative.unsafe.Zone

@main def main(): Unit =
    Zone:

        val conn = MySQL.connect(
            DBHost("127.0.0.1"),
            DBUser("test"),
            DBPassword("test"),
            DBDatabase("test"))

        conn.execute("drop table if exists users")

        conn.execute(
            """
            |create table users(
            |   id int auto_increment primary key,
            |   name varchar(50),
            |   username varchar(50)
            |)""".stripMargin)

        conn.execute(
            """
            |insert into users (name, username) values (?, ?)
            |""".stripMargin, "Ricardo", "ricardo@mobilemind")

        val row = conn.firstRow(
            "select name, username from users where id = ?", conn.lastInsertID)

        val name = row.map(_.getString("name"))
        val rows = conn.rows("select name, username from users")

        assertEquals("expect 1 row", 1, rows.size)
        assertEquals("expect ricardo", Some("Ricardo"), name)

```