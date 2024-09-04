# mysql4s
Scala Native MySQL

### Example

```scala 3
import com.mysql4s.*

import scala.scalanative.unsafe.Zone
import scala.util.{Failure, Success, Try}

extension [T](op: Option[T])
  def toTry: Try[T] = op match
    case Some(value) => Success(value)
    case None => Failure(Exception("none"))

@main def main(): Unit =
    val _ =
      Zone:
        UsingTryIn(MySQL.connect("127.0.0.1", "test", "test", "test")):
          conn =>
            for
              _ <- conn.execute("drop table if exists users")
              _ <- conn.execute("""
                  |create table users(
                  |   id int auto_increment primary key,
                  |   name varchar(50),
                  |   username varchar(50)
                  |)""".stripMargin)
              _ <- conn.execute(
                "insert into users (name, username) values (?, ?)", "Ricardo", "ricardo@mobilemind")
              rows <- conn.rows("select name, username from users")
              row <- conn.firstRow("select name, username from users where id = ?", conn.lastInsertID)
              name <- row.toTry.map(_.getString("name"))
              _ = println(s"count=${rows.size}, name=${name}")
              _ = conn.close()
            yield ()

```