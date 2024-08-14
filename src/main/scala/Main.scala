import scala.scalanative.unsafe.Zone
import scala.util.{Failure, Success}
import com.mysql4s.*

def cleanup(mysql: MySql)(using Zone) =
  mysql.execute("DROP TABLE IF EXISTS customer")

def setup(mysql: MySql)(using Zone) =
  mysql.execute(
    """
      |CREATE TABLE customer (
      |   id int auto_increment primary key,
      |   first_name varchar(50) not null,
      |   last_name varchar(50) not null,
      |   gender varchar(1) not null,
      |   birth_date varchar(20) not null,
      |   sales_limit decimal(12,2) not null,
      |   enabled bool not null,
      |   len int not null
      |)
      |""".stripMargin)

@main def main(agrs: String*): Unit =

  println("main")
  val mysql = MySql()
  Zone:
    val result =
      (for
        _ <- mysql.connect(
              host = "127.0.0.1",
              user = "test",
              password = "test",
              database = "test",
            )
        _ <- cleanup(mysql)
        _ <- setup(mysql)
        _ <- mysql.setAutoCommit(true)
        count <-   mysql.execute(
                        """
                          |INSERT INTO customer
                          |   (first_name, last_name, gender, birth_date, sales_limit, enabled, len)
                          |   values (?, ?, ?, ?, ?, ?, ?)
                          |""".stripMargin, "Ricardo", "Bocchi", "M", "1986-11-27", 500.0, true, 5)

        prepared <- mysql.prepare("select * from customer where id = ?")
        rows <-
          prepared.setInt(0, 1)
          prepared.executeQuery()

      yield (count, rows))

    result match
      case Success((len, rows)) =>
        println(s"insert count = $len")
        println(s"select count = ${rows.count}")
        rows.next() match
          case Success(Some(row)) =>
            println(s"select id = ${row.getInt("id")}")
            println(s"select name = ${row.getString("first_name")}")
            println(s"select last_name = ${row.getString("last_name")}")
            println(s"select gender = ${row.getString("gender")}")
          case Success(None) =>
            println(s"select none")
          case err =>
            println(s"select err ${err}")
      case Failure =>
        println(s"result = $result")

