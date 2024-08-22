





@main def main(agrs: String*): Unit =
  ()

  /*
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
                        s"""
                          |INSERT INTO customer
                          |   (${fields.mkString(", ")})
                          |   values (${fields.map(_ => "?").mkString(", ")})
                          |""".stripMargin, values*)

        prepared <- mysql.prepare(s"select ${fields.mkString(", ")} from customer where id = ?")
        rows <-
          prepared.setInt(0, 1)
          prepared.executeQuery()

        rs <- mysql.realQuery(s"select ${fields.mkString(", ")} from customer where id = 1")

      yield (count, rows, rs))

    result match
      case Success((len, rows, rs)) =>
        println(s"insert count = $len")
        println(s"select count = ${rows.count}")
        rows.next() match
          case Success(Some(row)) =>
            println(s"select id = ${row.getInt("id")}")
            println(s"select first_name = ${row.getString("first_name")}")
            println(s"select last_name = ${row.getString("last_name")}")
            println(s"select birth_date = ${row.getString("birth_date")}")
            println(s"select sales_limit = ${row.getDouble("sales_limit")}")
            println(s"select enabled = ${row.getBoolean("enabled")}")
            println(s"select len = ${row.getInt("len")}")
            println(s"select gender = ${row.getString("gender")}")
          case Success(None) =>
            println(s"select none")
          case err =>
            println(s"select err ${err}")

        println("-----------------------")
        rs.next() match
          case Success(None) =>
            println("rs none")
          case Success(Some(row)) =>
            println(s"select id = ${row.getInt("id")}")
            println(s"select first_name = ${row.getString("first_name")}")
            println(s"select last_name = ${row.getString("last_name")}")
            println(s"select birth_date = ${row.getString("birth_date")}")
            println(s"select sales_limit = ${row.getDouble("sales_limit")}")
            println(s"select enabled = ${row.getBoolean("enabled")}")
            println(s"select len = ${row.getInt("len")}")
            println(s"select gender = ${row.getString("gender")}")

      case Failure =>
        println(s"result = $result")
  */
