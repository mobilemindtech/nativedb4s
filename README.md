# mysql4s
Scala Native MySQL

```scala 3
import com.mysql4s.*

val connection = MySQL.connectExn("127.0.0.1", "user", "passwd", "mydb")

mysql.execute("""
    |create table users(
    |   id int auto_increment primary key, 
    |   name varchar(50),
    |   username varchar(50)
    |)""".stripMargin)

mysql.execute(
  """
    |insert into mytable (name, username) values (?, ?)
    |""".stripMargin, "Ricardo", "ricardo@mobilemind")

def printRow(row: Row) =
  println(s"id=${it.getInt("id").get}, name=${it.getString("name").get}, username=${it.getString("username").get}")  

val lastID = mysql.lastInsertID
val rows = mysql.executeQuery("select name, username from users").get
rows.foreach(printRow)

val row = mysql.firstRow("select name, username from users where id = ?", lastID).get.get
printRow(row)

rows.close()
connection.close()

```