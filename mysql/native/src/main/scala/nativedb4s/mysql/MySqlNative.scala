package nativedb4s.mysql

import io.nativedb4s.api.conn.Connection
import io.nativedb4s.api.core.*
import io.nativedb4s.mysql.conn.ConnectionImpl
export io.nativedb4s.mysql.rs.{getAs, getOrNull}
export io.nativedb4s.mysql.stmt.setAs

import scala.scalanative.unsafe.Zone

class MySqlNative(using Zone) extends NativeDB:
  /**
   * Create new MySQL connection
   *
   * @param host     Connection host
   * @param user     Connection user
   * @param password Connection password
   * @param database Connection database name
   * @param port     Connection port
   * @return A closeable MySQL connection or throw a exception
   */
  def connect(host: DBHost,
              user: DBUser,
              password: DBPassword,
              database: DBDatabase,
              port: DBPort = DBPort(3306)): Connection =
    val mysql = new ConnectionImpl()
    mysql.connect(
      host.host,
      user.user,
      password.password,
      database.database,
      port.port)
    mysql

object MySqlNative:
  def connect(host: DBHost,
              user: DBUser,
              password: DBPassword,
              database: DBDatabase,
              port: DBPort = DBPort(3306)): Zone ?=> Connection =
    new MySqlNative().connect(host, user, password, database, port)