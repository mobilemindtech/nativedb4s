package io.mysql4s

/**
  * MySQL Exception
  *
  * @param message
  * @param code
  */
class MySqlException(message: String, val code: Int) extends Exception(message)

object MySqlException:
  def apply(message: String, code: Int = 0) = new MySqlException(message, code)

  def exn(message: String, code: Int = 0) = MySqlException(message, code)