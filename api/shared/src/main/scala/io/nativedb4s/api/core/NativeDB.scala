package io.nativedb4s.api.core

import io.nativedb4s.api.conn.Connection

opaque type DBHost = String
opaque type DBUser = String
opaque type DBPassword = String
opaque type DBDatabase = String
opaque type DBPort = Int

object DBHost:
  def apply(s: String): DBHost = s

extension(self: DBHost)
  def host: String = self

object DBUser:
  def apply(s: String): DBUser = s

extension(self: DBUser)
  def user: String = self

object DBPassword:
  def apply(s: String): DBPassword = s

extension(self: DBPassword)
  def password: String = self

object DBDatabase:
  def apply(s: String): DBDatabase = s

extension(self: DBDatabase)
  def database: String = self

object DBPort:
  def apply(s: Int): DBPort = s

extension(self: DBPort)
  def port: Int = self

trait NativeDB:
  def connect(host: DBHost,
              user: DBUser,
              password: DBPassword,
              database: DBDatabase,
              port: DBPort): Connection