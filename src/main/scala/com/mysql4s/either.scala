package com.mysql4s

import scala.util.{Failure, NotGiven, Success, Try, boundary}
import boundary.{Label, break}

object asEither:
  import GetAble.Fail

  inline def apply[L, A](inline body: Label[Fail[L] | A] ?=> Fail[L] | A)(using ng : NotGiven[Fail[Nothing] <:< A],fe: ToEither[A]): Either[L | fe.L, fe.R] =
    boundary(body) match
      case Fail(value) =>
        Left(value.asInstanceOf[L])
      case success =>
        fe.toEither(success.asInstanceOf[A])

  extension [V, AV, FV](t: V)(using getable: GetAble[V] { type F = FV; type A = AV }, b: boundary.Label[Fail[FV]])
    /** Exits with `Fail` to next enclosing `getEither` boundary */
    //@targetName("questionMark")
    inline def ? : AV =
      getable.get(t) match
        case either: Fail[FV] =>
          break(either)
        case value =>
          value.asInstanceOf[AV]

/**
 * Type class for extracting boxed values while preserving failures
 *
 * @tparam T extractable type
 */
sealed trait GetAble[T]:
  import GetAble.Fail
  type F
  type A
  def get(value: T): Fail[F] | A

object GetAble:
  sealed case class Fail[+F](failure: F)

  transparent inline def apply[F](using GetAble[F]): GetAble[F] = summon[GetAble[F]]

  given opt[AV, O <: Option[AV]]: GetAble[O] with
    type F = Unit; type A = AV
    override def get(value: O): Fail[Unit] | AV = value.getOrElse(Fail(()))

  given tr[AV, T <: Try[AV]]: GetAble[T] with
    type F = Throwable; type A = AV
    override def get(value: T): Fail[Throwable] | AV = value.getOrElse(Fail(value.asInstanceOf[Failure[Nothing]].exception))

  given either[FV, AV, E <: Either[FV, AV]]: GetAble[E] with
    type F = FV; type A = AV
    override def get(value: E): Fail[FV] | AV = value match
      case value: Left[FV, AV] => Fail(value.value)
      case value: Right[FV, AV] => value.value

/**
 * Type class for converting different optional or failable types to
 * an `Either`.
 *
 * @tparam A value that is convertable to Either
 */
sealed trait ToEither[A]:
  type L
  type R
  def toEither(a: A): Either[L, R]

trait LowPriorityToEither:
  given any[A]: ToEither[A] with
    type L = Nothing; type R = A
    override def toEither(a: A): Either[Nothing, A] = Right(a)

object ToEither extends LowPriorityToEither:
  given opt[A, O <: Option[A]]: ToEither[O] with
    type L = Unit; type R = A
    def toEither(a: O): Either[Unit, A] =
      a.fold(Left(()))(Right.apply)

  given tr[A, T <: Try[A]]: ToEither[T] with
    type L = Throwable; type R = A
    override def toEither(a: T): Either[Throwable, A] =
      a.toEither

  given either[LV, RV, E <: Either[LV, RV]]: ToEither[E] with
    type L = LV; type R = RV

    override def toEither(a: E): Either[LV, RV] = a          