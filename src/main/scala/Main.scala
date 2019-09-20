package tryt

import cats._
import cats.implicits._
import cats.data.OptionT
import scala.util.{Failure, Success, Try}

/** A minimal equivalent of Cat's OptionT monad transformer, with Try instead of Option.
  * See here for OptionT explanations: https://typelevel.org/cats/datatypes/optiont.html
  */
case class TryT[F[_], A](value: F[Try[A]]) {

  def map[B](fn: A => B)(implicit F: Functor[F]): TryT[F, B] = TryT(
    F.map(value)(_ map fn)
  )

  def flatMap[B](fn: A => F[Try[B]])(implicit M: Monad[F]): TryT[F, B] = TryT(
    M.flatMap(value)({
      case Failure(err) => M.pure(Failure(err))
      case Success(v) => fn(v)
    })
  )

  /** Flatmap over the 'inside' monad (i.e. Try), leaving the outer one intact.
    * It is the equivalent of doing `value.map(try => try.flatMap(fn))`.
    */
  def subflatMap[B](fn: A => Try[B])(implicit F: Functor[F]): TryT[F, B] = TryT(
    F.map(value)(_ >>= fn)
  )

  def fold[C](fa: Throwable => C, fb: A => C)(implicit F: Functor[F]): F[C] =
    F.map(value)(_.fold(fa, fb))

  def foldF[C](fa: Throwable => F[C], fb: A => F[C])(implicit F: Monad[F]): F[C] =
    F.flatMap(value)(_.fold(fa, fb))

  def bimap[C, D](fa: Throwable => Throwable, fb: A => D)(implicit F: Functor[F]): TryT[F, D] =
    TryT(F.map(value)({
      case Failure(err) => Failure(fa(err))
      case Success(success) => Success(fb(success))
    }))

  /** Apply the transformation `f` to the context `F`.
   */
  def mapK[G[_]](f: F ~> G): TryT[G, A] = TryT[G, A](f(value))

  /* Run the side-effecting function on the result, then return said results
   */
  def tap(fn: A => Unit)(implicit F: Functor[F]): TryT[F, A] = TryT(
    F.map(value)(_ map (inside => {
      fn(inside)
      inside
    }))
  )

  def isSuccess(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isSuccess)

  def isFailure(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isFailure)

  def toOption(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.toOption))

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    F.map(value)(_ getOrElse default)

  def getOrElseF[AA >: A](default: => F[AA])(implicit F: Monad[F]): F[AA] =
    F.flatMap(value)({
      case Failure(_)  => default
      case Success(b) => F.pure(b)
    })

  def valueOr(f: Throwable => A)(implicit F: Functor[F]): F[A] =
    fold(f, identity)

  def valueOrF(f: Throwable => F[A])(implicit F: Monad[F]): F[A] =
    F.flatMap(value)({
      case Failure(a)  => f(a)
      case Success(b) => F.pure(b)
    })

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_ forall f)

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_ exists f)
}

object TryT {

  def pure[F[_], A](value: A)(implicit M: Monad[F]): TryT[F, A] = TryT(M.pure(Success(value)))
}
