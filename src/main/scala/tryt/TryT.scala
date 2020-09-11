package tryt

import cats._
import cats.implicits._
import cats.data.OptionT
import scala.util.{Failure, Success, Try}

/** A minimal equivalent of Cat's OptionT monad transformer, with Try instead of Option.
  * See here for OptionT explanations: https://typelevel.org/cats/datatypes/optiont.html
  */
final case class TryT[F[_], A](value: F[Try[A]])(implicit private val M: Monad[F]) {

  def map[B](fn: A => B): TryT[F, B] = TryT(
    M.map(value)(_ map fn)
  )

  def transform[B](fn: Try[A] => Try[B]): TryT[F, B] = TryT(
    M.map(value)(fn)
  )

  def flatTransform[B](fn: Try[A] => F[Try[B]]): TryT[F, B] = TryT(
    M.flatMap(value)(fn)
  )

  def flatTransformT[B](fn: Try[A] => TryT[F, B]): TryT[F, B] = TryT(
    M.flatMap(value)(fn(_).value)
  )

  def flatMap[B](fn: A => F[Try[B]]): TryT[F, B] =
    flatTransform({
      case Failure(err) => M.pure(Failure(err))
      case Success(v)   => fn(v)
    })

  def flatMapT[B](fn: A => TryT[F, B]): TryT[F, B] =
    flatTransformT({
      case Failure(err) => TryT(M.pure(Failure(err)))
      case Success(v)   => fn(v)
    })

  /** Flatmap over the 'inside' monad (i.e. Try), leaving the outer one intact.
    * It is the equivalent of doing `value.map(try => try.flatMap(fn))`.
    */
  def subflatMap[B](fn: A => Try[B]): TryT[F, B] =
    transform(_ >>= fn)

  def fold[C](fa: Throwable => C, fb: A => C): F[C] =
    M.map(value)(_.fold(fa, fb))

  def foldF[C](fa: Throwable => F[C], fb: A => F[C]): F[C] =
    M.flatMap(value)(_.fold(fa, fb))

  def bimap[C, D](fa: Throwable => Throwable, fb: A => D): TryT[F, D] =
    transform({
      case Failure(err)     => Failure(fa(err))
      case Success(success) => Success(fb(success))
    })

  /** Apply the transformation `f` to the context `F`.
   */
  def mapK[G[_]](f: F ~> G)(implicit M: Monad[G]): TryT[G, A] =
    TryT[G, A](f(value))

  /* Run the side-effecting function on the inside value, then return the TryT untouched
   */
  def tap(fn: A => Unit): TryT[F, A] =
    map(inside => {
      fn(inside)
      inside
    })

  def ===(that: TryT[F, A])(implicit Eq: Eq[F[Try[A]]]): Boolean =
    Eq.eqv(value, that.value)

  def filter(fn: A => Boolean): TryT[F, A] =
    transform(_ filter fn)

  def collect[B](fn: PartialFunction[A, B]): TryT[F, B] =
    transform(_ collect fn)

  def isSuccess: F[Boolean] =
    M.map(value)(_.isSuccess)

  def isFailure: F[Boolean] =
    M.map(value)(_.isFailure)

  def toOption: OptionT[F, A] =
    OptionT(M.map(value)(_.toOption))

  def getOrElse[AA >: A](default: => AA): F[AA] =
    M.map(value)(_ getOrElse default)

  def getOrElseF[AA >: A](default: => F[AA]): F[AA] =
    M.flatMap(value)({
      case Failure(_) => default
      case Success(b) => M.pure(b)
    })

  def valueOr(f: Throwable => A): F[A] =
    fold(f, identity)

  def valueOrF(f: Throwable => F[A]): F[A] =
    M.flatMap(value)({
      case Failure(a) => f(a)
      case Success(b) => M.pure(b)
    })

  def forall(f: A => Boolean): F[Boolean] =
    M.map(value)(_ forall f)

  def exists(f: A => Boolean): F[Boolean] =
    M.map(value)(_ exists f)
}

object TryT {

  def pure[F[_], A](value: A)(implicit M: Monad[F]): TryT[F, A] =
    TryT(M.pure(Success(value)))
}
