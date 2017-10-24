package spinoco.protocol.common

import scodec.{Attempt, Err}
import shapeless.tag._

/**
  * Created by pach on 16/07/16.
  */
object util {

  /** constructs from layz evaluation `a` catching any exceptions **/
  def attempt[A](a: => A):Attempt[A] = {
    try { Attempt.successful(a) }
    catch { case t: Throwable => Attempt.failure(Err(s"${t.getClass} : ${t.getMessage}"))}
  }

  /** constructs from Either where on left side is an exception **/
  def attemptFromEither[A](rslt: Either[Throwable, A]): Attempt[A] = {
    Attempt.fromEither(rslt.left.map { ex =>
      Err(s"Unexpected failure: ${ex.getMessage} [${ex.getClass.getName}]")
    })
  }


  class TaggerF[F[_],A] {
    def apply[T](f:F[A]):F[A @@ T] = f.asInstanceOf[F[A @@ T]]
    def unwrap[T](f:F[A @@ T]) = f.asInstanceOf[F[A]]
  }

  /** safely tag any `A` in `F` w/o need to access it **/
  def tagF[F[_],A]:TaggerF[F,A] = new TaggerF[F,A]

}
