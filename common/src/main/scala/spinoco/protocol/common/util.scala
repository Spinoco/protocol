package spinoco.protocol.common

import scodec.{Attempt, Err}
import shapeless.tag._

/**
  * Created by pach on 16/07/16.
  */
object util {

  def attempt[A](a: => A):Attempt[A] = {
    try { Attempt.successful(a) }
    catch { case t: Throwable => Attempt.failure(Err(s"${t.getClass} : ${t.getMessage}"))}
  }


  class TaggerF[F[_],A] {
    def apply[T](f:F[A]):F[A @@ T] = f.asInstanceOf[F[A @@ T]]
    def unwrap[T](f:F[A @@ T]) = f.asInstanceOf[F[A]]
  }

  /** safely tag any `A` in `F` w/o need to access it **/
  def tagF[F[_],A]:TaggerF[F,A] = new TaggerF[F,A]

}
