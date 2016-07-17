package spinoco.protocol.common

import scodec.{Attempt, Err}

/**
  * Created by pach on 16/07/16.
  */
object util {

  def attempt[A](a: => A):Attempt[A] = {
    try { Attempt.successful(a) }
    catch { case t: Throwable => Attempt.failure(Err(s"${t.getClass} : ${t.getMessage}"))}
  }

}
