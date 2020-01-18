package spinoco

package object protocol {

  implicit class RightBiasedEither[A, B](val either: Either[A, B]) extends AnyVal {
    def map[B1](f: B => B1): Either[A, B1] = either.right.map(f)
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = either.right.flatMap(f)
    def toOption: Option[B] = either match {
      case Right(b) => Some(b)
      case _        => None
    }
  }
}
