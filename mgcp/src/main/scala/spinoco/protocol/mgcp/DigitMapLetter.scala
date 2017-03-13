package spinoco.protocol.mgcp


/**
  * Created by pach on 10/03/17.
  */
object DigitMapLetter extends Enumeration {


  val `#` = Value("#")
  val `*` = Value("*")

  val A, B, C, D, T, X = Value
  val E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V, W, Y, Z = Value
}


object DTMFLetter extends Enumeration {
  val A, B, C, D = Value
}

object DigitLetter extends Enumeration {
  val `0` = Value("0")
  val `1` = Value("1")
  val `2` = Value("2")
  val `3` = Value("3")
  val `4` = Value("4")
  val `5` = Value("5")
  val `6` = Value("6")
  val `7` = Value("7")
  val `8` = Value("8")
  val `9` = Value("9")
}