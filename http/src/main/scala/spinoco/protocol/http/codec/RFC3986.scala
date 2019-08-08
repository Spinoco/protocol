package spinoco.protocol.http.codec

import java.nio.charset.StandardCharsets

import scala.collection.immutable.BitSet

/**
  * https://tools.ietf.org/html/rfc3986
  */
object RFC3986 {

  val genDelims = BitSet(':', '/', '?', '#', '[', ']', '@')

  val subDelims = BitSet('!', '$', '&', '\'', '(', ')' , '*', '+', ',', ';', '=')

  val reserved  = genDelims ++ subDelims

  val alpha = BitSet((('a' to 'z') ++ ('A' to 'Z')).map(_.toInt): _*)

  val digit = BitSet(('0' to '9').map(_.toInt): _*)

  val unreserved = alpha ++ digit ++ BitSet('-', '.', '_', '~')

  val pchar = unreserved ++ subDelims ++ BitSet(':', '@')


  // https://tools.ietf.org/html/rfc3986#section-3.1
  val scheme = alpha ++ digit ++ BitSet('+', '-', '.')

  // https://tools.ietf.org/html/rfc3986#section-3.3
  val pathSegment = pchar


  def encode(str: String, allowedChars: BitSet): String = {
    // add a buffer to hopefully account for all chars that need to be escaped
    val sb = new StringBuilder(str.length * 12 / 10)
    str.foreach { c =>
      if (allowedChars.contains(c)) sb.append(c)
      else {
        // https://tools.ietf.org/html/rfc3986#section-2.5
        c.toString.getBytes(StandardCharsets.UTF_8).foreach { b =>
          sb.append("%" + "%02X".format(b))
        }
      }
    }
    sb.mkString
  }

  def encodePathSegment(segment: String): String = encode(segment, pathSegment)

}
