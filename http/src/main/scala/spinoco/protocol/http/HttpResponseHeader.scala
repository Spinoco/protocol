package spinoco.protocol.http

import shapeless.Typeable
import spinoco.protocol.http.header.HttpHeader

/**
  * Header of the Http Response
  *
  * @param status       Status code
  * @param reason       Reason Phrase
  * @param headers      Headers of the response
  * @param version      Version
  */
sealed case class HttpResponseHeader(
  status: HttpStatusCode
  , reason: String
  , headers: List[HttpHeader] = Nil
  , version: HttpVersion.Value = HttpVersion.V1_1
) { self =>

  /** gets first http header satisfying type `H` constrain **/
  def firstHeader[H <: HttpHeader](implicit T: Typeable[H]): Option[H] =
    self.headers.collectFirst(Function.unlift(T.cast))

  /** select all headers of type `H` **/
  def selectHeaders[H <: HttpHeader](implicit T: Typeable[H]): Seq[H] =
    self.headers.collect(Function.unlift(T.cast))


}


