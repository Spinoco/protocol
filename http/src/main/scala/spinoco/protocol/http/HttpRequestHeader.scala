package spinoco.protocol.http

import shapeless.Typeable
import spinoco.protocol.http.header.HttpHeader

/**
  * Http Request Header
  *
  * @param method   Request Method
  * @param version  Http Request Version
  * @param path     Request path (absolute)
  * @param query    Request Query
  * @param headers  Request Headers
  */
sealed case class HttpRequestHeader(
  method: HttpMethod.Value
  , path: Uri.Path = Uri.Path.Root
  , headers: List[HttpHeader] = Nil
  , query: Uri.Query = Uri.Query.empty
  , version: HttpVersion.Value = HttpVersion.V1_1
) { self =>

  /** gets first http header satisfying type `H` constrain **/
  def firstHeader[H <: HttpHeader](implicit T: Typeable[H]): Option[H] =
    self.headers.collectFirst(Function.unlift(T.cast))

  /** select all headers of type `H` **/
  def selectHeaders[H <: HttpHeader](implicit T: Typeable[H]): Seq[H] =
    self.headers.collect(Function.unlift(T.cast))

}
