package spinoco.protocol.http

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
)
