package spinoco.protocol.http

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
)


