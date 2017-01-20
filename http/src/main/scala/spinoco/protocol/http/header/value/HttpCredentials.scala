package spinoco.protocol.http.header.value

/**
  * Created by pach on 12/01/17.
  */
sealed trait  HttpCredentials


object HttpCredentials {

  sealed case class BasicHttpCredentials(username: String, password: String) extends HttpCredentials

  sealed case class OAuth2BearerToken(token: String) extends HttpCredentials

  sealed case class GenericHttpCredentials(
    scheme: String
    , token: String
    , params: Map[String, String]
  ) extends HttpCredentials

}
