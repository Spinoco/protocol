package spinoco.protocol.http.header.value

import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import spinoco.protocol.http.codec.helper._

sealed case class ContentType(
  mediaType: MediaType
  , charset: Option[HttpCharset]
  , boundary:Option[String]
)


object ContentType {

  val codec: Codec[ContentType] = {
     val boundaryOrCharset: Codec[Either[HttpCharset, String]] = {
       val chsetCodec:Codec[Either[HttpCharset, String]] =
         tuple(_equal,trimmedAsciiString,HttpCharset.codec).exmap(
           {
             case ("charset", chset) =>  Attempt.successful(Left(chset))
             case other => Attempt.failure(Err("charset or boundary expected"))
           }
           , {
             case Left(chset) => Attempt.successful("charset" -> chset)
             case _ => Attempt.failure(Err("Charset expected"))
           }
         )

       val boundaryCodec:Codec[Either[HttpCharset, String]] =
         tuple(_equal,trimmedAsciiString,trimmedAsciiString).exmap(
           {
             case ("boundary", boundary) =>  Attempt.successful(Right(boundary))
             case other => Attempt.failure(Err("charset or boundary expected"))
           }
           , {
             case Right(boundary) => Attempt.successful("boundary" -> boundary)
             case _ => Attempt.failure(Err("Boundary expected"))
           }
         )

         choice(chsetCodec,boundaryCodec)
     }

     parametrized2[MediaType, Either[HttpCharset, String]](semicolon, semicolon_SP, MediaType.codec, boundaryOrCharset).xmap(
       { case (mt, borc) => ContentType(mt, charset = borc.flatMap(_.left.toOption), boundary = borc.flatMap(_.right.toOption)) }
       , ct => ct.mediaType -> (ct.boundary.map(Right(_)) orElse ct.charset.map(Left(_)))
     )
  }

}
