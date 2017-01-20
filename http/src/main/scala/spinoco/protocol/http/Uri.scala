package spinoco.protocol.http

import java.net.{URLDecoder, URLEncoder}

import scodec.Codec
import codec.helper._
import scodec.bits.ByteVector
import spinoco.protocol.common.util._
import spinoco.protocol.common.codec._

/**
  * Internet Uri, as defined in http://tools.ietf.org/html/rfc3986
  * All values are decoded (no % escaping)
  */
sealed case class Uri(
  scheme: HttpScheme.Value
  , host: HostPort
  , path: Uri.Path
  , query: Uri.Query
)


object Uri {


  val pathQueryCodec:Codec[(Uri.Path, Uri.Query)] = {
    parametrized(ByteVector('?'), Path.codec, Query.codec).xmap(
      { case (p,q) => p -> q.getOrElse(Query.empty) }
      , { case (p,q) => p -> (if (q.params.isEmpty) None else Some(q)) }
    )
  }

  val codec: Codec[Uri] = {

    val hostPathQueryCodec:Codec[(HostPort, Uri.Path, Uri.Query)] = {
      (bytesUntil(_ != '/').codedAs(HostPort.codec) ~ pathQueryCodec).xmap(
        { case (hp, (path, query)) => (hp, path, query) }
        , { case (hp, path, query)  =>  (hp, (path, query)) }
      )
    }

    tuple[HttpScheme.Value,(HostPort, Uri.Path, Uri.Query)](`://`, HttpScheme.codec, hostPathQueryCodec)
    .xmap(
      { case (scheme, (host, path, query)) => Uri(scheme, host, path, query) }
      , uri => (uri.scheme, (uri.host, uri.path, uri.query))
    )
  }


  sealed case class Path(initialSlash: Boolean, trailingSlash:Boolean, segments: Seq[String]) { self =>

    def / (s: String) =
      self.copy(trailingSlash = false, segments = self.segments :+ s)

    def / =
      self.copy(trailingSlash = true)


    def stringify:String = {
      val sb = new StringBuilder()
      if (self.initialSlash) sb.append("/")
      sb.append(self.segments.map(s => URLEncoder.encode(s, "UTF-8")).mkString("/"))
      if (self.trailingSlash) sb.append("/")
      sb.toString()
    }

    //override def toString: String = s"Uri.Path($stringify)"
  }

  object Path {

    /** constructs relative path without initial slash (`/`) **/
    def relative(s: String) : Path =
      Path(initialSlash = false, trailingSlash = false, segments = Seq(s))

    /** constructs absolute path with initial slash (`/`) **/
    def absolute(s: String) : Path =
      Path(initialSlash = true, trailingSlash = false, segments = Seq(s))

    def  / (s: String) : Path = absolute(s)

    def fromUtf8String(path: String):Uri.Path = {
      val trimmed = path.trim
      val segments = trimmed.split("/").filter(_.nonEmpty).map(s => URLDecoder.decode(s, "UTF-8"))
      Path(
        initialSlash =  trimmed.startsWith("/")
        , segments = segments
        , trailingSlash = trimmed.endsWith("/") && segments.nonEmpty
      )
    }


    val codec : Codec[Uri.Path] = {
      trimmedUtf8String.xmap(fromUtf8String, _.stringify)
    }

    val Root: Path = Path(initialSlash = true, segments = Nil, trailingSlash = false)

  }


  sealed case class Query(params: List[(String, String)]) { self =>

    def :+ (pair: (String, String)) : Query = self.copy(self.params :+ pair)


  }

  object Query {

    val empty = Query(List.empty)

    val codec:Codec[Query] = {
      val param: Codec[(String, String)] = {
        tuple(_equal, utf8String, utf8String).exmap(
          { case (k,v) => attempt { URLDecoder.decode(v.trim, "UTF-8") }.map { k.trim -> _ } }
          , { case (k,v) => attempt { URLEncoder.encode(v, "UTF-8") }.map { k.trim -> _ } }
        )
      }

      delimitedBy(amp,amp, param).xmap(Query(_), _.params)
    }



  }



}