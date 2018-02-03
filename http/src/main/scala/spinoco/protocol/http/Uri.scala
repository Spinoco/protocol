package spinoco.protocol.http

import java.net.{URLDecoder, URLEncoder}

import scodec.{Attempt, Codec, Err, codecs}
import codec.helper._
import scodec.bits.BitVector
import spinoco.protocol.common.util._
import spinoco.protocol.common.codec._
import spinoco.protocol.common.Terminator
import spinoco.protocol.http.Uri.QueryParameter.Multi

import scala.annotation.tailrec

/**
  * Internet Uri, as defined in http://tools.ietf.org/html/rfc3986
  * All values are decoded (no % escaping)
  */
sealed case class Uri(
  scheme: Scheme
  , host: HostPort
  , path: Uri.Path
  , query: Uri.Query
) { self =>
  /** replaces query with one specified **/
  def withQuery(query: Uri.Query): Uri =
    self.copy(query = query)

  /** appends supplied param to uri **/
  def withParam(k: String, v: String): Uri =
    self.copy(query = self.query :+ (k, v))

  /** if this is valid Uri, yields to string representation of this Uri **/
  lazy val stringify: Attempt[String] = {
    Uri.codec.encode(self) flatMap { bytes =>
      Attempt.fromEither(bytes.decodeUtf8.left.map(rsn => Err(s"Failed to decode UTF8: $rsn")))
    }
  }

  /** throws if this cannot be encoded to Uri **/
  def stringifyUnsafe: String =
    stringify.fold(err => throw new Throwable(s"Failed to stringify: ${err.message}"), identity)


}


object Uri {

  def http(host: String, path: String): Uri =
    Uri(HttpScheme.HTTP, HostPort(host, None), Uri.Path.fromUtf8String(path), Query.empty)

  def http(host: String, port: Int, path: String): Uri =
    Uri(HttpScheme.HTTP, HostPort(host, Some(port)), Uri.Path.fromUtf8String(path), Query.empty)

  def https(host: String, path: String): Uri =
    Uri(HttpScheme.HTTPS, HostPort(host, None), Uri.Path.fromUtf8String(path), Query.empty)

  def https(host: String, port: Int, path: String): Uri =
    Uri(HttpScheme.HTTPS, HostPort(host, Some(port)), Uri.Path.fromUtf8String(path), Query.empty)

  def ws(host: String, path: String): Uri =
    Uri(HttpScheme.WS, HostPort(host, None), Uri.Path.fromUtf8String(path), Query.empty)

  def ws(host: String, port: Int, path: String): Uri =
    Uri(HttpScheme.WS, HostPort(host, Some(port)), Uri.Path.fromUtf8String(path), Query.empty)

  def wss(host: String, path: String): Uri =
    Uri(HttpScheme.WSS, HostPort(host, None), Uri.Path.fromUtf8String(path), Query.empty)

  def wss(host: String, port: Int, path: String): Uri =
    Uri(HttpScheme.WSS, HostPort(host, Some(port)), Uri.Path.fromUtf8String(path), Query.empty)

  /** parse supplied string and receive Uri, if supplied string is valid **/
  def parse(uriString: String): Attempt[Uri] =
    codec.decodeValue(BitVector.view(uriString.getBytes))

  val pathQueryCodec:Codec[(Uri.Path, Uri.Query)] = {
    val queryCodec: Codec[Query] = {
      val d = (codecs.byte.unit('?') ~> Query.codec)
      Codec(q => if (q.params.isEmpty) attempt(BitVector.empty) else d.encode(q), d.decode _)
    }

    bytesUntil(_ != '?').codedAs(Path.codec) ~
    codecs.optional(codecs.bitsRemaining, queryCodec).xmap(_.getOrElse(Query.empty), Some(_))
  }

  val hostPortCodec: Codec[HostPort] = bytesUntil(_ != '/').codedAs(HostPort.codec)

  val codec: Codec[Uri] = {

    val hostPathQueryCodec:Codec[(HostPort, Uri.Path, Uri.Query)] = {
      (bytesUntil(b => b != '/' && b != '?').codedAs(HostPort.codec) ~ pathQueryCodec).xmap(
        { case (hp, (path, query)) => (hp, path, query) }
        , { case (hp, path, query)  =>  (hp, (path, query)) }
      )
    }

    (terminated(Scheme.codec, Terminator.constantString1("://")) ~ hostPathQueryCodec)
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
    val Empty: Path = Path(initialSlash = false, segments = Nil, trailingSlash = false)
  }


  sealed case class Query(params: List[QueryParameter]) { self =>

    def append(param: QueryParameter): Query = self.copy(params = self.params :+ param)
    def append(k: String, v: String): Query = append(QueryParameter.single(k, v))
    def append(flag: String): Query = append(QueryParameter.flag(flag))

    def :+(param: QueryParameter): Query = append(param)
    def :+(k: String, v: String): Query = append(QueryParameter.single(k, v))
    def :+(flag: String): Query = append(QueryParameter.flag(flag))

    def hasFlag(flag: String): Boolean =
      collectFirst { case QueryParameter.Flag(`flag`) => true }.getOrElse(false)

    def valueOf(k: String): Option[String] =
      collectFirst { case QueryParameter.Single(`k`, v) => v }

    def collectFirst[A](pf: PartialFunction[SingleOrFlagParameter, A]): Option[A] = {
      val lifted = pf.lift
      @tailrec
      def go(rem: List[QueryParameter]): Option[A] = {
        rem.headOption match {
          case Some(p) => p match {
            case QueryParameter.Multi(p1, p2, tail) => go(p1 +: p2 +: (tail ++ rem))
            case param: SingleOrFlagParameter => lifted(param) match {
              case None => go(rem.tail)
              case Some(a) => Some(a)
            }
          }

          case None => None
        }
      }
      go(self.params)
    }

    def collect[A](pf: PartialFunction[SingleOrFlagParameter, A]): List[A] = {
      val lifted = pf.lift
      @tailrec
      def go(rem: List[QueryParameter], acc: Vector[A]): List[A] = {
        rem.headOption match {
          case Some(p) => p match {
            case QueryParameter.Multi(p1, p2, tail) => go(p1 +: p2 +: (tail ++ rem), acc)
            case param: SingleOrFlagParameter =>  go(rem.tail, acc ++ lifted(param))
          }

          case None => acc.toList
        }
      }
      go(self.params, Vector.empty)
    }

  }

  object Query {
    import scodec.codecs._

    val empty = Query(List.empty)

    def apply(k:String, v:String): Query = empty :+ (k,v)
    def apply(flag: String): Query = empty :+ flag

    private def urlDecode(s: String) = attempt(URLDecoder.decode(s.trim, "UTF-8"))
    private def urlEncode(s: String) = attempt(URLEncoder.encode(s, "UTF-8"))

    val codec: Codec[Query] = {
      val `=` = BitVector.view("=".getBytes)
      val `;` = BitVector.view(";".getBytes)

      val urlEncodedString:Codec[String] =
        utf8.exmap(urlDecode, urlEncode)

      val param: Codec[QueryParameter] = {
        import QueryParameter._
        val flag: Codec[Flag] = {
          listDelimited(`=`, urlEncodedString).narrow(
            {
              case a :: Nil =>  Attempt.successful(Flag(a))
              case a :: b :: Nil if b.trim.isEmpty => Attempt.successful(Flag(a))
              case other => Attempt.failure(Err(s"Failed to decode flag, must be flag alone or followed by `=` : $other"))
            }
            , flag => List(flag.name)
          )
        }


        val single: Codec[Single] = {
          listDelimited(`=`, urlEncodedString).narrow(
            {
              case a :: b :: Nil if b.trim.nonEmpty => Attempt.successful(Single(a,b))
              case other => Attempt.failure(Err(s"Single paramter must be from key and value, both separated by `=` character: $other"))
            }
            , single => List(single.name, single.value)
          )
        }


        val multi: Codec[Multi] = {
          val multiParam: Codec[SingleOrFlagParameter] = {
            choice(
              single.upcast[SingleOrFlagParameter]
              , flag.upcast[SingleOrFlagParameter]
            )
          }

          listDelimited(`;`, multiParam).narrow(
            {
              case p1 :: p2 :: tail => Attempt.successful(Multi(p1, p2, tail))
              case other => Attempt.failure(Err(s"Uri Query multi-parameter must be seprated by `;` and must have at least two parameters: $other"))
            }
            , multi => List(multi.p1, multi.p2) ++ multi.tail
          )
        }

        choice(
          multi.upcast[QueryParameter]
          , single.upcast[QueryParameter]
          , flag.upcast[QueryParameter]
        )
      }

      spinoco.protocol.http.codec.helper.delimitedBy(amp,amp, param).xmap(Query(_), _.params)
    }



  }


  sealed trait QueryParameter {
    import QueryParameter._
    def append(param: SingleOrFlagParameter): Multi
    def append(k: String, v: String): Multi = append(Single(k, v))
    def append(flag: String): Multi = append(Flag(flag))

    def :+ (param: SingleOrFlagParameter): Multi = append(param)
    def :+ (k: String, v: String): Multi = append(Single(k, v))
    def :+ (flag: String): Multi = append(Flag(flag))
  }

  sealed trait SingleOrFlagParameter extends QueryParameter {
  }

  object QueryParameter {

    def single(k: String, v: String): Single = Single(k, v)
    def flag(flg: String): Flag = Flag(flg)
    def multi(p1: SingleOrFlagParameter, p2: SingleOrFlagParameter): Multi = Multi(p1, p2, Nil)

    final case class Single(name: String, value: String) extends SingleOrFlagParameter {
      def append(param: SingleOrFlagParameter): Multi = Multi(this, param, Nil)
    }
    final case class Flag(name: String) extends  SingleOrFlagParameter {
      def append(param: SingleOrFlagParameter): Multi = Multi(this, param, Nil)
    }
    final case class Multi(p1: SingleOrFlagParameter, p2: SingleOrFlagParameter, tail: List[SingleOrFlagParameter]) extends QueryParameter {
      def append(param: SingleOrFlagParameter): Multi = copy(tail = tail :+ param)

    }
  }



}
