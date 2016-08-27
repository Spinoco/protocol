package spinoco.protocol.kafka.codec

import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protocol.kafka._
import spinoco.protocol.common.util._


object MessageCodec {

  /** enodes supplied request **/
  val requestCodec:Codec[RequestMessage] =
    variableSizeBytes(XXXT("SIZE")(int32), impl.requestContentCodec)

  /**
    * Decodes response correlation id plus the bytes of the response
    * Once correlationId is known, then original request may be identified
    * and then `responseCodecFor` may be used to actually decode the message-
    */
  val responseCorrelationCodec:Codec[(Int,BitVector)] =
    variableSizeBytes(int32, ("Correlation Id" | int32) ~ bits )

  /** decodes concrete response **/
  def responseCodecFor(version: ProtocolVersion.Value, apiKey:ApiKey.Value):Codec[Response] = {
    apiKey match {
      case ApiKey.FetchRequest => FetchCodec.responseCodec(version).upcast
      case ApiKey.MetadataRequest => MetadataCodec.metadataResponseCodec.upcast
      case ApiKey.ProduceRequest => ProduceCodec.produceResponseCodec(version).upcast
    }
  }


  object impl {

    val apiKeyCodec:Codec[ApiKey.Value] =
      int16.exmap(
        code => attempt(ApiKey(code))
        , k => Attempt.successful(k.id)
      )

    val versionCodec:Codec[ProtocolVersion.Value] =
      int16.exmap(
        code => attempt(ProtocolVersion(code))
        , k => Attempt.successful(k.id)
      )

    type RequestHeader = ApiKey.Value :: ProtocolVersion.Value :: Int :: Option[String] :: HNil
    val requestHeaderCodec : Codec[RequestHeader] = {
      "Request Header" | (
        ("Api Key"        | apiKeyCodec) ::
        ("Api Version"    | versionCodec) ::
        ("Correlation Id" | int32) ::
        ("Client Id"      | kafkaOptionalString)
      )
    }

    val requestContentCodec:Codec[RequestMessage] = {
      def encode(rm:RequestMessage):(RequestHeader, Request) = {
        import Request._
        val key =
          rm.request match {
            case _: MetadataRequest => ApiKey.MetadataRequest
            case _: ProduceRequest => ApiKey.ProduceRequest
            case _: FetchRequest => ApiKey.FetchRequest
          }
        (key :: rm.version :: rm.correlationId :: rm.clientId :: HNil, rm.request)
      }
      def decode(header:RequestHeader, request:Request):RequestMessage = {
        val version :: correlation :: clientId :: HNil = header.tail
        RequestMessage(version,correlation,clientId,request)
      }

      requestHeaderCodec.flatZip[Request] {
        case api :: version :: _ =>
          import ApiKey._
          api match {
            case ProduceRequest => ProduceCodec.requestCodec.upcast
            case FetchRequest => FetchCodec.requestCodec.upcast
            case MetadataRequest => MetadataCodec.requestCodec.upcast
          }
      }.xmap(decode _ tupled,encode)
    }

  }

}
