package spinoco.protocol.kafka.codec

import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import scodec.codecs._
import shapeless.{::, HNil}
import spinoco.protocol.kafka._
import spinoco.protocol.common.util._
import spinoco.protocol.kafka.Request.{FetchRequest, MetadataRequest, OffsetsRequest, ProduceRequest}


object MessageCodec {

  /** enodes supplied request **/
  val requestCodec: Codec[RequestMessage] =
    variableSizeBytes(int32, impl.requestContentCodec)

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
      case ApiKey.OffsetRequest => OffsetCodec.responseCodec(version).upcast
    }
  }


  object impl {

    val apiKeyCodec:Codec[ApiKey.Value] =
      int16.exmap(
        code => attempt(ApiKey(code))
        , k => Attempt.successful(k.id)
      )



    type RequestHeader = ApiKey.Value :: Int :: Int :: String :: HNil
    val requestHeaderCodec : Codec[RequestHeader] = {
      "Request Header" | (
        ("Api Key"        | apiKeyCodec) ::
        ("Api Version"    | int16) ::
        ("Correlation Id" | int32) ::
        ("Client Id"      | kafkaRequiredString)
      )
    }

    val requestContentCodec: Codec[RequestMessage] = {
      def encode(rm:RequestMessage):(RequestHeader, Request) = {
        val key = ApiKey.forRequest(rm.request)
        val version: Int = rm.request match {
          case _: ProduceRequest => rm.version match {
            case ProtocolVersion.Kafka_0_8 => 0
            case ProtocolVersion.Kafka_0_9 => 1
            case ProtocolVersion.Kafka_0_10 |
                 ProtocolVersion.Kafka_0_10_1 |
                 ProtocolVersion.Kafka_0_10_2 => 2
          }

          case _: FetchRequest => rm.version match {
            case ProtocolVersion.Kafka_0_8 => 0
            case ProtocolVersion.Kafka_0_9 => 1
            case ProtocolVersion.Kafka_0_10 |
                 ProtocolVersion.Kafka_0_10_1 => 2
            case ProtocolVersion.Kafka_0_10_2 => 3
          }

          case _: MetadataRequest => 0

          case _: OffsetsRequest => 0
        }
        (key :: version :: rm.correlationId :: rm.clientId :: HNil, rm.request)
      }
      def decode(header:RequestHeader, request:Request): RequestMessage = {
        val version :: correlation :: clientId :: HNil = header.tail
        val protocolVersion = request match {
          case _: ProduceRequest => version match {
            case 0 => ProtocolVersion.Kafka_0_8
            case 1 => ProtocolVersion.Kafka_0_9
            case _ => ProtocolVersion.Kafka_0_10
          }
          case _: FetchRequest =>  version match {
            case 0 => ProtocolVersion.Kafka_0_8
            case 1 => ProtocolVersion.Kafka_0_9
            case 2 => ProtocolVersion.Kafka_0_10
            case 3 | _ => ProtocolVersion.Kafka_0_10_2
          }
          case _: MetadataRequest => ProtocolVersion.Kafka_0_8
          case _: OffsetsRequest => ProtocolVersion.Kafka_0_8
        }
        RequestMessage(protocolVersion, correlation, clientId, request)
      }

      requestHeaderCodec.flatZip[Request] {
        case api :: version :: _ =>
          api match {
            case ApiKey.ProduceRequest => ProduceCodec.requestCodec.upcast
            case ApiKey.FetchRequest => FetchCodec.requestCodec(version).upcast
            case ApiKey.MetadataRequest => MetadataCodec.requestCodec.upcast
            case ApiKey.OffsetRequest => OffsetCodec.requestCodec(version).upcast
          }
      }.xmap(decode _ tupled,encode)
    }

  }

}
