package spinoco.protocol.mgcp.codec

import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs._
import spinoco.protocol.mgcp._
import spinoco.protocol.common.codec._
import spinoco.protocol.sdp.codec.SessionDescriptionCodec


object MGCPCommandCodec {


  lazy val codec: Codec[MGCPCommand] = "MGCP Command" | {
    import impl._
      (header("Header " | (
        ("Command Line" | (untilEOL(
          ("Command Type" | untilWs(mgcpCommandType)) :: WS ::
          ("Tx Id" | untilWs(transactionId)) :: WS ::
          ("Local Endpoint Name" | takeWhileChar(localEndpointNameCodec)('@')) :: constantString("@") ::
          ("Domain" | untilWs(ascii)) :: WS ::
          ("Version" | versionCodec)
          ))
        ) :+
        ("MGCP Paramater" | list(MGCPParameterCodec.codec))
        )) :+
        ("SDP" | (optional(recover2(choice(`\r\n`, `\n`)), SessionDescriptionCodec.codec) ))
    ).as[MGCPCommand]

  }


  object impl {




    val mgcpCommandType: Codec[MGCPCommandType.Value] =
      mappedEnum(ascii, MGCPCommandType.values.map { v => v -> v.toString }.toMap)





    val versionCodec: Codec[MGCPVersion] = {
      (constant(BitVector.view("MGCP".getBytes)) :: WS ::
        ("Major" | takeWhileChar(intAsString)('.')) :: constant(BitVector.view(Array[Byte]('.'))) ::
        ("Minor" | untilWs(intAsString)) ::
        ("Profile" | optional(recover2(constant(BitVector.view(Array[Byte](' ')))), ascii))
      ).as[MGCPVersion]
    }




  }

}
