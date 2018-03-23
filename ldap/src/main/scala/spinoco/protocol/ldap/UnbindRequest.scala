package spinoco.protocol.ldap

import scodec.Codec

/** Request to unbind from the server, connection termination should be started. **/
case object UnbindRequest extends ProtocolOp {

  // Codec without the BER wrapping
  val codecInner: Codec[UnbindRequest.type] =
    scodec.codecs.provide(()).xmap(_ => UnbindRequest, _ => ())

}

