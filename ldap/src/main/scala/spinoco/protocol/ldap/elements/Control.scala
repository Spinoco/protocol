package spinoco.protocol.ldap.elements

import scodec.Codec
import scodec.bits.ByteVector
import spinoco.protocol.asn.ber
import spinoco.protocol.common.codec.{default, maybe}
import spinoco.protocol.ldap

/**
  * An extension argument for a protocolOp. Mechanism in which way client and
  * server can extend any protocolOp with additional values they understand.
  *
  * @param controlType    An unique identification of this control.
  * @param criticality    Whether the request should fail if the server does not understand
  *                       the control type.
  * @param controlValue   The value of the control, that should be decoded according the the
  *                       correct type specified by `controlType`
  */
case class Control(
  controlType: LDAPOID
  , criticality: Boolean
  , controlValue: Option[ByteVector]
)

object Control {

  val codec: Codec[Control] =
    ber.sequence((
      LDAPOID.codec ::
      default(ldap.boolean, false) ::
      maybe(ber.octetStringPrimitive)
    ).as[Control])

}
