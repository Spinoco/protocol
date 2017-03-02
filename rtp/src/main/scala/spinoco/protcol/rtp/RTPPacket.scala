package spinoco.protcol.rtp

import scodec.bits.ByteVector


/**
  * Models simple RTP Packet according to RFC 3550 (https://www.ietf.org/rfc/rfc3550.txt)
  *
  *
  *
  * @param version              This field identifies the version of RTP.  The version defined by
  *                             this specification is two (2).
  *
  * @param marker               The interpretation of the marker is defined by a profile.  It is
  *                             intended to allow significant events such as frame boundaries to
  *                             be marked in the packet stream.  A profile MAY define additional
  *                             marker bits or specify that there is no marker bit by changing the
  *                             number of bits in the payload type field (see Section 5.3).
  *
  * @param payloadType          This field identifies the format of the RTP payload and determines
  *                             its interpretation by the application.  A profile MAY specify a
  *                             default static mapping of payload type codes to payload formats.
  *                             Additional payload type codes MAY be defined dynamically through
  *                             non-RTP means (see Section 3).  A set of default mappings for
  *                             audio and video is specified in the companion RFC 3551 [1].  An
  *                             RTP source MAY change the payload type during a session, but this
  *                             field SHOULD NOT be used for multiplexing separate media streams
  *                             (see Section 5.2).
  *                             A receiver MUST ignore packets with payload types that it does not
  *                             understand.
  *
  * @param sequenceNumber       The sequence number increments by one for each RTP data packet
  *                             sent, and may be used by the receiver to detect packet loss and to
  *                             restore packet sequence.  The initial value of the sequence number
  *                             SHOULD be random (unpredictable) to make known-plaintext attacks
  *                             on encryption more difficult, even if the source itself does not
  *                             encrypt according to the method in Section 9.1, because the
  *                             packets may flow through a translator that does.  Techniques for
  *                             choosing unpredictable numbers are discussed in [17].
  *
  * @param timestamp            The timestamp reflects the sampling instant of the first octet in
  *                             the RTP data packet.  The sampling instant MUST be derived from a
  *                             clock that increments monotonically and linearly in time to allow
  *                             synchronization and jitter calculations (see Section 6.4.1).
  *
  * @param ssrc                The SSRC field identifies the synchronization source.  This
  *                            identifier SHOULD be chosen randomly, with the intent that no two
  *                            synchronization sources within the same RTP session will have the
  *                            same SSRC identifier.  An example algorithm for generating a
  *                            random identifier is presented in Appendix A.6.  Although the
  *                            probability of multiple sources choosing the same identifier is
  *                            low, all RTP implementations must be prepared to detect and
  *                            resolve collisions.  Section 8 describes the probability of
  *                            collision along with a mechanism for resolving collisions and
  *                            detecting RTP-level forwarding loops based on the uniqueness of
  *                            the SSRC identifier.  If a source changes its source transport
  *                            address, it must also choose a new SSRC identifier to avoid being
  *                            interpreted as a looped source (see Section 8.2).
  *
  * @param csrc                The CSRC list identifies the contributing sources for the payload
  *                            contained in this packet.  The number of identifiers is given by
  *                            the CC field.  If there are more than 15 contributing sources,
  *                            only 15 can be identified.  CSRC identifiers are inserted by
  *                            mixers (see Section 7.1), using the SSRC identifiers of
  *                            contributing sources.  For example, for audio packets the SSRC
  *                            identifiers of all sources that were mixed together to create a
  *                            packet are listed, allowing correct talker indication at the
  *                            receiver.
  *
  * @param payload             Payload of this RTP Packet.
  *
  * @param extensionHeader     Extension header, if present
  */
case class RTPPacket(
  version: RTPVersion.Value
  , marker: Boolean
  , payloadType: Int
  , sequenceNumber: Int
  , timestamp: Long
  , ssrc: Int
  , csrc: Seq[Int]
  , payload: ByteVector
  , extensionHeader: Option[RTPHeaderExtension]
)



case class RTPHeaderExtension(
  flag: Int
  , content: ByteVector
)


object RTPVersion extends Enumeration {
  val V_2 = Value(2)
}