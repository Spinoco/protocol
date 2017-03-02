package spinoco.protcol.rtp

import scodec.bits.ByteVector

/**
  * RTCP Packet in full
  *
  * @param version          RTCP Protocol version
  *
  *
  * @param body             Body and Type of the packet
  */
case class RTCPPacket(
  version: RTPVersion.Value
  , body: RTCPPacketBody
)

sealed trait RTCPPacketBody

object  RTCPPacketBody {

  /**
    * RTCP Sender Report Packet as defined in RFC 3550 (https://www.ietf.org/rfc/rfc3550.txt)
    *
    * @param ssrc             The synchronization source identifier for the originator of this SR packet.
    *
    * @param ntpTimestampMSW  Indicates the wallclock time (see Section 4) when this report was
    *                         sent so that it may be used in combination with timestamps
    *                         returned in reception reports from other receivers to measure
    *                         round-trip propagation to those receivers.
    * @param rtpTimestamp     Corresponds to the same time as the NTP timestamp (above), but in
    *                         the same units and with the same random offset as the RTP
    *                         timestamps in data packets.
    * @param packetCount      The total number of RTP data packets transmitted by the sender
    *                         since starting transmission up until the time this SR packet was
    *                         generated.  The count SHOULD be reset if the sender changes its
    *                         SSRC identifier.
    * @param octetCount       The total number of payload octets (i.e., not including header or
    *                         padding) transmitted in RTP data packets by the sender since
    *                         starting transmission up until the time this SR packet was
    *                         generated.  The count SHOULD be reset if the sender changes its
    *                         SSRC identifier.  This field can be used to estimate the average
    *                         payload data rate.
    * @param report           Report for each SSRC.
    *
    *
    *
    */
  case class RTCPSenderReport(
   ssrc: Int
   , ntpTimestampMSW: Long
   , ntpTimestampLSW: Long
   , rtpTimestamp: Long
   , packetCount: Long
   , octetCount: Long
   , report: Vector[Report]
   , extensions: ByteVector
  ) extends RTCPPacketBody

  /**
    * RTCP Receiver Report Packet as defined in RFC 3550 (https://www.ietf.org/rfc/rfc3550.txt)
    *
    * @param ssrc             The synchronization source identifier for the originator of this SR packet.
    *
    * @param report Report for each SSRC.
    *
    */
  case class RTCPReceiverReport(
   ssrc: Int
   , report: Vector[Report]
   , extensions: ByteVector
  ) extends RTCPPacketBody


  /**
    * A packet indicating descripton and attributes of various source streams
    * @param descriptors  List of descriptors
    */
  case class SourceDescription(
    descriptors: Vector[SourceDescriptor]
  ) extends RTCPPacketBody

  /**
    * A packet indicating the RTP session has been terminated
    * @param ids      Identifiers terminated
    * @param reason   Reason for termination, if available
    */
  case class Bye(
    ids: Vector[Int]
    , reason: Option[String]
  ) extends RTCPPacketBody


  /**
    * A packet indicating custome application data
    *
    * @param ssrc   SSrc to which the app data are related
    * @param name   Name (4 ascii chars max)
    * @param data   Custom application data
    */
  case class ApplicationData(
    ssrc: Int
    , name: String
    , data: ByteVector
  ) extends RTCPPacketBody

}
/**
  * Sender or Receive report bloc for each SSRC
  *
  * @param ssrc               The SSRC identifier of the source to which the information in this
  *                           reception report block pertains.
  *
  * @param fractionLost       The fraction of RTP data packets from source SSRC_n lost since the
  *                           previous SR or RR packet was sent, expressed as a fixed point
  *                           number with the binary point at the left edge of the field.  (That
  *                           is equivalent to taking the integer part after multiplying the
  *                           loss fraction by 256.)
  *
  * @param packetsLost        The total number of RTP data packets from source SSRC_n that have
  *                           been lost since the beginning of reception.
  *
  * @param hiSeqNoReceived    The low 16 bits contain the highest sequence number received in an
  *                           RTP data packet from source SSRC_n, and the most significant 16
  *                           bits extend that sequence number with the corresponding count of
  *                           sequence number cycles, which may be maintained according to the
  *                           algorithm in Appendix A.1.  Note that different receivers within
  *                           the same session will generate different extensions to the
  *                           sequence number if their start times differ significantly
  *
  * @param jitter             An estimate of the statistical variance of the RTP data packet
  *                           interarrival time, measured in timestamp units and expressed as an
  *                           unsigned integer.
  *
  * @param lastSRTimestamp    The middle 32 bits out of 64 in the NTP timestamp (as explained in
  *                           Section 4) received as part of the most recent RTCP sender report
  *                           (SR) packet from source SSRC_n.  If no SR has been received yet,
  *                           the field is set to zero.
  *
  * @param delaySinceLastSR   The delay, expressed in units of 1/65536 seconds, between
  *                           receiving the last SR packet from source SSRC_n and sending this
  *                           reception report block.  If no SR packet has been received yet
  *                           from SSRC_n, the DLSR field is set to zero.
  */
case class Report(
  ssrc: Int
  , fractionLost : Int
  , packetsLost: Int
  , hiSeqNoReceived: Long
  , jitter: Int
  , lastSRTimestamp: Long
  , delaySinceLastSR: Long
)


case class SourceDescriptor(
  ssrc: Int
  , attributes: Vector[(SourceDescriptorType.Value, String)]
)


/**
  * Class describing RTCP Header in each RTCP Packet. Header consist of first two words (64 bits)
  *
  * @param version          RTCP Protocol version
  *
  * @param padding          If the padding bit is set, this individual RTCP packet contains
  *                         some additional padding octets at the end which are not part of
  *                         the control information but are included in the length field.  The
  *                         last octet of the padding is a count of how many padding octets
  *                         should be ignored, including itself (it will be a multiple of
  *                         four).
  **
  * @param elements         Elements in the rctp packet. These may be reports, source descriptors etc.
  *
  * @param length           Length of the packet in words (32 bits) - 1
  */
case class RTCPHeader(
  version: RTPVersion.Value
  , padding: Boolean
  , elements: Int
  , length: Int
  , tpe: RTCPPacketType.Value
)

// type of RTCP Packet
object RTCPPacketType extends Enumeration {
  val SenderReport = Value(200)
  val ReceiverReport = Value(201)
  val SourceDescription = Value(202)
  val Bye = Value(203)
  val AppData = Value(204)
}


object SourceDescriptorType extends Enumeration {
  val CNAME = Value(1)
  val NAME = Value(2)
  val EMAIL = Value(3)
  val PHONE = Value(4)
  val LOC = Value(5)
  val TOOL = Value(6)
  val NOTE = Value(7)
  val PRIV = Value(8)
}