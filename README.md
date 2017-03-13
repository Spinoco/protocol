# protocol

This library contains various communication protocol decoders and encoders for scala. 

All the codecs are implemented with [scodec](http://scodec.org/). 

Following list of protocols is implemented:

 - STUN protocol according to [RFC5389](https://tools.ietf.org/html/rfc5389) and [RFC5245](https://tools.ietf.org/html/rfc5245)
 - WebSocket protocol according to [RFC6455](https://tools.ietf.org/html/rfc6455)
 - HTTP 1.1 protocol headers (Request/Response)
 - RTP and RTCP [RFC3550](https://www.ietf.org/rfc/rfc3550.txt)
 - SDP [RFC4566](https://tools.ietf.org/html/rfc4566)
 - MGCP [RFC3435](https://tools.ietf.org/html/rfc3435)
