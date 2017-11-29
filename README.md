# protocol

This library contains various communication protocol decoders and encoders for scala. 

All the codecs are implemented with [scodec](http://scodec.org/). 

Currently these protocols are implemented: 

 - STUN protocol according to [RFC5389](https://tools.ietf.org/html/rfc5389) and [RFC5245](https://tools.ietf.org/html/rfc5245)
 - WebSocket protocol according to [RFC6455](https://tools.ietf.org/html/rfc6455)
 - HTTP 1.1 protocol headers (Request/Response)
 - RTP and RTCP [RFC3550](https://www.ietf.org/rfc/rfc3550.txt)
 - SDP [RFC4566](https://tools.ietf.org/html/rfc4566)
 - MGCP [RFC3435](https://tools.ietf.org/html/rfc3435)
 - MIME [RFC2045](https://tools.ietf.org/html/rfc2045)
 - Email [RFC5322](https://tools.ietf.org/html/rfc5322), [RFC2047](https://tools.ietf.org/html/rfc2047)
 - IMAP v4 rev1 [RFC3501](https://tools.ietf.org/html/rfc3501) (BODY, BODYSTRUCTURE, ENVELOPE)