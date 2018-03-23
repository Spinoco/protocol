package spinoco.protocol.asn.ber

/**
  * BER identifier octets representation.
  *
  * @param berClass     The class tag of the content. Specifies the scope of the number tag.
  * @param constructed  Whether the contents are constructed, ie they will have another
  *                     BER encoded values inside of them.
  * @param numberTag    The number tag of the content, the identifier for the content
  *                     within the scope of the classTag.
  */
case class Identifier(
  berClass: BerClass.Value
  , constructed: Boolean
  , numberTag: Int
)
