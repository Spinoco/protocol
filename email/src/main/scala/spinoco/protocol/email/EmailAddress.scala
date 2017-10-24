package spinoco.protocol.email

/**
  * Email address as per RFC5322 sec. 3.4.1
  * @param localPart    Local address at given domain
  * @param domain       Domain of the email.
  */
case class EmailAddress(localPart: String, domain: String, display: Option[String]) {
  /** returns full address of this email **/
  lazy val address: String = localPart + "@" + domain
}
