package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.ldap
import spinoco.protocol.ldap.elements.LDAPDN.RelativeDistinguishedName

/**
  * Same as [[LDAPDN]] but has exactly one [[RelativeDistinguishedName]].
  *
  * @param nameComponent An unique identification of an entry.
  */
case class RelativeLDAPDN(
  nameComponent: RelativeDistinguishedName
)

object RelativeLDAPDN {

  val codec: Codec[RelativeLDAPDN] = ldap.ldapString.exmap(
    string => LDAPDN.decode(string).flatMap(dn =>
      Attempt.fromOption(dn.names.headOption, Err("There has to be at least one relative RelativeDistinguishedName for RelativeLDAPDN"))
      .map(RelativeLDAPDN(_))
    )
    , relative => LDAPDN.encodeRelative(relative.nameComponent)
  )

}
