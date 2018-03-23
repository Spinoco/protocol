package spinoco.protocol.ldap.elements

import scodec.{Attempt, Codec, Err}
import spinoco.protocol.ldap
import spinoco.protocol.ldap.elements.LdapDN.RelativeDistinguishedName

/**
  * Same as [[LdapDN]] but has exactly one [[RelativeDistinguishedName]].
  *
  * @param nameComponent An unique identification of an entry.
  */
case class RelativeLdapDN(
  nameComponent: RelativeDistinguishedName
)

object RelativeLdapDN {

  val codec: Codec[RelativeLdapDN] = ldap.ldapString.exmap(
    string => LdapDN.decode(string).flatMap(dn =>
      Attempt.fromOption(dn.names.headOption, Err("There has to be at least one relative RelativeDistinguishedName for RelativeLDAPDN"))
      .map(RelativeLdapDN(_))
    )
    , relative => LdapDN.encodeRelative(relative.nameComponent)
  )

}
