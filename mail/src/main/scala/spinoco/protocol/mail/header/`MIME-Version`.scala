package spinoco.protocol.mail.header

case class `MIME-Version`(version: String) extends DefaultEmailHeaderField


object `MIME-Version` extends DefaultHeaderDescription[`MIME-Version`] {
  val  codec = scodec.codecs.ascii.xmap[`MIME-Version`](`MIME-Version`.apply, _.version)
}
