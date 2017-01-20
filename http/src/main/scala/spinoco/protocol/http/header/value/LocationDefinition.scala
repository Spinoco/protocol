package spinoco.protocol.http.header.value

import scodec.Codec
import scodec.codecs._
import spinoco.protocol.http.Uri
import spinoco.protocol.http.Uri.{Path, Query}

/**
  * Created by pach on 18/01/17.
  */
trait LocationDefinition

object LocationDefinition {

  sealed case class Absolute(uri:Uri) extends LocationDefinition

  sealed case class Relative(path: Path, query: Query) extends LocationDefinition

  val codec: Codec[LocationDefinition] = {
    choice(
      Uri.codec.xmap[Absolute](Absolute.apply , _.uri).upcast
      , Uri.pathQueryCodec.xmap[Relative](Relative.apply _ tupled, rel => rel.path -> rel.query).upcast
    )
  }

}
