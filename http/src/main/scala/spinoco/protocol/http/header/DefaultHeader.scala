package spinoco.protocol.http.header

import spinoco.protocol.http.header.value.HeaderCodecDefinition


trait DefaultHeader extends HttpHeader { self =>
  lazy val name: String = {
    HeaderCodecDefinition.nameFromClass(self.getClass)
  }
}
