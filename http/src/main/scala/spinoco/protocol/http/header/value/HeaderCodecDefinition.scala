package spinoco.protocol.http.header.value

import scodec.Codec
import spinoco.protocol.http.header.HttpHeader

import scala.reflect.ClassTag

/**
  * Created by pach on 12/01/17.
  */
trait HeaderCodecDefinition[A <: HttpHeader] {

  def headerName: String

  def headerCodec: Codec[A]

}


object HeaderCodecDefinition {

  def apply[A <: HttpHeader](codec: Codec[A])(implicit ev: ClassTag[A]):HeaderCodecDefinition[HttpHeader] =
    new HeaderCodecDefinition[HttpHeader] {
      def headerName: String =  nameFromClass(ev.runtimeClass)

      def headerCodec: Codec[HttpHeader] = codec.asInstanceOf[Codec[HttpHeader]]
    }

  def nameFromClass(clz:Class[_]):String = {
    clz.getSimpleName.replace("$minus","-")
  }

}