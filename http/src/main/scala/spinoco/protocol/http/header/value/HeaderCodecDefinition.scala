package spinoco.protocol.http.header.value

import scodec.Codec
import shapeless.Typeable
import spinoco.protocol.http.header.{ContentHeaderField, HttpHeader}

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
      def headerName: String = nameFromClass(ev.runtimeClass)

      def headerCodec: Codec[HttpHeader] = codec.asInstanceOf[Codec[HttpHeader]].withContext(headerName)
    }

  def nameFromClass(clz:Class[_]):String = {
    clz.getSimpleName.replace("$minus","-")
  }

  trait ContentHeaderCodecDefinition[A <: HttpHeader] extends HeaderCodecDefinition[A]{

    def contentField: Codec[ContentHeaderField]

  }

  def contentField[A <: ContentHeaderField: Typeable](codec: Codec[A])(implicit ev: ClassTag[A]): ContentHeaderCodecDefinition[HttpHeader] =
    new ContentHeaderCodecDefinition[HttpHeader]  {
      def headerName: String = nameFromClass(ev.runtimeClass)

      def headerCodec: Codec[HttpHeader] = codec.asInstanceOf[Codec[HttpHeader]].withContext(headerName)

      def contentField: Codec[ContentHeaderField] = codec.upcast
    }

}