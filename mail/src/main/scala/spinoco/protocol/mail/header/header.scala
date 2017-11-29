package spinoco.protocol.mail
import scodec.Codec
import shapeless.Typeable

import scala.reflect.ClassTag

/**
  * Created by pach on 13/10/17.
  */
package object header {


  trait EmailHeaderField {
    /** returns name of the email header **/
    def name: String
  }

  /** denotes header fields that may be used with MIME parts **/
  trait ContentHeaderField extends EmailHeaderField

  trait DefaultEmailHeaderField extends EmailHeaderField {
    lazy val name: String = this.getClass.getSimpleName.replace("$minus","-")
  }


  /** description of header, including its codec **/
  trait HeaderDescription[H <: EmailHeaderField] {


    /** name of the header **/
    def name: String

    /** header codec **/
    def codec: Codec[H]

    def emailHeaderField: Codec[EmailHeaderField]

  }

  trait ContentHeaderDescription[H <: ContentHeaderField] extends HeaderDescription[H] {
    def contentHeaderField: Codec[ContentHeaderField]
  }

  abstract class DefaultHeaderDescription[H <: EmailHeaderField : ClassTag : Typeable]
    extends HeaderDescription [H] {

    /** name of the header **/
    lazy val name: String = implicitly[ClassTag[H]].runtimeClass.getSimpleName.replace("$minus","-")

    /** header codec **/
    def codec: Codec[H]

    def emailHeaderField: Codec[EmailHeaderField] = codec.upcast

  }

  abstract class DefaultContentHeaderFieldDescription[H <: ContentHeaderField : ClassTag : Typeable]
    extends DefaultHeaderDescription[H] with ContentHeaderDescription[H] {
    def contentHeaderField: Codec[ContentHeaderField] = codec.upcast
  }


}
