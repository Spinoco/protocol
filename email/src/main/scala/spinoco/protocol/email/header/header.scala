package spinoco.protocol.email
import scodec.Codec

/**
  * Created by pach on 13/10/17.
  */
package object header {


  trait EmailHeaderField {

    def name: String

  }



  /** description of header, including its codec **/
  trait HeaderDescription[A <: EmailHeaderField] {


    /** name of the header **/
    def name: String

    /** header codec **/
    def codec: Codec[A]

    def fieldCodec: Codec[EmailHeaderField]

  }


}
