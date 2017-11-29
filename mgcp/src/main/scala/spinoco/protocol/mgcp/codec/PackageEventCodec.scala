package spinoco.protocol.mgcp.codec

import scodec.{Codec, Err}
import scodec.bits.BitVector
import scodec.codecs._
import spinoco.protocol.mgcp.PackageEvent
import spinoco.protocol.mgcp.mgcppackage._
import spinoco.protocol.common.codec._

/**
  * Created by pach on 10/03/17.
  */
object PackageEventCodec {
import impl._
  lazy val codec: Codec[PackageEvent] = {

    discriminated[PackageEvent].by(takeWhileChar(ascii)('/') <~ constantString1("/"))
    .typecase("l", linePackageEventCodec)
    .typecase("L", linePackageEventCodec)
    .typecase("d", dtmfPackageEventCodec)
    .typecase("D", dtmfPackageEventCodec)
    .typecase("g", genericPackageEventCodec)
    .typecase("G", genericPackageEventCodec)

  }

  object impl {

    val bracketParams: Codec[List[String]] = "EventParams" | {
      enclosedBy('(',')')(listDelimited(BitVector.view(",".getBytes), ascii))
    }

    val linePackageEventCodec: Codec[LinePackageEvent] = "LinePackage Event" | {
      val callerIdCodec: Codec[LinePackageEvent] =
        bracketParams.xmap(LinePackageEvent("ci", _), _.params)

      val distinctiveTonePatternCodec: Codec[LinePackageEvent] =
        bracketParams.xmap(LinePackageEvent("s", _), _.params)

      val specialInformationToneCodec: Codec[LinePackageEvent] =
        bracketParams.xmap(LinePackageEvent("sit", _), _.params)

      takeWhileChar(ascii)('(').flatZip {
        case "aw" => provide(LinePackageEvent.AnswerTone)
        case "bz" => provide(LinePackageEvent.BusyTone)
        case "ci" => callerIdCodec
        case "dl" => provide(LinePackageEvent.DialTone)
        case "e" => provide(LinePackageEvent.ErrorTone)
        case "hd"=> provide(LinePackageEvent.OffHookTransition)
        case "hf" => provide(LinePackageEvent.FlashHook)
        case "ht" => provide(LinePackageEvent.OnHoldTone)
        case "hu" => provide(LinePackageEvent.OnHookTransition)
        case "lsa" => provide(LinePackageEvent.LineSideAnswer)
        case "mwi"=> provide(LinePackageEvent.MessageWaiting)
        case "nbz"=> provide(LinePackageEvent.NetworkBusy)
        case "oc" => provide(LinePackageEvent.OperationComplete)
        case "of" => provide(LinePackageEvent.OperationFailure)
        case "osi" => provide(LinePackageEvent.NetworkDisconnect)
        case "ot" => provide(LinePackageEvent.OffHookWarningTone)
        case "p" => provide(LinePackageEvent.PromptTone)
        case "rg" => provide(LinePackageEvent.Ringing)
        case "ro" => provide(LinePackageEvent.ReorderTone)
        case "rs" => provide(LinePackageEvent.Ringsplash)
        case "s" => distinctiveTonePatternCodec
        case "sit" => specialInformationToneCodec
        case "sl" => provide(LinePackageEvent.StutterDialTone)
        case "v" => provide(LinePackageEvent.AlertingTone)
        case "vmwi" => provide(LinePackageEvent.VisualMessageWaiting)
        case "wt" => provide(LinePackageEvent.CallWaitingTone)
        case "y" => provide(LinePackageEvent.RecorderWarningTone)
        case "z" => provide(LinePackageEvent.CallingCardServiceTone)
        case "r0" => provide(LinePackageEvent.DistinctiveRinging(0))
        case "r1" => provide(LinePackageEvent.DistinctiveRinging(1))
        case "r2" => provide(LinePackageEvent.DistinctiveRinging(2))
        case "r3" => provide(LinePackageEvent.DistinctiveRinging(3))
        case "r4" => provide(LinePackageEvent.DistinctiveRinging(4))
        case "r5" => provide(LinePackageEvent.DistinctiveRinging(5))
        case "r6" => provide(LinePackageEvent.DistinctiveRinging(6))
        case "r7" => provide(LinePackageEvent.DistinctiveRinging(7))
        case "wt1" => provide(LinePackageEvent.AlternativeCallWT(1))
        case "wt2" => provide(LinePackageEvent.AlternativeCallWT(2))
        case "wt3" => provide(LinePackageEvent.AlternativeCallWT(3))
        case "wt4" => provide(LinePackageEvent.AlternativeCallWT(4))
      }.xmap(
        _._2
        , evt => evt.name -> evt
      )
    }





    val dtmfPackageEventCodec: Codec[DTMFPackageEvent] = {
      val dtmfPackageEventPatternCodec: Codec[DTMFPackagePatternEvent] = "DTMF Package Event" | {
        val digiLetters = Set[Char](
          'A', 'B', 'C', 'D', 'T', 'X'
          , 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'U', 'V', 'W', 'Y', 'Z'
        ).flatMap {c => Set(c, c.toLower)} ++ Set('#', '*', '-')
        def isDigitMap(s:String):Option[Err] = {
          if (s.forall(c => c.isDigit || digiLetters.contains(c))) None
          else Some(Err(s"Unexpected characters in digitMap: $s"))
        }

        ((constantString1("[") ~> guard(takeWhileChar(ascii)(']'))(isDigitMap)) <~ constantString1("]")).as[DTMFPackagePatternEvent]
      }
      val eventCodec: Codec[DTMFEvent] = {
        takeWhileChar(ascii)('(').flatZip {
          case "0" => provide(DTMFPackageEvent.DTMF0)
          case "1" => provide(DTMFPackageEvent.DTMF1)
          case "2" => provide(DTMFPackageEvent.DTMF2)
          case "3" => provide(DTMFPackageEvent.DTMF3)
          case "4" => provide(DTMFPackageEvent.DTMF4)
          case "5" => provide(DTMFPackageEvent.DTMF5)
          case "6" => provide(DTMFPackageEvent.DTMF6)
          case "7" => provide(DTMFPackageEvent.DTMF7)
          case "8" => provide(DTMFPackageEvent.DTMF8)
          case "9" => provide(DTMFPackageEvent.DTMF9)
          case "#" => provide(DTMFPackageEvent.`DTMF#`)
          case "*" => provide(DTMFPackageEvent.`DTMF*`)
          case "A" => provide(DTMFPackageEvent.DTMF9)
          case "B" => provide(DTMFPackageEvent.DTMF9)
          case "C" => provide(DTMFPackageEvent.DTMF9)
          case "D" => provide(DTMFPackageEvent.DTMF9)
          case "DD" => bracketParams.xmap[DTMFEvent](DTMFEvent("DD",_), _.params)
          case "DO" => bracketParams.xmap[DTMFEvent](DTMFEvent("DO",_), _.params)
          case "L" => provide(DTMFPackageEvent.LongDuration)
          case "oc" => provide(DTMFPackageEvent.OperationComplete)
          case "of" => provide(DTMFPackageEvent.OperationFailure)
          case "T" => provide(DTMFPackageEvent.InterDigitTimer)
        }.xmap(
          _._2
          , evt => evt.name -> evt
        )
      }
      choice(
        dtmfPackageEventPatternCodec.upcast
        , eventCodec.upcast
      )
    }

    val genericPackageEventCodec: Codec[GenericPackageEvent] = "Generic Package Event" | {
      takeWhileChar(ascii)('(').flatZip {
        case "cf" => provide(GenericPackageEvent.ConfirmTone)
        case "cg" => provide(GenericPackageEvent.CongestionTone)
        case "ft" => provide(GenericPackageEvent.FaxTone)
        case "it" => provide(GenericPackageEvent.InterceptTone)
        case "ld" => provide(GenericPackageEvent.LongDurationConnection)
        case "mt" => provide(GenericPackageEvent.ModemTone)
        case "oc" => provide(GenericPackageEvent.OperationComplete)
        case "of" => provide(GenericPackageEvent.OperationFailure)
        case "pat" => bracketParams.xmap[GenericPackageEvent](GenericPackageEvent("pat",_), _.params)
        case "pt" => provide(GenericPackageEvent.PreemptionTone)
        case "rbk" => bracketParams.xmap[GenericPackageEvent](GenericPackageEvent("rbk",_), _.params)
        case "rt" => provide(GenericPackageEvent.RingBackTone)
      }.xmap(
        _._2
        , evt => evt.name -> evt
      )
    }

  }

}
