package internet.protocols.coap

class CoapMessageFormatException(msg: String) extends RuntimeException(msg)

object CoapMessageFormat {
  // if no token, no options and no payload 
  val MinLen = 4
}

/*
 *
 */
class CoapDestinationEndpoint extends CoapUtil {
  /*
   *
   */
  def receive (udpPayload: Array[Byte]): Unit = {
    try {
      val message = parsePayload(udpPayload)

    }
    catch {
      case msgFmtEx: CoapMessageFormatException => {
        log (msgFmtEx.getMessage)
      }
    }
  }

  /*
   * 
   */
  def parseOneOption(udpPayload: Array[Byte], offsetIni: Int, lastOptionNum: Int): Tuple2[Int, Option[CoapOption]] = {
    if (udpPayload.length <= offsetIni || udpPayload (offsetIni) == 0xFF.toByte)
      (offsetIni, None)
    else {
      try {
        val byte0 = udpPayload(offsetIni)

        val smallDelta = byte0 >> 4

        val (offsetAfterDelta, delta) = if (smallDelta == 15)
          throw new CoapMessageFormatException ("Option delta equals 15")
        else if (smallDelta == 14)
          (offsetIni + 3, parseInt(udpPayload, offsetIni+1) - 269)
        else if (smallDelta == 13)
          (offsetIni + 2, udpPayload (offsetIni+1).toInt - 13)
        else
          (offsetIni + 1, smallDelta)

        val smallLength = byte0 & 0x0F
        val (offsetAfterLength, length) = if (smallLength == 15)
          throw new CoapMessageFormatException ("Option length equals 15")
        else if (smallLength == 14)
          (offsetAfterDelta + 2, parseInt(udpPayload, offsetAfterDelta) - 269)
        else if (smallDelta == 13)
          (offsetAfterDelta + 1, udpPayload (offsetAfterDelta).toInt - 13)
        else
          (offsetAfterDelta, smallLength)
        
        val optionValue = new Array[Byte] (length)
        scala.Array.copy(udpPayload, offsetAfterLength, optionValue, 0, length)

        val coapOption = CoapOption(delta + lastOptionNum, optionValue)

        (offsetAfterLength + length, Some(coapOption))
      }
      catch {
        case ex: IndexOutOfBoundsException => {
          // ex.printStackTrace ()
          throw new CoapMessageFormatException ("UDP payload too small " + udpPayload.length + " during options parsing") 
        }
      }
    }
  }

  /*
   *
   */
  def parseOptions(udpPayload: Array[Byte], offsetIni: Int): Tuple2[Int,Array[CoapOption]] = {
    var list: List[CoapOption] = Nil
    var offset = offsetIni
    var finished = false
    var optionNum = 0
    
    while (! finished) {
      val (newOffset, newOption) = parseOneOption(udpPayload, offset, optionNum)
       
      if (newOption.isDefined) {
        // log ("newOption " + new String (newOption.get.value))
        offset = newOffset
        list = newOption.get :: list
        optionNum = newOption.get.number 
      }
      else
        finished = true
    }

    (offset, list.toArray)
  }

  /*
   *
   */
  def parsePayload (udpPayload: Array[Byte]): CoapMessage = {
    import CoapMessageFormat._

    var minLen = MinLen

    if (udpPayload.length < minLen)
      throw new CoapMessageFormatException ("UDP payload too small " + udpPayload.length)

    val byte0 = udpPayload(0)
    
    val version = byte0 >> 6
    if (version != 1)
      throw new CoapMessageFormatException ("Unexpected version " + version)

    val msgType = (byte0 & 0x30) >> 4

    val tokenLength = byte0 & 0x0F
    if (tokenLength >= 9) 
      throw new CoapMessageFormatException ("Reserved token length " + tokenLength)
    minLen += tokenLength
    if (udpPayload.length < minLen) {
      throw new CoapMessageFormatException ("UDP payload too small " + udpPayload.length + " < " + minLen)
    }

    val code = udpPayload(1)

    val messageId = toInt(udpPayload(2), udpPayload(3))

    val token = new Array[Byte](tokenLength)
    scala.Array.copy(udpPayload, MinLen, token, 0, tokenLength)

    val (offsetAfterOptions, options) = parseOptions(udpPayload, minLen)

    val payload = if (offsetAfterOptions < udpPayload.length) {
      val payloadLen = udpPayload.length - (offsetAfterOptions + 1)
      
      if (payloadLen == 0)
        throw new CoapMessageFormatException ("Zero length Payload")
      else {
        val payload = new Array[Byte](payloadLen)
        scala.Array.copy(udpPayload, offsetAfterOptions + 1, payload, 0, payloadLen)
        payload
      }
    }
    else
      new Array[Byte](0)

    CoapMessage (version, msgType, tokenLength, code, messageId, token, options, payload)
  }
  
  /*
   *
   */
  def parseInt (bytes: Array[Byte], offset: Int): Int = {
    toInt(bytes(offset), bytes(offset+1))
  }
}
