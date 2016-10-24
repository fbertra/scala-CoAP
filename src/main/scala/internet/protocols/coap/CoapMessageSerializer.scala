package internet.protocols.coap

class CoapMessageFormatException(msg: String) extends RuntimeException(msg)

object CoapMessageFormat {
  // if no token, no options and no payload 
  val MinLen = 4
}

/*
 * format and parse a CoapMessage to/form network UDP payload
 */
trait CoapMessageSerializer extends CoapUtil {
  /* parse methods */

  /*
   * 
   */
  def parseOneOption(udpPayload: Array[Byte], offsetIni: Int, lastOptionNum: Int): Tuple2[Int, Option[CoapOption]] = {
    if (udpPayload.length <= offsetIni || udpPayload (offsetIni) == 0xFF.toByte)
      (offsetIni, None)
    else {
      try {
        val byte0 = udpPayload(offsetIni)

        val smallDelta = (byte0 & 0xF0) >>> 4

        val (offsetAfterDelta, delta) = if (smallDelta == 15)
          throw new CoapMessageFormatException ("Option delta equals 15")
        else if (smallDelta == 14)
          (offsetIni + 3, parseInt(udpPayload, offsetIni+1) + 269)
        else if (smallDelta == 13)
          (offsetIni + 2, udpPayload (offsetIni+1).toInt + 13)
        else
          (offsetIni + 1, smallDelta)

        val smallLength = byte0 & 0x0F
        val (offsetAfterLength, length) = if (smallLength == 15.toByte)
          throw new CoapMessageFormatException ("Option length equals 15")
        else if (smallLength == 14)
          (offsetAfterDelta + 2, parseInt(udpPayload, offsetAfterDelta) + 269)
        else if (smallLength == 13)
          (offsetAfterDelta + 1, udpPayload (offsetAfterDelta).toInt + 13)
        else
          (offsetAfterDelta, smallLength)
        
        /*
        debug ("length: " + length)
        debug ("offsetIni: " + offsetIni)
        debug ("offsetAfterDelta: " + offsetAfterDelta)
        debug ("offsetAfterLength: " + offsetAfterLength)
        debug ("smallDelta: " + smallDelta)
        debug ("smallLength: " + smallLength)
        */
        
        val optionValue = new Array[Byte] (length)
        scala.Array.copy(udpPayload, offsetAfterLength, optionValue, 0, length)

        val coapOption = CoapOption(delta + lastOptionNum, optionValue)

        (offsetAfterLength + length, Some(coapOption))
      }
      catch {
        case ex: IndexOutOfBoundsException => {
          // debug (ex)
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

    (offset, list.toArray.reverse)
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
    
    val version = (byte0 & 0xF0) >>> 6
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

    val code = udpPayload(1) & 0xFF

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

    CoapMessage (msgType, code, messageId, token, options, payload)
  }
  
  /* format methods */
  
  /*
   *
   */
  def parseInt (bytes: Array[Byte], offset: Int): Int = {
    toInt(bytes(offset), bytes(offset+1))
  }
 
 
  private def sizeOptions (options: Array[CoapOption]): Int = {
    def sizeNumber (num: Int) = {
      if (num <= 12) 
        0
      else if (num <= 255 + 13) 
        1
      else 
        2
    }

    var num = 0
    var size = 0

    for (option <- options) {
      val delta = option.number - num
      num = option.number

      val sizeDelta = sizeNumber (delta)
      val sizeLen = sizeNumber (option.value.size)

      size += (1 + sizeDelta + sizeLen + option.value.size)
    }

    size
  }

  private def sizePayload(coapMessage: CoapMessage): Int = {
    4 + // header
      coapMessage.token.size + 
      sizeOptions (coapMessage.options) +
      (if (coapMessage.payload.size > 0) {1 + coapMessage.payload.size} else 0) 
  }

  private def formatOption (delta: Int, value: Array[Byte], udpPayload: Array[Byte], offsetIni: Int): Int = {
    def smallBigSpace (num: Int) : Tuple3[Int, Int, Int] = {
      if (num <= 12) 
        (num, 0, 0)
      else if (num <= 255 + 13)
        (13, num - 13, 1)
      else
        (14, num - 269, 2)
    }

    val (smallDelta, bigDelta, spaceDelta) = smallBigSpace (delta)
    val (smallLen, bigLen, spaceLen) = smallBigSpace (value.length)
    
    // debug ("delta: " + delta + " -> " + smallDelta + " / " + bigDelta + " / " + spaceDelta)
    // debug ("len: " + value.length + " -> " + smallLen + " / " + bigLen + " / " + spaceLen)

    var offset = offsetIni
    udpPayload(offset) = ((smallDelta << 4) | smallLen).toByte
    offset = offset + 1

    if (spaceDelta == 1) {
      udpPayload(offset) = bigDelta.toByte
      offset = offset + 1
    }
    else if (spaceDelta == 2) {
      formatInt (udpPayload, offset, bigDelta)
      offset = offset + 2
    }

    if (spaceLen == 1) {
      udpPayload(offset) = bigLen.toByte
      offset = offset + 1
    }
    else if (spaceLen == 2) {
      formatInt (udpPayload, offset, bigLen)
      offset = offset + 2
    }

    scala.Array.copy (value, 0, udpPayload, offset, value.length)

    offset + value.length
  }

  private def formatOptions (options: Array[CoapOption], udpPayload: Array[Byte], offsetIni: Int): Int = {
    var offset = offsetIni

    // 
    var num = 0
    for (option <- options) {
      val delta = option.number - num
      num = option.number

      offset = formatOption (delta, option.value, udpPayload, offset)
    }
    
    offset
  }

  /*
   *
   */
  def formatPayload (coapMessage: CoapMessage): Array[Byte] = {
    val udpPayload = new Array[Byte] (sizePayload (coapMessage))

    // header
    udpPayload(0) = ((1 << 6) | (coapMessage.msgType << 4) | coapMessage.token.size).toByte
    udpPayload(1) = coapMessage.code.toByte
    formatInt (udpPayload, 2, coapMessage.messageId)
    
    // token
    scala.Array.copy (coapMessage.token, 0, udpPayload, 4, coapMessage.token.size)

    // options
    var offset = formatOptions(coapMessage.options, udpPayload, 4 + coapMessage.token.size)

    if (coapMessage.payload.size > 0) {
      // marker
      udpPayload(offset) = 0xFF.toByte
      offset = offset + 1
      // payload
      scala.Array.copy (coapMessage.payload, 0, udpPayload, offset, coapMessage.payload.size)
    }

    udpPayload
  }


  def formatInt (bytes: Array[Byte], offset: Int, i: Int) = {
    val (byte0, byte1) = fromInt (i)
    
    bytes(offset) = byte0
    bytes(offset + 1) = byte1
  }
  
}