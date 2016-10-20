package internet.protocols.coap

/*
 *
 */
class CoapOriginatingEndpoint extends CoapUtil {  

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
