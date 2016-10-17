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
  def parsePayload (udpPayload: Array[Byte]): CoapMessage = {
    import CoapMessageFormat._
    
    if (udpPayload.length < MinLen)
      throw new CoapMessageFormatException ("UDP payload too small " + udpPayload.length)

    val byte0 = udpPayload(0)
    
    val version = byte0 >> 6
    if (version != 1)
      throw new CoapMessageFormatException ("Unexpected version " + version)

    val msgType = (byte0 & 0x30) >> 4
    
    val tokenLength = byte0 & 0x0F
    
    val code = udpPayload(1)
    
    val messageId = 0
    
    val token = new Array[Byte](0)
    
    val options = new Array[CoapOption](0)
    
    val payload = new Array[Byte](0)
    
    CoapMessage (version, msgType, tokenLength, code, messageId, token, options, payload)
  }
}
