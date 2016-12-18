package internet.protocols.coap

trait CoapUtil {
  def log(msg: String) = {
    println(msg)
  }
  
  def log(msg: String, th: Throwable) = {
    println("CoAP " + msg)
  }
  
  def logMessageError(payload: Array[Byte], th: Throwable) = {
    println(th.getMessage)
    println(payload.mkString (","))
  }
  
  def logMessageError(coapMessage: CoapMessage, th: Throwable) = {
    println(th.getMessage)
    println(coapMessage.toString)
  }
  
  def debug(msg: String) = {
    println("CoAP " + msg)
  }
  
  def debug (coapMessage: CoapMessage) = {
    println("CoAP message: type=" + coapMessage.msgType + ", code=" + coapMessage.codeC + "." + coapMessage.codeDD + ", message id=" + coapMessage.messageId + ", token=[" + coapMessage.token.mkString(",") + "], payload=[" + coapMessage.payload.mkString(",") + "]" )
  }

  def debug (th: Throwable) {
    th.printStackTrace ()
  }

  def toInt(byte0: Byte, byte1: Byte): Int = {
    val int0 = (byte0 & 0xFF).toInt
    int0 * 256 + (byte1 & 0xFF).toInt
  }

  def fromInt (i: Int): Tuple2[Byte, Byte] = {
    val byte0 = (i >>> 8).toByte
    val byte1 = (i & 0xFF).toByte
    
    (byte0, byte1)
  }
  
  def parseInt (bytes: Array[Byte], offset: Int): Int = {
    toInt(bytes(offset), bytes(offset+1))
  } 
}
