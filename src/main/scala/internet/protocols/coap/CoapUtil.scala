package internet.protocols.coap

trait CoapUtil {
  def log(msg: String) = {
    println(msg)
  }
  
  def log(msg: String, th: Throwable) = {
    println(msg)
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
    println(msg)
  }

  def debug (th: Throwable) {
    th.printStackTrace ()
  }

  def toInt(byte0: Byte, byte1: Byte): Int = {
    val int0 = byte0.toInt
    int0 * 256 + byte1.toInt
  }

  def fromInt (i: Int): Tuple2[Byte, Byte] = {
    val byte0 = (i >>> 8).toByte
    val byte1 = (i & 0xFF).toByte
    
    (byte0, byte1)
  }
}
