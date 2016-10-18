package internet.protocols.coap

trait CoapUtil {
  def log(msg: String) = {
    println(msg)
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
}
