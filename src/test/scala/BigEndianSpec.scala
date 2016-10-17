import org.scalatest._

class BigEndianSpec extends FlatSpec with Matchers {

  "coapUtil.toInt" should "convert big endian" in {
    val byte0: Byte = 0x03
    val byte1: Byte = 0x48
    
    println (byte0)
    println (byte1)
    
    val util = new Object with internet.protocols.coap.CoapUtil
    
    val res: Int = 0x0348
    
    println (res)
    
    util.toInt(byte0, byte1) should be (res)
  }
}
