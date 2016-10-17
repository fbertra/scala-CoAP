import org.scalatest._
import internet.protocols.coap._

/*
  0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |Ver| T |  TKL  |      Code     |          Message ID           |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |   Token (if any, TKL bytes) ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |   Options (if any) ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |1 1 1 1 1 1 1 1|    Payload (if any) ...
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
*/

object MessageFactory {
  
  def toArrayByte(sz: String): Array [Byte] = {
    if (sz.length % 8 != 0) {
      throw new RuntimeException ("Cannot convert to array[Byte], length is " + sz.length)
    }
    
    val invalid = sz.exists (c => ! (c == '0' || c == '1'))    
    if (invalid) {
      throw new RuntimeException ("Cannot convert to array[Byte], all chars should be 0 or 1")
    }

    val numBytes = sz.length / 8

    val ret = new Array[Byte] (numBytes)

    for (i <- 0 until numBytes) {
      var byte = 0

      for (j <- 0 until 8) {
        byte = byte << 1
        
        val ch = sz.charAt (i * 8 + j)
        val bit = if (ch == '1') 1 else 0

        byte = byte + bit
      }

      ret (i) = byte.toByte
    }
    
    ret
  }
  
  val test1 = toArrayByte ("00000001")
  
                           // 0         1         2         3
                           //  01234567890123456789012345678901  
  val tooSmall = toArrayByte ("01011111")
  
                                 // 0         1         2         3
                                 //  01234567890123456789012345678901  
  val invalidVersion = toArrayByte ("11011111111111111111111111111111")
  
                           // 0         1         2         3
                           //  01234567890123456789012345678901  
  val smallest = toArrayByte ("01010000001000000000000100000100")
  
}


class MessageSpec extends FlatSpec with Matchers {

  val ep = new CoapDestinationEndpoint()
  
  // test the test method
  "test1" should  "equals 1" in {
    val msg = MessageFactory.test1
    println ("test1: " + msg.mkString (","))
    
    1 should be (msg (0))
  }
 
  // 
  it should "throw CoapMessageFormatException if message is too small" in {
    a [CoapMessageFormatException] should be thrownBy {
      val msg = MessageFactory.tooSmall
      
      println ("too small: " + msg.mkString (","))

      ep.parsePayload (msg)
    }
  } 
  
  it should "throw CoapMessageFormatException if version not equals to 1" in {
    a [CoapMessageFormatException] should be thrownBy {
      val msg = MessageFactory.invalidVersion
      println ("invalid version: " + msg.mkString (","))

      ep.parsePayload (msg)
    }
  } 
  
 "smallest" should  "be version 1, type 1, tkl 0, code 1.00, message id 300" in {
   val msg = MessageFactory.smallest
   println ("smallest: " + msg.mkString (","))
   
   val smallest = ep.parsePayload (msg)
   
   val cmp = CoapMessage (
     version = 1, 
     msgType = 1, 
     tokenLength = 0, 
     code = 1 << 5,
     messageId = 256 + 4,
     token = new Array[Byte] (0),
     options = new Array[CoapOption] (0),
     payload = new Array[Byte] (0)
   )
   
   cmp should be (smallest)
 }
}
