import org.scalatest._
import matchers._
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
  val test255 = toArrayByte ("11111111")
  val test255_0 = toArrayByte ("1111111100000000")

                                 // 0         1         2         3
                                 //  01234567890123456789012345678901  
  val tooSmall =       toArrayByte ("01011111")

                                 // 0         1         2         3
                                 //  01234567890123456789012345678901  
  val invalidVersion = toArrayByte ("11011111111111111111111111111111")

                                 // 0         1         2         3
                                 //  01234567890123456789012345678901  
  val okSmallest     = toArrayByte ("01010000001000000000000100000100")

  val tooSmall2      = toArrayByte ("0101010000100000000000010000010000000000")

  val _TKLReserved   = toArrayByte ("01011001001000000000000100000100" + 
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000" +
                                    "00000000")

  val okTKL4_ABCD    = toArrayByte ("01010100001000000000000100000100" +
                                    "01000001" + // A
                                    "01000010" + // B
                                    "01000011" + // C
                                    "01000100")  // D

  val okTKL0_abcd    = toArrayByte ("01010000001000000000000100000100" +
                                    "11111111" + // 0xFF
                                    "01100001" + // 'a', 97
                                    "01100010" + // 'b', 98
                                    "01100011" + // 'd', 99
                                    "01100100")  // 'd', 100
                                    
  val finishTKL0_FF  = toArrayByte ("01010000001000000000000100000100" +
                                    "11111111")  // 0xFF

  val okTKL0_Option1_3_abcd =   
                       toArrayByte ("01010000001000000000000100000100" +
                                    "00110100" + // delta = 3, length = 4
                                    "01100001" + // 'a', 97
                                    "01100010" + // 'b', 98
                                    "01100011" + // 'd', 99
                                    "01100100")  // 'd', 100
                                    
}

/*
 *
 */
trait CoapMessageMatchers {
  class CoapMessageMatcher(rigth: CoapMessage) extends Matcher[CoapMessage] {

    def compare(left: Array[Byte], rigth: Array[Byte]): Boolean = {
      left.size == rigth.size &&
      {
        var sameContent = true
        for (i <- 0 until left.size)
          if (left(i) != rigth(i)) {
            println ("pos #" + i + ", left: " + left(i) + " != right: " + rigth(i))
            sameContent = false
          }
          
        sameContent
      }
    }

    def compare(left: Array[CoapOption], rigth: Array[CoapOption]): Boolean = {
      left.size == rigth.size &&
      {
        var sameContent = true
        for (i <- 0 until left.size) {
          if (left(i).number != rigth(i).number || ! compare (left(i).value, rigth(i).value)) {
            println ("pos #" + i + ", left: " + left(i) + " != right: " + rigth(i))
            sameContent = false
          }
        }
          
        sameContent
      }
    }

    def compare(left: CoapMessage, rigth: CoapMessage): Boolean = {
      left.version == rigth.version &&
      left.msgType == rigth.msgType &&
      left.tokenLength == rigth.tokenLength &&
      left.code == rigth.code &&
      left.messageId == rigth.messageId &&
      compare(left.token, rigth.token) &&
      compare(left.options, rigth.options) &&
      compare(left.payload, rigth.payload)
    }

    def apply(left: CoapMessage) = {
      MatchResult(
        compare(left, rigth),
        s"""Message $left != "$rigth"""",
        s"""Message $left == "$rigth""""
      )
    }
  }

  def sameMessage(rigth: CoapMessage) = new CoapMessageMatcher(rigth)
}


/*
 *
 */
class MessageSpec extends FlatSpec with Matchers with CoapMessageMatchers {

  val ep = new CoapDestinationEndpoint()

  // test the test method
  "test1" should  "equals 1" in {
    val msg = MessageFactory.test1
    println ("test1: " + msg.mkString (","))
    
    1 should be (msg (0))
  }

  "test255" should  "equals 255" in {
    val msg = MessageFactory.test255
    println ("test255: " + msg.mkString (","))
    
    255.toByte should be (msg (0))
  }

  "test255_0" should  "equals 255, 0" in {
    val msg = MessageFactory.test255_0
    println ("test255: " + msg.mkString (","))
    
    255.toByte should be (msg (0))
    0.toByte should be (msg (1))
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
  
  "smallest" should "be version 1, type 1, tkl 0, code 1.00, message id 300, empty token, options, payload" in {
    val msg = MessageFactory.okSmallest
   
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
   
    cmp should sameMessage (smallest)
  }
 
  // 
  it should "throw CoapMessageFormatException if message is too small (TKL==4)" in {
    a [CoapMessageFormatException] should be thrownBy {
      val msg = MessageFactory.tooSmall2
      
      ep.parsePayload (msg)
    }
  }
  
  // 
  it should "throw CoapMessageFormatException if token length is reserved TKL >= 9" in {
    a [CoapMessageFormatException] should be thrownBy {
      val msg = MessageFactory._TKLReserved
      
      ep.parsePayload (msg)
    }
  }
  
  // 
  "okTKL4_ABCD" should "be version 1, type 1, tkl 4, code 1.00, message id 300, token=ABCD, empty options/payload" in {
    val msg = MessageFactory.okTKL4_ABCD

    val okTKL4_ABCD = ep.parsePayload (msg)

    val token = new Array[Byte] (4)
    token(0) = 'A'.toByte
    token(1) = 'B'.toByte
    token(2) = 'C'.toByte
    token(3) = 'D'.toByte

    val cmp = CoapMessage (
      version = 1,
      msgType = 1,
      tokenLength = 4,
      code = 1 << 5,
      messageId = 256 + 4,
      token = token,
      options = new Array[CoapOption] (0),
      payload = new Array[Byte] (0)
    )
   
    cmp should sameMessage (okTKL4_ABCD)
  }
  
  "okTKL0_abcd" should "be version 1, type 1, tkl 0, code 1.00, message id 300, empty token/options, payload=abcd" in {
    val msg = MessageFactory.okTKL0_abcd

    val okTKL0_abcd = ep.parsePayload (msg)

    val payload = new Array[Byte](4) 
    payload(0) = 'a'.toByte
    payload(1) = 'b'.toByte
    payload(2) = 'c'.toByte
    payload(3) = 'd'.toByte

    val cmp = CoapMessage (
      version = 1,
      msgType = 1,
      tokenLength = 0,
      code = 1 << 5,
      messageId = 256 + 4,
      token = new Array[Byte] (0),
      options = new Array[CoapOption] (0),
      payload = payload
    )
   
    cmp should sameMessage (okTKL0_abcd)
  }
  
  // 
  it should "throw CoapMessageFormatException if finish with 0xFF and no payload" in {
    a [CoapMessageFormatException] should be thrownBy {
      val msg = MessageFactory.finishTKL0_FF
      
      ep.parsePayload (msg)
    }
  }
  
  //
  "okTKL0_Option1_3_abcd" should "be version 1, type 1, tkl 0, code 1.00, message id 300, empty token, options=(3,abcd), empty payload" in {
    val msg = MessageFactory.okTKL0_Option1_3_abcd

    val okTKL0_Option1_3_abcd = ep.parsePayload (msg)

    val options = new Array[CoapOption] (1)
    options(0) = CoapOption (3, "abcd".getBytes)    
    
    val cmp = CoapMessage (
      version = 1,
      msgType = 1,
      tokenLength = 0,
      code = 1 << 5,
      messageId = 256 + 4,
      token = new Array[Byte] (0),
      options = options,
      payload = new Array[Byte] (0)
    )
   
    cmp should sameMessage (okTKL0_Option1_3_abcd)
  }
  
  
}
