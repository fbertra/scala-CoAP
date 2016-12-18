package internet.protocols.coap

import java.net.SocketAddress

/*
 * keep track of messages sent to a destination and received from an origin
 */
class CoapRetransmission extends CoapUtil {
  val confirmableRequests = new scala.collection.mutable.HashMap [Int, (CoapResponse) => Unit] ()
  
  def newConfirmableRequest (cb: (CoapResponse) => Unit): Tuple3 [Int, Int, Array[Byte]] = {
    val messageId = nextMessageId ()
      
    val (tokenId, token) = nextToken ()
      
    confirmableRequests (tokenId) = cb
      
    (messageId, tokenId, token)  
  }

  /*
   * 
   */
  def newResponse (coapMessage: CoapMessage) = {
    val tokenId = fromToken (coapMessage.token)
    
    confirmableRequests.get (tokenId) match {
      case Some (cb) => {
        confirmableRequests.remove (tokenId)
        Some (cb)
      }
      case None => {
        log ("Unexpected token id in response: " + tokenId)        
        None
      }
    }   
  } 

  var messageId = 1
  
  /*
   *
   */
  def nextMessageId () = {
    messageId = messageId + 1
    if (messageId >= Short.MaxValue)
      messageId = 0
    
    messageId
  }
  
  /*
   * 
   */
  
  var tokenId = 1000
  
  /*
   * TO-DO: not secure at all
   */
  def nextToken (): Tuple2 [Int, Array[Byte]] = {
    tokenId = tokenId + 1 
    
    if (tokenId <= 0)
      tokenId = 0
   
    val token = if (tokenId <= Byte.MaxValue) {
      val ret = new Array[Byte](1)
      ret(0) = tokenId.toByte
      ret
    }
    else if (tokenId <= Short.MaxValue) {
      val ret = new Array[Byte](2)
      val (byte0, byte1) = fromInt(tokenId)
      ret(0) = byte0
      ret(1) = byte1
      ret
    }
    else {
      val ret = new Array[Byte](4)
      val (byte0, byte1) = fromInt(tokenId >>> 16)
      ret(0) = byte0
      ret(1) = byte1
      val (byte2, byte3) = fromInt(tokenId & 0xFFFF)
      ret(2) = byte2
      ret(3) = byte3
      ret      
    }
    
    (tokenId, token)
  }

  /*
   * TO-DO: support all valid token length
   */
  
  def fromToken (token: Array[Byte]): Int = {
    if (token.length == 1) {
      token (0) & 0xFF
    }
    else if (token.length == 2) {
      parseInt (token, 0)
    }
    else if (token.length == 0) {
      0
    }
    else {
      throw new RuntimeException ("Unsupported token length: " + token.length)
    }
  }
  
  
}