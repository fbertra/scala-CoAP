package internet.protocols.coap

import java.net.SocketAddress
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

/*
 * constants as seen by client API
 */
object CoapConstants {
  val GET  = 0
  val POST = 1
  val PUT = 2
  val DELETE = 4
  
  val Ok                   = (2 << 6)       // 2.00
  val BadRequest           = (4 << 6)       // 4.00  
  val NotFound             = (4 << 6) | 4   // 4.04  
  val InternalServerError  = (5 << 6)       // 5.00
}

case class CoapResponse (code: Int, payload: Array[Byte])
case class CoapIncomingRequest (code: Int, payload: Array[Byte], from: SocketAddress)

/*
 * 
 */
class CoapEndpoint (port: Int) extends CoapMessageSerializer {
  /*
   * public API
   */
  
  def registerService (code: Int, payloadStart: Array[Byte], cb: (CoapIncomingRequest) => Option [CoapResponse]) = {
    services = CoapRequestPattern (code, payloadStart, cb) :: services 
  }
  
  /*
   * asincroneous. Response will be processed in callback cb
   * 
   * Note: request are confirmable by default
   */
  def request (code: Int, payload: Array[Byte], to: SocketAddress) (cb: (CoapResponse) => Unit) = {
    if (code < CoapConstants.GET || code > CoapConstants.DELETE) {
      cb (new CoapResponse (CoapConstants.BadRequest, empty))
    }
    else {
      val messageId = nextMessageId (to)
      val token = nextToken ()
      
      val requestMessage = CoapMessage (CoapMessageConstants.ConfirmableType, code, messageId, token, emptyOptions, payload)
      
      send (requestMessage, to)
    }
  }
  
  /*
   * private part
   */

  def processIncomingMessage (coapMessage: CoapMessage, from: SocketAddress) = {
    if (coapMessage.isRequest)
      processIncomingRequest (coapMessage, from)
    else if (coapMessage.isResponse)
      processIncomingResponse (coapMessage, from)
  }
  
  /*
   * 
   */
  def processIncomingRequest (coapMessage: CoapMessage, from: SocketAddress) = {
    val service = services.find (service => service.code == coapMessage.code && coapMessage.payload.startsWith(service.payloadStart))
    
    val coapResponseMessage = if (service.isEmpty) {
      coapMessage.copy (msgType = CoapMessageConstants.AcknowledgementType, code = CoapConstants.NotFound)      
    }
    
    else {
      try {
        service.get.cb (CoapIncomingRequest (coapMessage.code, coapMessage.payload, from)) match {
          case Some (response) => { 
            // piggyback the response
            CoapMessage (
               msgType = CoapMessageConstants.AcknowledgementType, 
               code = response.code, 
               messageId = nextMessageId (from),
               token = coapMessage.token,
               options = coapMessage.options,
               payload = response.payload
             )
          }
          case None => {
            // acknowledge the message, the service will respond later
            CoapMessage (
               msgType = CoapMessageConstants.AcknowledgementType, 
               code = 0, 
               messageId = coapMessage.messageId,
               token = empty,
               options = emptyOptions,
               payload = empty
             )
          }
        }
      }
      catch {
        case th: Throwable => {
          logMessageError (coapMessage, th)
          
          coapMessage.copy (msgType = CoapMessageConstants.AcknowledgementType, code = CoapConstants.InternalServerError)
        }
      }
    }
              
    send (coapResponseMessage, from)
  }
  
  /*
   * 
   */
  def processIncomingResponse (coapMessage: CoapMessage, from: SocketAddress) = {
    
  }
  
  /*
   * 
   */
  
  def sendResponse (coapMessage: CoapMessage, to: SocketAddress) = {
    
  }  

  /*
   * 
   */
  case class CoapRequestPattern (code: Int, payloadStart: Array[Byte], cb: (CoapIncomingRequest) => Option [CoapResponse])
  
  var services: List [CoapRequestPattern] = Nil

  val empty = new Array[Byte](0)
  val emptyOptions = new Array[CoapOption] (0)
  
  /*
   * TO-DO: should be by origin/destination address 
   */
  var messageId = 1
  
  /*
   * TO-DO: not secure at all
   */
  def nextMessageId (to: SocketAddress) = {
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
  def nextToken (): Array[Byte] = {
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
    
    token
  }
  
  /*
   * UDP level with java.nio
   */
  
  // mono thread, one receive or one send at the same time
  val buf = ByteBuffer.allocate(2048)
  
  val channel = DatagramChannel.open ()
  channel.socket ().bind (new InetSocketAddress (port))
  channel.configureBlocking(false)
  
  /*
   *
   */
  private def receive (): Unit = {
    
    buf.clear()
    val from = channel.receive(buf)
    buf.flip()
  
    if (from != null) {
      val udpPayload = new Array[Byte] (buf.capacity)
    
      buf.get (udpPayload)
    
      val message = try {
        Some (parsePayload (udpPayload))
      }
      catch {
        case th: Throwable => logMessageError (udpPayload, th); None
      }
    
      message.foreach (processIncomingMessage (_, from))
    }
  }

  /*
   *
   */
  private def send (message: CoapMessage, to: SocketAddress): Int = {
    val payload = formatPayload (message)
    
    buf.clear()
    buf.put(payload)
    buf.flip()

    channel.send (buf, to)
  }
}