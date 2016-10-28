package internet.protocols.coap

import java.net.SocketAddress
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel
import iot.Task

/*
 * constants as seen by client API
 */
object CoapConstants {
  val GET  = 1
  val POST = 2
  val PUT = 3
  val DELETE = 4
  
  val Ok                   = (2 << 5)       // 2.00
  val BadRequest           = (4 << 5)       // 4.00  
  val NotFound             = (4 << 5) | 4   // 4.04  
  val InternalServerError  = (5 << 5)       // 5.00
}

case class CoapResponse (code: Int, payload: Array[Byte])
case class CoapIncomingRequest (message: CoapMessage, from: SocketAddress)
case class CoapPendingResponse (time: Long, request: CoapIncomingRequest)

/*
 * 
 */
class CoapEndpoint (port: Int) extends CoapMessageSerializer with Task {
  /*
   * public API
   */
  
  def registerService (code: Int, payloadStart: Array[Byte], cb: (Int, Array[Byte], Array[Byte]) => Option [CoapResponse]) = {
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
      val (tokenId, token) = nextToken ()
      
      confirmableRequests (tokenId) = cb
      
      val requestMessage = CoapMessage (CoapMessageConstants.ConfirmableType, code, messageId, token, emptyOptions, payload)
      
      debug ("sending request")
      debug (requestMessage)
      send (requestMessage, to)
    }
  }
  
  /*
   * 
   */
  def respondTo (id: Int, code: Int, payload: Array[Byte]) = {
    pendingResponses.get (id) match { 
      case Some (pending) => {
        val to = pending.request.from
        val token = pending.request.message.token
        val options = pending.request.message.options
        
        val coapMessage = CoapMessage (
           msgType = CoapMessageConstants.ConfirmableType, 
           code = code, 
           messageId = nextMessageId (to),
           token = token,
           options = options,
           payload = payload
        )
    
        send (coapMessage, to)    
      }
      
      case None => {
        debug ("Invalid pending response " + id)
      }
    }
  }
  
  /*
   * 
   */
  def run(): Unit = {
    // eventually receive an incoming message and process it
    receive ()
    
    // TO-DO: re-send confirmable message waiting for acknowledge or response
  }
  
  /*
   * private part
   */
  
  val confirmableRequests = new scala.collection.mutable.HashMap [Int, (CoapResponse) => Unit] ()
  
  // keep track of the pending response from the local service 
  val pendingResponses = new scala.collection.mutable.HashMap [Int, CoapPendingResponse] ()
  
  

  def processIncomingMessage (coapMessage: CoapMessage, from: SocketAddress) = {
    if (coapMessage.isRequest) {
      debug ("processing incoming request")
      debug (coapMessage)
      processIncomingRequest (coapMessage, from)
    }
    else if (coapMessage.isResponse) {
      debug ("processing response")
      debug (coapMessage)
      processIncomingResponse (coapMessage, from)
    }
    // TO-DO CoAP ping
    else {
      log ("invalid message code " + coapMessage.code)
    }
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
        // To-DO: use a global local id instead of coapMessage.token 
        service.get.cb (coapMessage.code, coapMessage.token, coapMessage.payload) match {
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
            // TO-DO: distinct clients can use the same token
            val tokenId = fromToken (coapMessage.token)
            
            pendingResponses (tokenId) = CoapPendingResponse (System.currentTimeMillis(), CoapIncomingRequest(coapMessage, from))
            
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
              
    //
    debug ("sending response")
    debug (coapResponseMessage)
    send (coapResponseMessage, from)
  }
  
  /*
   * 
   */
  def processIncomingResponse (coapMessage: CoapMessage, from: SocketAddress) = {
    val tokenId = fromToken (coapMessage.token)
    
    confirmableRequests.get (tokenId) match {
      case Some (cb) => {
        val response = CoapResponse (coapMessage.code, coapMessage.payload)
        confirmableRequests.remove (tokenId)
        cb (response)
      }
      case None => {
        log ("Unexpected token Id " + tokenId)
      }
    }
  }
  
  /*
   * 
   */
  
  /*
   * 
   */
  case class CoapRequestPattern (code: Int, payloadStart: Array[Byte], cb: (Int, Array[Byte], Array[Byte]) => Option [CoapResponse])
  
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
  
  def fromToken (token: Array[Byte]): Int = {
    if (token.length == 1) {
      token (0) & 0xFF
    }
    else if (token.length == 2) {
      parseInt (token, 0)
    }
    else {
      throw new RuntimeException ("token length not supported " + token.length)
    }
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
      val udpPayload = new Array[Byte] (buf.limit())
    
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