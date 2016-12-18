package internet.protocols.coap

import java.net.SocketAddress
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel
import iot.Task
import iot.Scheduler

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

case class CoapPendingResponse (time: Long, request: CoapIncomingRequest, endpoint: CoapEndpoint) {
  def respond (code: Int, payload: Array[Byte]) = {
    endpoint.sendResponse (this, code, payload)
  }
}

/*
 * 
 */
class CoapEndpoint (port: Int) extends CoapMessageSerializer with Task {
  /*
   * public API
   */
  
  /*
   * callback cb will always respond inmediately
   */
  def registerService (code: Int, payloadStart: Array[Byte], cb: (Int, Array[Byte], Array[Byte]) => CoapResponse) = {
    services = CoapRequestPattern (code, payloadStart, Left (cb)) :: services 
  }
  
  /*
   * callback cb may respond later
   * 
   * the service is responsible to keep track of pending response
   */
  def registerDelayedService (code: Int, payloadStart: Array[Byte], cb: (CoapPendingResponse) => Unit) = {
    services = CoapRequestPattern (code, payloadStart, Right (cb)) :: services 
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
      val transmission = getOrCreateRetransmission (to)
      
      val (messageId, tokenId, token) = transmission.newConfirmableRequest (cb)
      
      val requestMessage = CoapMessage (CoapMessageConstants.ConfirmableType, code, messageId, token, emptyOptions, payload)
      
      debug ("sending request")
      debug (requestMessage)
      udpIntf.send (requestMessage, to)
    }
  }
  
  /*
   * useful for a stream of events
   * 
   * non confirmable by default
   * PUT by default
   */
  def fireAndForget (payload: Array[Byte], to: SocketAddress) {
    val transmission = getOrCreateRetransmission (to)
    val messageId = transmission.nextMessageId ()
      
    val message = CoapMessage (CoapMessageConstants.NonConfirmableType, CoapConstants.PUT, messageId, empty, emptyOptions, payload)
    
    debug ("sending fire and forget message")
    debug (message)
    udpIntf.send (message, to)
  }
  
  /*
   * 
   */
  def runTask(scheduler: Scheduler): Unit = {
    // eventually receive an incoming message and process it
    udpIntf.receive ()
    
    // TO-DO: re-send confirmable message waiting for acknowledge or response
  }
  
  /*
   * private part
   */
  
  val udpIntf = new UdpIntf (port, this)
  
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
    else if (coapMessage.isEmpty) {
      debug ("processing acknowledgement for message id " + coapMessage.messageId) 
    }
    else {
      log ("invalid message code " + coapMessage.code)
    }
  }
  
  /*
   * 
   */
  def requestMethodName (basicMethod: Int): String = {
    basicMethod match {
      case CoapConstants.GET    => "GET"
      case CoapConstants.PUT    => "PUT"
      case CoapConstants.POST   => "POST"
      case CoapConstants.DELETE => "DELETE"
    }
  }
  
  /*
   * 
   */
  private def processIncomingRequest (coapMessage: CoapMessage, from: SocketAddress) = {
    val service = services.find (service => service.code == coapMessage.code && coapMessage.payload.startsWith(service.payloadStart))

    // the default response type
    val responseType = if (coapMessage.isConfirmable) 
      // piggyback the response
      CoapMessageConstants.AcknowledgementType 
    else 
      CoapMessageConstants.NonConfirmableType
              
    val coapResponseMessage = if (service.isEmpty) {
      log ("service not found: " + requestMethodName (coapMessage.code) + " " + new String (coapMessage.payload))
      Some (coapMessage.copy (msgType = responseType, code = CoapConstants.NotFound))      
    }
    
    else {
      try {
        service.get.cb match {
          
          case Left (cb) => {
            val response = cb (coapMessage.code, coapMessage.token, coapMessage.payload) 
            
            Some (CoapMessage (
              msgType = responseType,  
              code = response.code, 
              messageId = coapMessage.messageId,
              token = coapMessage.token,
              options = coapMessage.options,
              payload = response.payload
            ))
          }
          
          case Right (cb) => {
            val incomingRequest = CoapIncomingRequest(coapMessage, from)
            
            val pending = CoapPendingResponse (System.currentTimeMillis(), incomingRequest, this)
            
            cb (pending)
            
            if (coapMessage.isConfirmable) {
              // only acknowledge the message, the service should respond later
              Some (CoapMessage (
               msgType = CoapMessageConstants.AcknowledgementType, 
               code = 0, 
               messageId = coapMessage.messageId,
               token = empty,
               options = emptyOptions,
               payload = empty
             ))
            }
            else
              None
          }
        }
      }
      catch {
        case th: Throwable => {
          logMessageError (coapMessage, th)
          
          // TO-DO: see if Reset is better
          Some (coapMessage.copy (msgType = responseType, code = CoapConstants.InternalServerError))
        }
      }
    }
              
    //
    coapResponseMessage match {
      case Some (message) => {
        debug ("sending response")
        debug (message)
        udpIntf.send (message, from)
      }
      case None => {
        debug ("non confirmable message don't need acknowledge")
      }      
    }
  }
  
  /*
   * 
   */
  private def processIncomingResponse (coapMessage: CoapMessage, from: SocketAddress) = {
    val transmission = getOrCreateRetransmission (from)
      
    val cb = transmission.newResponse (coapMessage)
    
    cb.foreach (callback => {
      val response = CoapResponse (coapMessage.code, coapMessage.payload)
      
      callback (response)
    })

  }
  
  /*
   * 
   */
  private[coap] def sendResponse (pendingResponse: CoapPendingResponse, code: Int, payload: Array[Byte]) = {
    val _request = pendingResponse.request.message
    
    val transmission = getOrCreateRetransmission (pendingResponse.request.from)
      
    val coapMessage = _request.copy (
      code = code, 
      messageId = transmission.nextMessageId (),
      payload = payload
    )
        
    assert (coapMessage.isResponse)
        
    debug ("sending response")
    debug (coapMessage)
   
    udpIntf.send (coapMessage, pendingResponse.request.from)
  }

  
  /*
   * 
   */
  case class CoapRequestPattern (code: Int, payloadStart: Array[Byte], cb: Either [(Int, Array[Byte], Array[Byte]) => CoapResponse, (CoapPendingResponse) => Unit])
  
  var services: List [CoapRequestPattern] = Nil

  val empty = new Array[Byte](0)
  val emptyOptions = new Array[CoapOption] (0)
  
  /*
   * 
   */

  private var transmissionMap = new scala.collection.mutable.HashMap [SocketAddress, CoapRetransmission] ()
  
  private def getOrCreateRetransmission (addr: SocketAddress): CoapRetransmission = {
    transmissionMap.get (addr) match {
      case Some (transmission) => transmission
      case None => {
        val transmission = new CoapRetransmission ()
        transmissionMap (addr) = transmission
        transmission
      }
    }
  }
  
}