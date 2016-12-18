package internet.protocols.coap

import java.net.SocketAddress
import java.nio.channels.DatagramChannel
import java.nio.ByteBuffer
import java.net.InetSocketAddress

/*
 * UDP level with java.nio
 */
  
class UdpIntf (port: Int, ep: CoapEndpoint) {
  // mono thread, one receive or one send at the same time
  val buf = ByteBuffer.allocate(2048)
  
  val channel = DatagramChannel.open ()
  
  channel.socket ().bind (new InetSocketAddress (port))
  channel.configureBlocking(false)
  
  /*
   *
   */
  def receive (): Unit = {
    
    buf.clear()
    val from = channel.receive(buf)
    buf.flip()
  
    if (from != null) {
      val udpPayload = new Array[Byte] (buf.limit())
    
      buf.get (udpPayload)
    
      val message = try {
        Some (ep.parsePayload (udpPayload))
      }
      catch {
        case th: Throwable => ep.logMessageError (udpPayload, th); None
      }
    
      message.foreach (ep.processIncomingMessage (_, from))
    }
  }

  /*
   *
   */
  def send (message: CoapMessage, to: SocketAddress): Int = {
    val payload = ep.formatPayload (message)
    
    buf.clear()
    buf.put(payload)
    buf.flip()

    channel.send (buf, to)
  }  
}