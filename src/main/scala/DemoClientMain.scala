import java.net.InetSocketAddress

import internet.protocols.coap.CoapConstants
import internet.protocols.coap.CoapEndpoint
import internet.protocols.coap.CoapResponse
import iot.Task


object DemoClientMain extends Task {
  
  val scheduler = new VerySimpleScheduler
  
  var ep: CoapEndpoint = null
  
  /*
   * 
   */
  def main (args: Array[String]) = {
    val port = if (args.length > 2) {
      args (0).toInt
    }
    else
      10000
      
    ep = new CoapEndpoint (port)
    
    scheduler.addTask (ep)
    scheduler.addTask (this)
    
    println ("Sending coap message from UDP port " + port)
    
    new Thread (scheduler).run ()
  }
  
  var finished = false
  
  /*
   * 
   */
  
  def processResponse (response: CoapResponse): Unit = {
    val sz = new String (response.payload)
    
    println ("response payload " + sz)
  }

  /*
   * 
   */
  def run (): Unit = {
    if (! finished) {
      val to = new InetSocketAddress ("127.0.0.1", 9999)
      
      val payload = "/calc/sum/1/2".getBytes()
      
      ep.request(CoapConstants.GET, payload, to)(processResponse)
      
      finished = true
    }
  }
}
