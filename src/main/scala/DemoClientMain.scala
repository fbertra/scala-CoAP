import java.net.InetSocketAddress
import internet.protocols.coap.CoapConstants
import internet.protocols.coap.CoapEndpoint
import internet.protocols.coap.CoapResponse
import iot.Task
import iot.VerySimpleScheduler
import iot.Scheduler
import internet.protocols.coap.CoapPendingResponse


/*
 * 
 */
class DemoClient (ep: CoapEndpoint) extends Task {
  var incr = 0
  
  val to = new InetSocketAddress ("127.0.0.1", 9999)
  
  ep.registerDelayedService (CoapConstants.PUT, "/measure/".getBytes, printMeasure)
  
  /*
   * 
   */
  def printMeasure (pending: CoapPendingResponse) {
    println ("measure " + new String (pending.request.message.payload))
  }
      
  /*
   * 
   */
  def runTask (scheduler: Scheduler): Unit = {
    println ("run task " + incr)
    
    if (incr == 0)
      sendSum
    else if (incr == 19)
      sendSlow
      
    incr += 1

    if (incr > 100) {
      scheduler.shutdown()
    }
      
  }
  
  def sendSum = send ("/calc/sum/1/2")
  
  def sendSlow = send ("/calc/slow")
  
  def send (payload: String) = {
    println ("sending request: " + payload)
    
    ep.request(CoapConstants.GET, payload.getBytes(), to)(processResponse)
  }
  
  /*
   * 
   */  
  def processResponse (response: CoapResponse): Unit = {
    val sz = new String (response.payload)
    
    println ("response payload " + sz)
  }
}


/*
 * 
 */
object DemoClientMain {  
  /*
   * 
   */
  def main (args: Array[String]) = {
    val port = if (args.length > 2) {
      args (0).toInt
    }
    else
      10000
      
    println ("Sending coap message from UDP port " + port)
    
    val ep = new CoapEndpoint (port)
    val sender = new DemoClient (ep)
    
    val scheduler = new VerySimpleScheduler
  
    scheduler.addTask (ep)
    scheduler.addTask (sender)
    
    controlLoop (scheduler)
  }
  
  /*
   * 
   */ 
  def controlLoop (scheduler: Scheduler) = {
    new Thread (scheduler).run ()
  }
}