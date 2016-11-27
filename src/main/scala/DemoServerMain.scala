import internet.protocols.coap.CoapConstants
import internet.protocols.coap.CoapEndpoint
import internet.protocols.coap.CoapResponse
import internet.protocols.coap.CoapPendingResponse
import iot.Scheduler
import iot.Task
import iot.VerySimpleScheduler

/*
 * 
 */
class DemoServer (ep: CoapEndpoint) extends Task {
  
  // inmediate response
  ep.registerService (CoapConstants.GET, "/calc/sum".getBytes, serviceSum)
  
  // delayed response
  ep.registerDelayedService (CoapConstants.GET, "/calc/slow".getBytes, serviceSlow)
    
  // 
  var pendingResponse: Option [CoapPendingResponse] = None
  
  /*
   * /calc/sum/1/2 -> 3
   * /calc/sum/10/1 -> 11
   */
  def serviceSum (code: Int, token: Array[Byte], payload: Array[Byte]): CoapResponse = {
    incrInvocation()
      
    val szPayload = new String (payload)
    println ("new incoming request with payload: " + szPayload)
      
    val nums = szPayload.substring (10).split ("/")

    //
    if (nums.size != 2)
      CoapResponse (CoapConstants.BadRequest, payload)
    else {
      val num0 = nums(0).toInt
      val num1 = nums(1).toInt
      
      val ret = (num0 + num1).toString.getBytes
      
      CoapResponse (CoapConstants.Ok, ret)
    }
  }
  
  /*
   * /calc/slow/xyz -> xyz
   */
  def serviceSlow (pending: CoapPendingResponse): Unit = {
    incrInvocation()
    
    pendingResponse = Some (pending)
  }
  
  /*
   * 
   */
  def runTask (scheduler: Scheduler) = {
    if (pendingResponse.isDefined) {
      // TO-DO: send a delayed response
      
      pendingResponse.get.respond (CoapConstants.Ok, pendingResponse.get.request.message.payload)
      
      pendingResponse = None
    }
    
    // 
    if (numInvocation > 10) {
      println ("I'm small and I need to rest")
      scheduler.shutdown ()
    }
      
    timesWithoutChange = timesWithoutChange + 1        
      
    if (timesWithoutChange > 100) {
      println ("I'm small and I'm getting bored fast")
      scheduler.shutdown ()
    }
   }
  
  //
  var numInvocation = 0
  var timesWithoutChange = 0

  
  def incrInvocation () = {
    numInvocation = numInvocation + 1
    timesWithoutChange = 0
  }
}

/*
 *  
 */
object DemoServerMain {
  
  /*
   * 
   */
  def main (args: Array[String]) = {
    val port = if (args.length > 2) {
      args (0).toInt
    }
    else
      9999
      
    val scheduler = new VerySimpleScheduler
  
    val ep = new CoapEndpoint (port)
    
    scheduler.addTask (ep)
    scheduler.addTask (new DemoServer (ep)) 
    
    println ("Waiting for incoming request on UDP port " + port)
    
    controlLoop (scheduler)
  }
  
  /*
   * 
   */ 
  def controlLoop (scheduler: Scheduler) = {
    new Thread (scheduler).run ()
  }
}
