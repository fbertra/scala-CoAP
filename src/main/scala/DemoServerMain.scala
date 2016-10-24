import internet.protocols.coap.CoapEndpoint
import internet.protocols.coap.CoapResponse
import internet.protocols.coap.CoapIncomingRequest
import internet.protocols.coap.CoapConstants

object DemoServerMain {
  
  var numInvocation = 0
  
  /*
   * /calc/sum/1/2 -> 3
   * /calc/sum/10/1 -> 11
   */
  def serviceSum (req: CoapIncomingRequest):  Option [CoapResponse] = {
    numInvocation = numInvocation + 1
    
    try {
      val nums = new String (req.payload.drop (9)).split ("/")
    
      val num0 = nums(0).toInt
      val num1 = nums(1).toInt
      
      val ret = (num0 + num1).toString.getBytes
      
      Some (CoapResponse (CoapConstants.Ok, ret))
    }
    catch {
      case th : Throwable => {
        println ("bad URL")
        th.printStackTrace ()
        
        Some (CoapResponse (CoapConstants.BadRequest, req.payload))
      }
    }
  }
  
  /*
   * 
   */
  def main (args: Array[String]) = {
    val port = if (args.length > 2) {
      args (0).toInt
    }
    else
      9999
      
    val ep = new CoapEndpoint (port)
    
    ep.registerService (CoapConstants.GET, "/calc/sum".getBytes, serviceSum)
    
    println ("Waiting for incoming request on UDP port " + port)
    
    waitForAWhile ()
  }
  
  /*
   * wati until 10 invocation or 5 minutes after the last invocation
   */
  
  def waitForAWhile () = { 
    
    var lastInvocation = numInvocation
    
    var timesWithoutChange = 0

    while (true) {
      if (numInvocation > 10) {
        println ("I'm small and I need to rest")
        System.exit (1)
      }
      
      Thread.sleep (1000L)
      
      if (lastInvocation == numInvocation) 
        timesWithoutChange = timesWithoutChange + 1        
      else {
        lastInvocation = numInvocation
        timesWithoutChange = 0
      }
      
      if (timesWithoutChange > 5 * 30) {
        println ("I'm small and I'm getting bored fast")
        System.exit (1)
      }
    }
  }
}