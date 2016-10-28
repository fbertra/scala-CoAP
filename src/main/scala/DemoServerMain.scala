import internet.protocols.coap.CoapConstants
import internet.protocols.coap.CoapEndpoint
import internet.protocols.coap.CoapIncomingRequest
import internet.protocols.coap.CoapResponse
import iot.Task

/*
 * 
 */

object DemoServerMain extends Task {
  
  // 
  var pendingResponse: Option [Tuple3 [Int, Int, Array[Byte]]] = None
  
  /*
   * /calc/sum/1/2 -> 3
   * /calc/sum/10/1 -> 11
   */
  def serviceSum (code: Int, token: Array[Byte], payload: Array[Byte]):  Option [CoapResponse] = {
    scheduler.incrInvocation()
      
    val szPayload = new String (payload)
    println ("new incoming request with payload: " + szPayload)
      
    val nums = szPayload.substring (10).split ("/")

    //
    if (nums.size != 2)
      Some (CoapResponse (CoapConstants.BadRequest, payload))
    else {
      val num0 = nums(0).toInt
      val num1 = nums(1).toInt
      
      val ret = (num0 + num1).toString.getBytes
      
      if (num0 < 1000 && num1 < 1000)
        Some (CoapResponse (CoapConstants.Ok, ret))
      else {
        pendingResponse = Some ((num0, num1, token))
        // simulate a lenghty service: acknowledge the message first and respond later
        None
      }          
    }
  }
  
  val scheduler = new VerySimpleScheduler
  
  var ep: CoapEndpoint = null
  

  def run () = {
    if (pendingResponse.isDefined) {
      
      pendingResponse = None
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
      
    ep = new CoapEndpoint (port)
    
    scheduler.addTask (ep)
    scheduler.addTask (this) 
    
    ep.registerService (CoapConstants.GET, "/calc/sum".getBytes, serviceSum)
    
    println ("Waiting for incoming request on UDP port " + port)
    
    new Thread (scheduler).run ()
  }
}

/*
 * simulate an mono-thread app which must run different tasks
 */

class VerySimpleScheduler extends Runnable {
  var tasks: List[Task] = Nil
  
  def run (): Unit = {
    while (true) {
      for (task <- tasks) {
        task.run ()
      }
                    
      waitForAWhile ()
    }
  }
  
  /*
   * 
   */
  def addTask (task: Task) = {
    tasks = task :: tasks
  }
  
  //
  var numInvocation = 0
  var timesWithoutChange = 0

  
  def incrInvocation () = {
    numInvocation = numInvocation + 1
    timesWithoutChange = 0
  }
  
  /*
   * wait until 10 invocation or 5 minutes after the last invocation
   */
  
  def waitForAWhile () = {     
    if (numInvocation > 10) {
      println ("I'm small and I need to rest")
      System.exit (1)
    }
      
    Thread.sleep (100L)
      
    timesWithoutChange = timesWithoutChange + 1        
      
    if (timesWithoutChange > 5 * 30 * 10) {
      println ("I'm small and I'm getting bored fast")
      System.exit (1)
    }
  }
}