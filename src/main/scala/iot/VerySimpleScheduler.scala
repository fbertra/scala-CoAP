package iot

/*
 * to simulate a mono-thread app which must run different tasks
 */

class VerySimpleScheduler extends Scheduler {
  var tasks: List[Task] = Nil
  
  def run (): Unit = {
    while (true) {
      for (task <- tasks) {
        task.runTask (this)
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
  
  /*
   * 
   */
  def removeTask (task: Task) = {
    tasks = tasks.filter (_ != task)
    
    if (tasks.isEmpty)
      shutdown ()
  }
  
  /*
   * 
   */
  def shutdown () = {
    println ("shutting down")
    System.exit (1)
  }
  
  /*
   * wait until 10 invocation or 5 minutes after the last invocation
   */
  
  def waitForAWhile () = {     
    Thread.sleep (100L)
  }
}