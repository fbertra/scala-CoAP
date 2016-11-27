package iot

trait Task {
  def runTask(scheduler: Scheduler): Unit
}

trait Scheduler extends java.lang.Runnable {
  def addTask (task: Task)
  
  def removeTask (task: Task)
  
  def shutdown ()
}

