package iot

trait Task {
  def run(schedueler: Scheduler): Unit
}

trait Scheduler {
  def addTask (task: Task)
  
  def removeTask (task: Task)
  
  def shutdown ()
}

