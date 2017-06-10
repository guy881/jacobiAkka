/**
  * Created by stevens on 07.06.17.
  */

import MessageType.{Calculate, Init, InitMaster, InitialData}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object MasterActor {
  def props(threadsNum: Int, iterLimit: Int): Props = {
    Props(new MasterActor(threadsNum: Int, iterLimit: Int))
  }
}

class MasterActor(threadsNum: Int, iterLimit: Int) extends Actor {
  var workNumber = 0
  val threadsNumber: Int = threadsNum
  var iteration = 0
  var allConverged = false
  var actorList: List[ActorRef] = Nil
  var actorPathList: List[String] = Nil
  var mainThread: ActorRef = _
  var lastProcessNumber = 0
  var matrixU: Matrix = Matrix.emptyMatrix()
  val MAX_ITERATIONS: Int = iterLimit

  override def receive: Receive = {
    case InitMaster(aFile, bFile) =>
      // read input matrices
      val A = Matrix.LoadMatrixFromFile(aFile)
      val b = Matrix.LoadMatrixFromFile(bFile)

      val D = new Matrix(A.rows, A.columns) // D^-1
      val R = new Matrix(A.rows, A.columns)

      for (i <- 0 until A.rows) {
        for (j <- 0 until A.columns) {
          val elem = A.GetAt(i, j)
          if (i == j) {
            if (elem == 0) {
              throw new Exception("Zero na diagonalii, nie mogę obliczyć D^-1")
            }
            D.PutAt(i, j, 1 / elem) // D^-1
          }
          else
            R.PutAt(i, j, elem)
        }
      }

      println("A: ")
      println(A)
      println("b: ")
      println(b)
      println("D ^-1: ")
      println(D)
      println("R: ")
      println(R)

      // create Actors
      val slave1 = context.actorOf(SlaveActor.props(1))
      // TODO: create more actors according to threads pool

      // divide work between actors

      var x0 = new Vec(A.rows)
      val ITER_LIMIT = 1

      slave1 ! InitialData(b, D, R, x0)

      for (k <- 0 to ITER_LIMIT) {
        slave1 ! Calculate(x0)
      }
      println("Done")
    //    x.PrintMatrix()


    // send work to actors
    // send x(k) between actors
    // if converged -> return result

    case X(x) =>

  }
}

object Jacobi extends App {
  val threadsCount = 4 // TODO: get threads number
  val MAX_ITERATIONS = 75

  override def main(args: Array[String]) {
    val system: ActorSystem = ActorSystem("helloAkka")
    val master: ActorRef = system.actorOf(MasterActor.props(threadsCount, MAX_ITERATIONS), "printerActor")

    master ! InitMaster("A_jacobi.txt", "B_jacobi.txt")
  }
}
