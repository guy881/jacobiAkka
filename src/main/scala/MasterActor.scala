import MessageType._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object MasterActor {
  def props(threadsNum: Int, iterLimit: Int): Props = {
    Props(new MasterActor(threadsNum, iterLimit))
  }
}

class MasterActor(threadsNum: Int, iterLimit: Int) extends Actor {
  val DEBUG = false
  var t1: Long = System.nanoTime  // execution time measure
  val threadsNumber: Int = threadsNum
  var iteration = 0
  var doneInIteration = 0
  var xk: Vec = Vec.emptyVec()
  var allConverged = false
  var slavesArray = new Array[ActorRef](threadsNumber) // or threads num - 1??
  val MAX_ITERATIONS: Int = iterLimit
  var workDivision = new Array[(Int, Int, Int)](threadsNumber) // slave index, start, end

  override def receive: Receive = {
    case InitMaster(aFile, bFile) =>
      // read input matrices
      val A = Matrix.LoadMatrixFromFile(aFile)
      val b = Matrix.LoadMatrixFromFile(bFile)
      val D = new Matrix(A.rows, A.columns) // D^-1
      val R = new Matrix(A.rows, A.columns)
      t1 = System.nanoTime // measure time

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

      xk = new Vec(A.rows)
      val rowsPerThread = xk.size / threadsNumber
      var modulo = xk.size % threadsNumber
      var end = 0
      for (i <- 0 until threadsNumber) {
        val start = end
        end += rowsPerThread
        if (modulo > 0) {
          end += 1
          modulo -= 1
        }
        // create and init slave
        val slave = context.actorOf(SlaveActor.props(i + 1))
        slave ! InitialData(b, D, R, xk, start, end)
        slavesArray(i) = slave
        // println((i + 1) + ": " + "start: " + start + " end: " + end)

      }

      // divide work between actors

//      println(threadsNumber + " threads will be working in range:")
//      println("Max iterations: " + MAX_ITERATIONS)

      // send work to actors
      for (i <- slavesArray.indices) {
        slavesArray(i) ! Calculate(xk)
      }

    /////////////////////////////////////////////////////////////////////////////////////////////
    case returnX(xkPlus1) => // get x(k) from slave
      doneInIteration += 1
      for (i <- xkPlus1.indices) {
        xk.PutAt(xkPlus1(i)._1, xkPlus1(i)._2)
      }
      if (DEBUG)
        println(iteration + " iter: xk: " + xk)

      if (doneInIteration == threadsNumber) { // iteration finished
        iteration += 1
        if (DEBUG)
          println("\n Iteration finished")
        if (iteration == MAX_ITERATIONS) {
          val duration = (System.nanoTime - t1) / 1e9d
          println("Done in: " + duration + " s")
          println(xk)

        } else {
          for (i <- slavesArray.indices) {
            slavesArray(i) ! Calculate(xk)
          }
        }
        doneInIteration = 0
      }
  }
}

object Jacobi extends App {

  override def main(args: Array[String]) {
    val threadsCount = 4 // TODO: get threads number
    val MAX_ITERATIONS = 100
    val system: ActorSystem = ActorSystem("helloAkka")
    val master: ActorRef = system.actorOf(MasterActor.props(threadsCount, MAX_ITERATIONS), "masterActor")

    master ! InitMaster("A_jacobi.txt", "B_jacobi.txt")
    //    system.terminate()

  }
}
