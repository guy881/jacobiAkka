/**
  * Created by stevens on 07.06.17.
  */

import MessageType.{Calculate, Init, InitialData, returnX}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object SlaveActor {
  def props(id: Int): Props = Props(new SlaveActor(id: Int))

  final case class WhoToGreet(who: String)

  case object Greet

}

class SlaveActor(id: Int) extends Actor {
  var converged = false
  var master: ActorRef = _
  var x0: Vec = Vec.emptyVec()
  var b: Matrix = Matrix.emptyMatrix()
  var D: Matrix = Matrix.emptyMatrix()
  var R: Matrix = Matrix.emptyMatrix()

  override def receive: Receive = {
    case Init(workerNumber, row, col) =>
      println("działa!")

    case InitialData(bMatrix, dMatrix, rMatrix, xVec) =>
      master = sender
      b = bMatrix
      D = dMatrix
      R = rMatrix
      x0 = xVec

    case Calculate(xk, start, end) =>
      val results = new Array[(Int, Double)](end - start)
      var len = 0 // elements in results
      for (i <- start until end) {
        var rxSum = 0.0
        for(j <- 0 until R.columns){
          if(i != j )
            rxSum += R.GetAt(i, j) * xk.GetAt(j)
        }
        val bMinusRx = b.GetAt(i, 0) - rxSum // b - Ax(k)
        val new_x = D.GetAt(i, i) * bMinusRx // D^-1(b - Ax(k))
        results(len) = (i, new_x)
        len += 1

      }
      master ! returnX(results)

  }
}
