/**
  * Created by stevens on 07.06.17.
  */

import MessageType.{Calculate, Init, InitialData, xMap}
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
    case Init(workerNumber, row, col) => {
      println("działa!")
    }
    case InitialData(bMatrix, dMatrix, rMatrix, xVec) => {
      master = sender
      b = bMatrix
      D = dMatrix
      R = rMatrix
      x0 = xVec

    }

    case Calculate(xk) => {
      println(R)
      val Rx = R.dot(xk) // Rx(k)
      val bMinusRx = b.subtract(Rx) // b - Ax(k)
      val new_x = D.dot(bMinusRx) // D^-1(b - Ax(k))
      new_x.PrintMatrix()
      master ! xMap(x0)
    }

  }
}
