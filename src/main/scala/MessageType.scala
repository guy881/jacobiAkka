/**
  * Created by stevens on 08.06.17.
  */
object MessageType {
  case class Init(workerNumber: Int, row: Int, col: Int)

  case class InitMaster(aFile: String, bFile: String)

  case class InitialData(bMatrix: Matrix, DMatrix: Matrix, RMatrix: Matrix, xVec: Vec, start: Int, end: Int)

  case class Calculate(xk: Vec)

  case class X(xk: Map[Int, Double]) // index of element in X vector -> value

  case class returnX(xk: Array[(Int, Double)])

  case object IsConverged

  case object RetrieveBlocks

  case class ReturnedBlock(D: Matrix, R: Matrix)

  case object HasDone

}
