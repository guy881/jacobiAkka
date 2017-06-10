//object Jacobi {
//  // x(k+1) = x(k) + D^-1(b-Ax(k))
//  // delta_x = D^-1(b-Ax(k))
//  def main(args: Array[String]) {
//
//    val A = Matrix.LoadMatrixFromFile("A_jacobi.txt")
//    val b = Matrix.LoadMatrixFromFile("B_jacobi.txt")
//
//    val D = new Matrix(A.rows, A.columns)
//    val R = new Matrix(A.rows, A.columns)
//
//    for (i <- 0 until A.rows) {
//      for (j <- 0 until A.columns) {
//        if (i == j)
//          D.PutAt(i, j, A.GetAt(i, j))
//        else
//          R.PutAt(i, j, A.GetAt(i, j))
//      }
//    }
//
//    //D^-1
//    var DInverse = new Matrix(A.rows, A.columns)
//    for (i <- 0 until A.rows) {
//      val diagonalElem = D.GetAt(i, i)
//      if (diagonalElem == 0){
//        println("Zero na diagonalii, nie mogę obliczyć D^-1")
//        return
//      }
//      DInverse.PutAt(i, i, 1 / diagonalElem)
//    }
//
//    println()
//    D.PrintMatrix()
//    println()
//    b.PrintMatrix()
//    println()
//
//    var x = new Matrix(A.rows, 1)
//    var new_x = new Matrix(A.rows, 1)
//    val ITER_LIMIT = 25
//
////    for (k <- 0 to ITER_LIMIT) {
//    for (k <- 0 to ITER_LIMIT) {
//      var dx = multiplyMatrices(A, x) // Ax(k)
//      dx = subtractMatrices(b, dx) // b - Ax(k)
//      dx = multiplyMatrices(DInverse, dx) // D^-1(b - Ax(k))
//      new_x = addMatrices(x, dx)
//      new_x.PrintMatrix()
//      x = new_x
//    }
//    println("Done")
//    x.PrintMatrix()
//
//  }
//
//}