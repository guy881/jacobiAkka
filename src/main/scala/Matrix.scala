import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.io.Source
class Matrix(val rows : Int,val columns : Int) {
  val data = InitializeMatrix(rows, columns)

  def InitializeMatrix(rows: Int, columns: Int): Array[Double] = new Array[Double](rows * columns)

  // matrix.GetAt(0,0) returns first element (a11 in matrix terminology)
  def GetAt(row: Int, column: Int): Double ={
//    if( row >= this.rows || column >= this.columns)
//      throw new Exception("Row or column exceeds matrix size!")
    data(row * this.columns + column)
  }
  def PutAt(row: Int, column: Int, value : Double) = {
//    if( row >= this.rows || column >= this.columns)
//      throw new Exception("Row or column exceeds matrix size!")
    data(row * this.columns + column) = value
  }
  val size = (rows, columns)

  def subtract(matrix2: Matrix): Matrix = {
    if (this.columns != matrix2.columns || this.rows != matrix2.rows) { // cannot multiply
      throw new Exception("Cannot subtract those matrices")
    }

    val res = new Matrix(this.rows, this.columns)
    for (i <- 0 until this.rows) {
      for (j <- 0 until matrix2.columns) {
        val difference = this.GetAt(i, j) - matrix2.GetAt(i, j)
        res.PutAt(i, j, difference)
      }
    }
    res // return
  }

  def add(matrix2: Matrix): Matrix = {
    if (this.columns != matrix2.columns || this.rows != matrix2.rows) { // cannot multiply
      throw new Exception("Cannot add those matrices")
    }

    val res = new Matrix(this.rows, this.columns)
    for (i <- 0 until this.rows) {
      for (j <- 0 until matrix2.columns) {
        val sum = this.GetAt(i, j) + matrix2.GetAt(i, j)
        res.PutAt(i, j, sum)
      }
    }
    res // return
  }

  override def toString: String = {
    val (r, c) = this.size
    var str = "[\n"
    for (i <- 0 until r) {
      for (j <- 0 until c) {
        str += " " + this.GetAt(i, j)
      }
      str += "\n"
    }
    str += "]"
    str
  }

  def PrintMatrix(): Unit = {
    val (rows, columns) = this.size
    for (i <- 0 until rows) {
      for (j <- 0 until columns) {
        printf("%2f  ", this.GetAt(i, j))
      }
      println()
    }
  }
}

object Matrix {
  def apply(row: Int, col: Int) = new Matrix(row, col)

  def random(row: Int, col: Int): Matrix = {
    val m = Matrix(row, col)
    for {i <- 0 until row
         j <- 0 until col}
      m.PutAt(i, j, util.Random.nextDouble())
    m
  }

  def from2DArray(arr: Array[Array[Double]]): Matrix = {
    val col = arr.size
    val row = arr(0).size
    val m = Matrix(row, col)
    for {i <- 0 until row
         j <- 0 until col}
      m.PutAt(i, j, arr(j)(i))
    m
  }

  def emptyMatrix() = new Matrix(0, 0)

  def LoadMatrixFromFile(path: String): Matrix = {
    val lines: Array[String] = Source.fromFile(path).getLines().toArray

    val mat = new Matrix(lines(0).toInt, lines(1).toInt)
    if (lines.length != (mat.rows * mat.columns + 2))
      return null;
    var i = 0

    for (line <- lines.drop(2)) {
      //println( i + "   "+i /(mat.columns) + "     " +  i % mat.columns)
      mat.PutAt(i / mat.columns, i % mat.columns, line.toDouble)
      i += 1
    }
    mat
  }
}