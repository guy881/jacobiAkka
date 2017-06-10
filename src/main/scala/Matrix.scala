import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.io.Source


//@SerialVersionUID(100L)
class Matrix(val rows: Int, val columns: Int) extends Serializable {
  val matrix = Array.ofDim[Double](columns, rows)
  val size = (rows, columns)

  def isEmpty = rows == 0 && columns == 0

  def GetAt(i: Int, j: Int) = matrix(j)(i)

  def PutAt(i: Int, j: Int, value: Double): Unit = {
    matrix(j)(i) = value
  }

  def PutAt(colIndex: Int, arr: Vec) =
    0 until arr.size map (x => matrix(colIndex)(x) = arr.GetAt(x))

  def getCol(colIndex: Int): Vec = Vec.fromArray(this.matrix(colIndex))

  def getRow(rowIndex: Int): Vec = {
    var list: List[Double] = Nil
    for (i <- 0 until columns)
      list = this.matrix(i)(rowIndex) :: list
    Vec.fromList(list.reverse)
  }

  def addByCol(other: Matrix): Matrix = {
    val newMatrix = Matrix(rows, columns + other.columns)
    for (i <- 0 until rows; j <- 0 until newMatrix.columns) {
      if (j < columns)
        newMatrix.PutAt(i, j, GetAt(i, j))
      else
        newMatrix.PutAt(i, j, other.GetAt(i, j - columns))
    }
    newMatrix
  }

  def sliceByCol(from: Int, to: Int): Matrix = {
    val max_col = math.max(from, to)
    val min_col = math.min(from, to)
    val slice_col = max_col - min_col + 1
    val sliceMatrix = new Matrix(rows, slice_col)
    for {i <- 0 until rows
         j <- 0 until slice_col} {
      sliceMatrix.PutAt(i, j, this.GetAt(i, j + min_col))
    }
    sliceMatrix
  }

  def sliceByColEqually(pieces: Int): List[Matrix] = {
    val colPerPiece = columns / pieces
    var mod = columns % pieces
    var res: List[Matrix] = Nil
    var index = 0
    for (i <- 0 until pieces) {
      val tempCol = if (mod > 0) colPerPiece + 1 else colPerPiece
      res = sliceByCol(index, index + tempCol - 1) :: res
      index += tempCol
      mod -= 1
    }
    res.reverse
  }

  def t: Matrix = {
    val transposedMatrix = new Matrix(columns, rows)
    for {i <- 0 until columns
         j <- 0 until rows} {
      transposedMatrix.PutAt(i, j, this.GetAt(j, i))
    }
    transposedMatrix
  }

  def dot(that: Matrix) = {
    val (r, c) = that.size
    if (this.columns != r) {
      throw new Exception("matrix dot operation dimension is not matched")
    } else {
      val dotMatrix = new Matrix(this.rows, c)
      for (i <- 0 until dotMatrix.rows;
           j <- 0 until dotMatrix.columns) {
        val rowVec = getRow(i)
        val colVec = that.getCol(j)
        dotMatrix.PutAt(i, j, rowVec dot colVec)
      }
      dotMatrix
    }
  }

  def dot(that: Vec) = { // matrix dot vector
    if (this.columns != that.size) {
      printf("cols: %d rows: %d size: %d\n", this.columns, this.rows, that.size)
      throw new Exception("matrix dot operation dimension is not matched")
    } else {
      val dotMatrix = new Matrix(this.rows, 1) // 1 because Vec has 1 column
      for (i <- 0 until dotMatrix.rows;
           j <- 0 until dotMatrix.columns) {
        val rowVec = getRow(i)
        dotMatrix.PutAt(i, j, rowVec dot that)
      }
      dotMatrix
    }
  }

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

  def normalizeU = {
    val vecArray = (0 until columns map (c => {
      val vec = getCol(c)
      (vec / vec.norm, vec.norm)
    })).sortWith((x, y) => x._2 > y._2)
    val matrixU = Matrix(rows, columns)
    for (i <- 0 until columns; j <- 0 until rows) {
      matrixU.PutAt(j, i, vecArray(i)._1.GetAt(j))
    }
    matrixU
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

object SerializationDemo extends App { // (1) create a Stock instance
  val matrix = Matrix.random(4, 4)
  println(matrix)
  // (2) write the instance out to a file
  val oos = new ObjectOutputStream(new FileOutputStream("/tmp/nflx"))
  oos.writeObject(matrix)
  oos.close
  // (3) read the object back in
  val ois = new ObjectInputStream(new FileInputStream("/tmp/nflx"))
  val stock = ois.readObject.asInstanceOf[Matrix]
  ois.close
  // (4) print the object that was read back in
  println(matrix)
}