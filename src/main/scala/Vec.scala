@SerialVersionUID(123L)
class Vec(val size: Int) extends Serializable{
  val vec = new Array[Double](size)
  lazy val norm = math.sqrt(vec.map(x => x*x).sum)

  def dot(that: Vec): Double = {
    if (that.size != this.size){
      println("two vector size do not match!")
      0d
    } else {
      (0 until size map (x => this.vec(x) * that.vec(x))).sum
    }
  }

  def GetAt(i: Int): Double = vec(i)

  def PutAt(i: Int, value: Double): Unit = {
    if (i >= size)
      println("out of index bounds")
    vec(i) = value
  }
  def /(factor: Double): Vec = {
    Vec.fromArray(vec.map(v => v / factor))
  }
  override def toString() = {
    val str = "[ " + vec.mkString(" ") + " ]"
    str
  }
}

object Vec {
  def emptyVec() = new Vec(0)
  def apply(size: Int) = new Vec(size)
  def fromArray(arr: Array[Double]) = {
    val vec = new Vec(arr.size)
    0 until vec.size map (x => vec.PutAt(x, arr(x)))
    vec
  }
  def fromList(list: List[Double]) = {
    val vec = new Vec(list.size)
    0 until vec.size map (x => vec.PutAt(x, list(x)))
    vec
  }
}