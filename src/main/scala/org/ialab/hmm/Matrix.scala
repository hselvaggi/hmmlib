package org.ialab.hmm

final case class FloatMatrix(array: Array[Double], cols: Int) {
  import FloatMatrix._

  def this(rows: Int, cols: Int) = this(new Array[Double](cols * rows), cols)
  def this(rows: Int, cols: Int, value: Double) = {
    this(rows, cols)
    for(i <- 0 until rows * cols) array(i) = value
  }
  def this(rows: Int, cols: Int, initialState: FloatMatrix.SpecializedFunction3[Int, Int, Double, Double]) = {
    this(new Array[Double](cols * rows), cols)
    foreachUpdate(initialState)
  }

  val rows = array.length / cols

  @inline def apply(y: Int, x: Int): Double = try array(x + y * cols)
  catch { case e: ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException(s"$y/$rows, $x/$cols")}
  @inline def update(y: Int, x: Int, v: Double) = try array(x + y * cols) = v
  catch { case e: ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException(s"$y/$rows, $x/$cols")}

  @inline def map(f: SpecializedFunction3[Int, Int, Double, Double]): FloatMatrix = {
    val res = new Array[Double](array.length)
    foreach((y, x, v) => res(x + y * cols) = f(y, x, v))
    FloatMatrix(res, cols)
  }

  @inline def foldLeft[@specialized(Int, Double) A](a: A)(f: SpecializedFunction4[A, Int, Int, Double, A]): A = {
    var res = a
    foreach((y, x, v) => res = f(res, y, x, v))
    res
  }

  @inline def foreach[@specialized(Unit) U](f: SpecializedFunction3[Int, Int, Double, U]): Unit = foreach()(f)
  @inline def foreach[@specialized(Unit) U](row0: Int = 0, col0: Int = 0, rowN: Int = rows, colN: Int = cols)(f: SpecializedFunction3[Int, Int, Double, U]): Unit = {
    for (y <- row0 until rowN; x <- col0 until colN) f(y, x, apply(y, x))
  }
  @inline def foreachUpdate(row0: Int = 0, col0: Int = 0, rowN: Int = rows, colN: Int = cols)(f: SpecializedFunction3[Int, Int, Double, Double]): Unit = {
    foreach(row0, col0, rowN, colN)((y, x, v) => update(y, x, f(y, x, v)))
  }
  @inline def foreachUpdate(f: SpecializedFunction3[Int, Int, Double, Double]): Unit = foreachUpdate()(f)

  def row(y: Int): FloatIterator = {
    new FloatIterator {
      var x = -1
      def hasNext = (x + 1) < cols
      def next = {
        x += 1
        apply(y, x)
      }
    }
  }
  /*
   * An iterable of the rows of the matrix.
   */
  def rowsIterable: Iterable[FloatIterator] = {
    new Iterable[FloatIterator] {
      def iterator = new Iterator[FloatIterator] {
        var y = -1
        def hasNext = (y + 1) < rows
        def next = {
          y += 1
          row(y)
        }
      }
    }
  }

  def col(x: Int): FloatIterator = {
    new FloatIterator {
      var y = -1
      def hasNext = (y + 1) < rows
      def next = {
        y += 1
        apply(y, x)
      }
    }
  }
  /**
   * An iterable of the columns of the matrix.
   */
  def colsIterable: Iterable[FloatIterator] = {
    new Iterable[FloatIterator] {
      def iterator = new Iterator[FloatIterator] {
        var x = -1
        def hasNext = (x + 1) < cols
        def next = {
          x += 1
          col(x)
        }
      }
    }
  }

  /**
   * add all the floats in the matrix
   */
  @inline def sum = foldLeft(0.0)((acc, _, _, v) => acc + v)

  override def toString =
    rowsIterable.map(_.foldLeft(new StringBuilder)((sb, n) => sb.append(n).append(", ")).toString.dropRight(2)).map("| " + _  +" |").mkString("\n")
}
object FloatMatrix {

  def ConstantMatrix(rows: Int, cols: Int, value: Double) = new FloatMatrix(rows, cols, value)

  /* specialized function3 to avoid boxing*/
  trait SpecializedFunction3[@specialized(Int, Double) T1, @specialized(Int, Double) T2, @specialized(Int, Double) T3, @specialized(Int, Double, Unit) R] {
    def apply(t1: T1, t2: T2, t3: T3): R
  }
  /* specialized function4 to avoid boxing*/
  trait SpecializedFunction4[@specialized(Int, Double) T1, @specialized(Int, Double) T2, @specialized(Int, Double) T3, @specialized(Int, Double) T4, @specialized(Int, Double, Unit) R] {
    def apply(t1: T1, t2: T2, t3: T3, t4: T4): R
  }
  trait FloatIterator {
    def hasNext: Boolean
    def next: Double
    def nextOption = if (hasNext) Some(next) else None

    def foldLeft[@specialized(Int, Double) S](s: S)(f: (S, Double) => S): S = {
      var res = s
      while (hasNext) res = f(res, next)
      res
    }
    def sum = foldLeft(0.0)(_ + _)
    def max = foldLeft(Double.MinValue)(math.max)
    def min = foldLeft(Double.MaxValue)(math.min)
    def indexOf(v: Double): Int = {
      var i = -1
      while ({i += 1; hasNext}) {
        val n = next
        if (n == v) return i
      }
      -1
    }
  }
}