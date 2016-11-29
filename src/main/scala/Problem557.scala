import org.apache.logging.log4j._
import org.apache.spark.sql._
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.expressions.Aggregator

/**
  * https://projecteuler.net/problem=557
  * Created by fred on 11/21/16.
  */
object Problem557 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val num_drivers = 24
    val spark = SparkSession.builder
      .master(s"local[$num_drivers]") // using `nxy`: 1e3 -> 8:32/8threads, 5:46/16, 5:02/24, 4:31/32, 4:55/40
      .appName(getClass.getSimpleName) // new! using `nxa`: 1e3 -> 30s / 24 threads
      .getOrCreate
    import spark.implicits._

    val min_n = 4
    val max_n = 1e4.toInt // 10->12, 20->259, 1e2->20765, 1e3->8736041, 1e4->5015484425?

    // initial uncut triangle search range for n = a + b + c + d
    val ni = (min_n to max_n by num_drivers)
    val ns = (0 until num_drivers).flatMap(offset => ni.map(n => n + offset))
    val n_range: Dataset[Int] = spark.createDataset(ns.filter(_ <= max_n))

    // join n to x, y, and a
    val nx = n_range.flatMap(n => (2 to n-2).map((n, _))) // x = a + b
//star
    val nxa = nx.flatMap { case (n, x) => (1 to x-1).map((n, x, _)) }
    val valid_quadruple = nxa.map { case (n, x, a) => {

      val b = x - a

      // d = numer / denom * (c + a) - b
      val numer: BigInt = b * n // BigInt already has a gcd method defined
      val denom = a * x
      val g = (numer gcd denom).toInt

      // d will be an integer at c's periodicity of p (for c starting at -a, i.e. c=-a,
      // c=-a+p, c=-a+2p, ...)
      val p = denom / g

      // this is where c+a gets divided by p
      def d(c: Int): Int = numer.toInt * (c + a) / denom - b

      // number of n's at which c and d are integers (it's possible that the first
      // couple c's are non-positive)
      val num_n = (-a to n - x - 1 by p).count(c => (c >= b) && (d(c) + c == n - x))
      //if (num_n > 0) logger.info(s"n=$n numer=$numer denom=$denom g=$g p=$p a=$a b=$b cs=" + (-a to n - x - 1 by p).filter(c => (c >= b) && (d(c) + c == n - x)))
      if (num_n > 0) logger.info(s"n=$n numer=$numer denom=$denom g=$g p=$p a=$a b=$b num_n=$num_n")
      /*val cs = (-a to n - x - 1 by p).filter(c => (c >= b) && (d(c) + c == n - x))
      cs.foreach { c => logger.info(s"n=$n numer=$numer denom=$denom g=$g p=$p  a=$a b=$b c=$c d=${d(c)}") }*/
      num_n * n
/*
    val nxy = nx.flatMap { case (n, x) => (x to n-2).map((n, x, _)) } // y = a + c
    //val nxya = nxy.flatMap { case (n, x, y) => (1 to x-1).map((n, x, y, _)) }

    val valid_quadruple = nxy.map { case (n, x, y) => {
      // sum over all the possible valus of `a` given n, x, and y (= a + c)
      // 10->2 of these, 20->17, 1e2->320, 1e3->14270
      val sum_a = (1 to x-1).fold(0) { (s, a) => {

        val b = x - a
        val c = y - a
        val d = n - a - b - c

        // 1) start by dividing the triangle into 2 parts, d+b and a+c
        // 2) the ratio of these two sums is the same as the ratio of
        //    how the edge shared by d and c is divided
        // 3) e.g. if a=22, b=8, c=11, d=14 then this ratio is 22:33
        // 4) next consider the ratio of b:a which would be the same
        //    as d+b:a+c if c and d were both of size 0 (i.e. if the
        //    edge shared by c and d were colinear with the dividing line
        //    between dc and ba
        // 5) but they aren't colinear and as one line rotates away from
        //    the other (using ca's vertex as a pivot point) d grows at
        //    faster than c a rate equal to the ratio n:a+b

        val r1 = (d.toDouble + b) / (a + c)
        val r2 = b.toDouble / a * n.toInt / (a + b)

        if ((r1 - r2).abs < 1e-10) logger.info(f"n=$n, ($a, $b, $c, $d), r1=$r1, r2=$r2")
        if ((r1 - r2).abs < 1e-10) s + n else s
      }}

      if (sum_a != 0)
        logger.info(f"n=$n, x=$x, y=$y, s=$sum_a")
      sum_a
*/
    }}

    //valid_quadruple.repartition(32) // 1e3 -> 11:22/16threads

    // https://docs.cloud.databricks.com/docs/spark/1.6/examples/Dataset%20Aggregator.html
    val bigIntSum = new Aggregator[Int, Long, Long] with Serializable {
      def zero: Long = 0                     // The initial value.
      def reduce(b: Long, a: Int) = b + a    // Add an element to the running total
      def merge(b1: Long, b2: Long) = b1 + b2 // Merge intermediate values.
      def finish(b: Long) = b

      // "there currently is no real good support for custom class encoders"
      // http://stackoverflow.com/questions/36648128/how-to-store-custom-objects-in-a-dataset
      override def bufferEncoder: Encoder[Long] = Encoders.scalaLong
      override def outputEncoder: Encoder[Long] = Encoders.scalaLong
    }.toColumn

    val S: Long = valid_quadruple.select(bigIntSum).collect()(0)
    //val S = valid_quadruple.reduce((s: Int, n: Int) => s + n)
    logger.info(f"S = $S")

    spark.stop
  }

  /**
    * http://stackoverflow.com/questions/14740199/cross-product-in-scala
    */
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  /**
    * http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer
    */
  def isSquare(n: Long): Boolean = {
    if (n < 0) {
      false
    }
    else {
      val tst: Long = (Math.sqrt(n) /*+ 0.5*/).toLong
      tst * tst == n
    }
  }
}

