import java.io._

import org.apache.logging.log4j._

import scala.collection.immutable.Range
import scala.collection.parallel._
import scala.collection.parallel.immutable.ParSeq

/**
  * https://projecteuler.net/problem=557
  * Created by fred on 11/21/16.
  */
object Problem557 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val min_n: BigInt = 4
    val max_n: BigInt = 10000

    //val n = (4 to max_n)
    //val w = (1 to 2*n)
    //val nw = n.cross(w)

    val n_range = Range.BigInt(min_n, max_n+1, 1).par
    n_range.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(14))

    //n_range.foreach { n => logger.info(s"n=$n") }

    val Ss: ParSeq[BigInt] = n_range.map { (n: BigInt) => {
      //if (n % 100 == 1)
        logger.info(s"n=$n")

      if (n > max_n) {
        logger.error(s"Why is $n > $max_n?")
        System.exit(1)
      }

      var num_n = 0
      for(x <- 2 to n.toInt - 2; // a+b
          y <- x to n.toInt - 2; // a+c
          a <- 1 to x - 1) {
        //val h = 2*n.toDouble / w
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
        //    a rate proportional to the ratio n:a+b (= 1 + d+c:a+b)

        val r1 = (d.toDouble + b) / (c + a)
        val r2 = (b.toDouble * n.toInt) / (a + b) / a

        if ((r1 - r2).abs < 1e-10) {
          num_n += 1
          //logger.info(f"n=$n, ($a, $b, $c, $d), r2=$r1, r2=$r2")
          //if (n % 100 == 1)
            //logger.info(s"n=$n, foldcum=$foldcum, num_n=$num_n")
        }
      }

      n * num_n
    }}

    val S: BigInt = Ss.sum
    logger.info(f"S = $S")
  }

  /**
    * http://stackoverflow.com/questions/14740199/cross-product-in-scala
    */
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }
}

