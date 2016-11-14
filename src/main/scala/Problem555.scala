import org.apache.logging.log4j._

/**
  * https://projecteuler.net/problem=555
  * Created by fred on 11/11/16.
  */
object Problem555 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val m = 100
    val M_91 = new M_mks(m, 11, 10)
    for(n <- 1 to m) {
      logger.info("M_91(" + n + ") = " + M_91(n))
    }

    val F_mks = (1 to m).filter(n => n == M_91(n))
    logger.info("F_mks = " + F_mks)
    logger.info("SF_mks = " + F_mks.sum)

    val S_10_10 = new S_pm(10e6.asInstanceOf[Int], 10e6.asInstanceOf[Int])
    logger.info("S_10_10() = " + S_10_10())
  }
}

class M_mks(m: Int, k: Int, s: Int) {
  def apply(n: Int): Int = n match {
    case n if n > m => n - s
    case n => this(this(n + k))
  }
}

class S_pm(p: Int, m: Int) {
  def apply(): Int = {
    var r: Int = 0
    for(s <- 1 to p) {
      for(k <- s+1 to p) {
        val M = new M_mks(m, k, s)
        val SF_mks = (1 to m).filter(n => n == M(n)).sum
        r += SF_mks
      }
    }
    r
  }
}
