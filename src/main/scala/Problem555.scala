import org.apache.logging.log4j._

/**
  * https://projecteuler.net/problem=555
  * Created by fred on 11/11/16.
  */
object Problem555 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val m = 100
    val k = 11
    val s = 10
    val M_91 = new M_mks(m, k, s)
    for(n <- s to k + (k - s))
      logger.info("M_91(" + n + ") = " + M_91(n) + " or " + M_91.nopreproc_apply(n) + " or " + M_91.recursive_apply(n))

    val F_mks = (1 to m).filter(n => n == M_91(n))
    logger.info("F_mks = " + F_mks)
    logger.info("SF_mks = " + F_mks.sum)

    val pm = 1e3.toInt
    val S_10_10 = new S_pm(pm, pm)
    logger.info("S_10_10() = " + S_10_10())
  }
}

class M_mks(m: Int, k: Int, s: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  // this is the max value that this M_mks function will ever compute
  val m2sk = m - 2*s + k
  //logger.info("M_mks(m: " + m + ", k: " + k + ", s: " + s + ") -- m - 2s + k = " + m2sk)

  // preprocess range [s,k) each of which gets mapped to a unique value
  /*val preproc: IndexedSeq[Int] = (s until k).map(nopreproc_apply(_))
  if (preproc.max != m2sk)
    logger.warn("preproc.max != (m - 2*s + k) : " + preproc.max + " != " + m2sk)*/
  val preproc_0 = nopreproc_apply(s)

  def recursive_apply(n: Int): Int = n match {
    case n if n > m => n - s
    case n => recursive_apply(recursive_apply(n + k))
  }

  def nopreproc_apply(n: Int): Int = {
    // the number of k-s's to add to n to get within k of m
    val ks = k - s
    val num_ks = (m - n) / ks // integer division
    val apply_n_plus_ks = num_ks * ks + n
    if (apply_n_plus_ks > m)
      logger.error("apply_n_plus_ks > m : " + apply_n_plus_ks + " > " + m)
    recursive_apply(apply_n_plus_ks)
  }

  def apply(n: Int): Int = {
    // the extra "+ k - s" is to ensure that the left operand is positive
    val i_ = (n - s) % (k - s)
    val i = if (i_ < 0) i_ + k - s else i_
    //preproc(i)
    val preproc_i = preproc_0 + i
    if (preproc_i <= m2sk)
      preproc_i
    else
      preproc_i - (k - s)
  }
}

class S_pm(p: Int, m: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def apply(): Int = {
    var r: Int = 0
    for(s <- 1 to p) {
      if (s % 457 == 0)
        logger.info("s=" + s)
      for(k <- s+1 to p) {
        //logger.info("k=" + k)
        try {
          val M = new M_mks(m, k, s)

          // F_{m,k,s} is the set of fixed points of M_{m,k,s}
          // no reason to iterate over the entire (1 to m) when we're only looking
          // for values equal to one of the preprocessed values
          val mn = M.m2sk - (k - s)
          val mx = M.m2sk
          val SF_mks = (mn to mx).filter(n => n == M(n)).sum
          //val SF_mks = (1 to m).filter(n => n == M(n)).sum

          /*val v2 = (1 to m).filter(n => n == M.recursive_apply(n)).sum
          if (SF_mks != v2)
            logger.error(SF_mks + " != " + v2 + ", s=" + s + ", k=" + k)*/
          r += SF_mks
        } catch {
          case e: Throwable => logger.error("s=" + s + ", k=" + k)
        }
      }
    }
    r
  }
}
