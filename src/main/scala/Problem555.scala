import org.apache.logging.log4j._

/**
  * https://projecteuler.net/problem=555
  * Created by fred on 11/11/16.
  */
object Problem555 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val m = 100
    val k = 9
    val s = 2
    val M_91 = new M_mks(m, k, s)
    logger.info("M_mks(m: " + m + ", k: " + k + ", s: " + s + ") -- m-2s+k = " + M_91.max_val)
    for(n <- s to k)
    //for(n <- s - (k - s) to k + (k - s))
    //for(n <- 1 to m)
      logger.info("M_91(" + n + ") = " + M_91(n) + " or " + M_91.nopreproc_apply(n) + " or " + M_91.recursive_apply(n))

    val F_mks = (1 to m).filter(n => n == M_91(n))
    logger.info("F_mks = " + F_mks)
    logger.info("SF_mks = " + F_mks.sum)

    val pm = 1e3.toInt
    val S_10_10 = new S_pm(pm, pm)
    logger.info("S_10_10() = " + S_10_10())
  }
}

/**
  * Functor class to compute M_{m,k,s} function.
  */
class M_mks(m: Int, k: Int, s: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  // this is the max value that this M_mks function will ever compute
  val max_val = m - 2*s + k
  val min_val = max_val - (k - s) + 1
  //logger.info("M_mks(m: " + m + ", k: " + k + ", s: " + s + ") -- m - 2s + k = " + max_val)

  // preprocess range [s,k) each of which gets mapped to a unique value
  /*val preproc: IndexedSeq[Int] = (s until k).map(nopreproc_apply(_))
  if (preproc.max != max_val)
    logger.warn("preproc.max != (m - 2*s + k) : " + preproc.max + " != " + max_val)*/
  val preproc_0 = nopreproc_apply(s)

  val argmin = max_val - preproc_0 + s + 1
  /*if (this(argmin) != min_val)
    logger.error("this(argmin) != min_val : " + this(argmin) + " != " + min_val)*/

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
    val i_ = (n - argmin) % (k - s)
    val i = if (i_ < 0) i_ + k - s else i_
    //preproc(i)
    /*val preproc_i = preproc_0 + i
    if (preproc_i <= max_val)
      preproc_i
    else
      preproc_i - (k - s)*/
    min_val + i
  }
}

/**
  * Functor class to compute S_{p,m} function.
  */
class S_pm(p: Int, m: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def apply(): BigInt = {
    var cumsum: BigInt = 0
    for(s <- 1 to p) {
      if (s % 45/*7*/ == 0)
        logger.info("s=" + s + ", cumsum=" + cumsum)
      for(k <- s+1 to p) {
        //logger.info("k=" + k)
        try {
          val M = new M_mks(m, k, s)

          // F_{m,k,s} is the set of fixed points of M_{m,k,s}
          // no reason to iterate over the entire (1 to m) when we're only looking
          // for values equal to one of the preprocessed values
          val mn = M.max_val - (k - s)
          val mx = scala.math.min(M.max_val, m)
          val SF_mks = (mn to mx).filter(n => n == M(n)).sum
          //val SF_mks = (1 to m).filter(n => n == M(n)).sum

          /*val v2 = (1 to m).filter(n => n == M.recursive_apply(n)).sum
          if (SF_mks != v2)
            logger.error(SF_mks + " != " + v2 + ", s=" + s + ", k=" + k)*/
          cumsum += SF_mks
        } catch {
          case e: Throwable => logger.error("s=" + s + ", k=" + k)
        }
      }
    }
    cumsum
  }
}
