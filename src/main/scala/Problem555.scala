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
    logger.info("M_mks(m=" + m + ", k=" + k + ", s=" + s + ") -- min/m-2s+k=" + M_91.min_val + "/" + M_91.max_val + ", argmin=" + M_91.argmin)
    for(n <- s to k)
    //for(n <- s - (k - s) to k + (k - s))
    //for(n <- 1 to m)
      logger.info("M_91(" + n + ") = " + M_91(n) + "/" + M_91.nopreproc_apply(n) + "/" + M_91.recursive_apply(n))

    val F_mks = (1 to m).filter(n => n == M_91(n))
    logger.info("F_mks=" + F_mks)
    logger.info("SF_mks=" + F_mks.sum)

    val pm = 1e6.toInt
    val S_10_10 = new S_pm(pm, pm)
    val cumsum = S_10_10()
    logger.info("S_10_10()D=" + S_10_10.cumsumD)
    logger.info("S_10_10()=" + cumsum)
  }
}

/**
  * Functor class to compute M_{m,k,s} function.
  */
class M_mks(m: Int, k: Int, s: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  // max_val is the max value that this M_mks function will ever compute
  val ks = k - s
  val max_val = m - s + ks // = m - 2s + k
  val min_val = max_val - ks + 1

  // preprocess range [s,k) each of which gets mapped to a unique value
  /*val preproc: IndexedSeq[Int] = (s until k).map(nopreproc_apply(_))
  if (preproc.max != max_val)
    logger.warn("preproc.max != (m - 2*s + k) =" + preproc.max + " != " + max_val)*/
  val preproc_0 = nopreproc_apply(s)

  val argmin = max_val - preproc_0 + s + 1
  /*if (this(argmin) != min_val)
    logger.error("this(argmin) != min_val =" + this(argmin) + " != " + min_val)*/

  //logger.info("M_mks(m=" + m + ", k=" + k + ", s=" + s + ") -- min/m-2s+k=" + min_val + "/" + max_val + ", argmin=" + argmin)

  def recursive_apply(n: Int): Int = n match {
    case n if n > m => n - s
    case n => recursive_apply(recursive_apply(n + k))
  }

  def nopreproc_apply(n: Int): Int = {
    // the number of k-s's to add to n to get within k of m
    val num_ks = (m - n) / ks // integer division
    val apply_n_plus_ks = num_ks * ks + n
    if (apply_n_plus_ks > m)
      logger.error("apply_n_plus_ks > m =" + apply_n_plus_ks + " > " + m)
    recursive_apply(apply_n_plus_ks)
  }

  def apply(n: Int): Int = {
    // the extra "+ k - s" is to ensure that the left operand is positive
    val i_ = (n - argmin) % ks
    val i = if (i_ < 0) i_ + ks else i_
    //preproc(i)
    /*val preproc_i = preproc_0 + i
    if (preproc_i <= max_val)
      preproc_i
    else
      preproc_i - ks*/
    min_val + i
  }
}

/**
  * Functor class to compute S_{p,m} function.
  */
class S_pm(p: Int, m: Int) {
  /*@transient lazy*/ val logger = LogManager.getLogger

  var cumsumD: BigDecimal = 0

  def apply(): BigInt = {
    var cumsum: BigInt = 0
    for(s <- 1 to p) {
      if (s % 100 == 0)
        logger.info("s=" + s + ", cumsum=" + cumsum)
      for(k <- s+1 to p) {
        //logger.info("k=" + k)
        try {
          val M = new M_mks(m, k, s)

          // F_{m,k,s} is the set of fixed points of M_{m,k,s}
          // no reason to iterate over the entire (1 to m) when we're only looking
          // for values equal to one of the preprocessed values
          val mn = M.min_val
          val mx = scala.math.min(M.max_val, m)
          val mmn = M(M.min_val)
          val mmx = M(M.max_val)
          val mmnn = scala.math.min(mmn, mmx)
          val mmxx = scala.math.max(mmn, mmx)

          var SF_mks = 0
          /*if (scala.math.abs(mmxx - mmnn - (mx - mn)) > 10)
            logger.info(s"$mmxx-$mmnn<$mx-$mn : ${mmxx - mmnn}<${mx - mn} : ${mmxx - mmnn - (mx - mn)}")*/
          if (mmxx - mmnn < mx - mn)
            SF_mks = (mmnn to mmxx).filter(n => n == M(n)).sum
          else
            SF_mks = (mn to mx).filter(n => n == M(n)).sum

          //val SF_mks = (1 to m).filter(n => n == M(n)).sum

          /*val mmn = M(M.min_val)
          val mmx = M(M.max_val)
          logger.info(s"mmn=$mmn, mmx=$mmx")

          /*val i_ = (M.max_val - M.argmin) % M.ks
          val i = if (i_ < 0) i_ + M.ks else i_*/
          val argmin = M.max_val - M(M.min_val) + M.min_val
          //logger.info("mn=" + mn + " to mx=" + mx + ", min/max_val=" + M.min_val + "/" + M.max_val + ", M.argmin=" + M.argmin + ", argmin=" + argmin)
          if (M(argmin) != M.min_val)
            logger.error("error1")
          if (argmin < M.min_val || argmin > M.max_val)
            logger.error("error2")*/

          /*val v2 = (1 to m).filter(n => n == M.recursive_apply(n)).sum
          if (SF_mks != v2)
            logger.error(SF_mks + " != " + v2 + ", s=" + s + ", k=" + k)*/
          cumsum += SF_mks
          cumsumD += SF_mks
        } catch {
          case e: Throwable => logger.error("s=" + s + ", k=" + k)
        }
      }
    }
    cumsum
  }
}
