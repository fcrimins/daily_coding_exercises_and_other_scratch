import java.io._
import org.apache.logging.log4j._
import scala.collection.immutable.Range
import scala.collection.parallel._

/**
  * https://projecteuler.net/problem=555
  * Created by fred on 11/11/16.
  */
object Problem555 {
  /*@transient lazy*/ val logger = LogManager.getLogger

  def main(args: Array[String]): Unit = {

    val s0: BigInt = Range.BigInt(250000, 357143, 1).sum
    val s1: BigInt = (250000 until 357143).sum
    logger.info(s"s0=$s0, s1=$s1")
    //System.exit(1)

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

    logger.info("S_10_10()=" + new S_pm(10, 10)()) // 225
    logger.info("S_1000_1000()=" + new S_pm(1000, 1000)()) // 208724467
    logger.info("S_10000_10000()=" + new S_pm(10000, 10000)()) // 208541582207

    val pm = 1e6.toInt
    val S_1e_1e = new S_pm(pm, pm)
    val cumsum = S_1e_1e()
    logger.info("S_1e_1e()D=" + S_1e_1e.cumsumD)
    logger.info("S_1e_1e()=" + cumsum) // 208517717451208352 (odd how they all start w/ 2s, I wonder if there's a pattern akin to that of prime numbers)
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

  def apply(n: BigInt): BigInt = {
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

    // Range.BigInt has exclusive end
    val s_range = Range.BigInt(1, p+1, 1).par
    //val s_range = Range.BigInt(750001, 750002, 1).par

    // http://docs.scala-lang.org/overviews/parallel-collections/configuration.html
    s_range.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(14))

    val cumsums = s_range.map { (s: BigInt) => {
      if (s % 1000 == 0)
        logger.info(s"_s=$s")

      val k_range = Range.BigInt(s+1, p+1, 1) // not parallel

      val foldinit: BigInt = 0
      val cs: BigInt = k_range.fold(foldinit) { (foldcum: BigInt, k: BigInt) => {
        //logger.info("k=" + k)
        val M = new M_mks(m, k.toInt, s.toInt)

        // F_{m,k,s} is the set of fixed points of M_{m,k,s}
        // no reason to iterate over the entire (1 to m) when we're only looking
        // for values equal to one of the preprocessed values
        //val SF_mks = (1 to m).filter(n => n == M(n)).sum

        // there are only 3 options: (1) M can be constant, e.g. 91, (2) it can
        // be equal to itself for the first element and every element over the
        // [min,max] range, or (3) it can not coincide ever with its input
        var SF_mks: BigInt = 0
        if (M.min_val == M.max_val) {
          SF_mks = M.max_val
          //logger.info(s"s=$s k=$k v=${M.max_val} minmax")
        }
        else if (M.min_val == M(M.min_val)) {
          val N: BigInt = M.max_val - M.min_val + 1
          SF_mks = N * (M.min_val - 1) + N * (N + 1) / 2 // don't factor out N to avoid odd N+1
          //SF_mks = Range.BigInt(M.min_val, M.max_val+1, 1).sum
          //SF_mks = (M.min_val to M.max_val).sum // <- int overflow
          //logger.info(s"s=$s k=$k v=${M.min_val}-${M.max_val}")
          //(M.min_val to M.max_val).foreach(v => logger.info(s"s=$s k=$k v=$v"))
        }

        /*if (SF_mks != 0)
          logger.info(s"k=$k for SF_mks=$SF_mks")*/
        foldcum + SF_mks
      }}

      if (cs < 0) {
        logger.fatal(s"cs=$cs for s=$s, p=$p, m=$m")
        System.exit(1)
      }

      if (p == 1e6)
        logger.info(s"s=$s, cs=$cs")
      cs
    }}

    val file = new File(s"cumsums_$p.tsv")
    val bw = new BufferedWriter(new FileWriter(file))
    (1 to p).zip(cumsums).foreach(t => bw.write(s"${t._1}\t${t._2}\n"))
    bw.close()

    cumsumD = cumsums.map(_.toDouble).sum
    cumsums.sum
  }
}
