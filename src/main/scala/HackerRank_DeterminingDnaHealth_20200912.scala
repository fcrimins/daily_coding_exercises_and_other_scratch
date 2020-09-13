import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.annotation.tailrec

/**
  * https://www.hackerrank.com/challenges/determining-dna-health/problem
  *
  * This/these answers only correctly solved 2 of the solution problems, which is probably why this
  * problem has only been solved by 15% of responders.
  */
object HackerRank_DeterminingDnaHealth_20200912 {

  case class Healthifier(genes: Array[String], health: Array[Int]) {

    /**
      * igene: index into genes and health arrays
      * dnaStartIndex: index into `dna` string indicating start of an in-progress gene
      */
    case class InProgress(igene: Int, dnaStartIndex: Int)

    @tailrec
    final def computeHealth(first: Int, last: Int, dna: String, h: Int = 0): Int =  dna.length match {
      case 0 => h
      case _ =>
        val newh = h + (first to last).foldLeft(0) { (agg, j) =>
          agg + (if (dna.startsWith(genes(j))) health(j) else 0)
        }
        computeHealth(first, last, dna.tail, newh)
      /*
                  var totalHealth = 0
                  var inProgress = scala.collection.mutable.Set.empty[InProgress]

                  dna.zipWithIndex.foreach { case (dnaCh, i) =>

                      // add in-progress genes first so that if they're only 1 char their
                      // healths can immediately be aggregated and removed
                      (first to last).foreach { j =>
                          val geneCh = genes(j).charAt(0)
                          if (geneCh == dnaCh) {
                              inProgress += InProgress(j, i)
                              //println(f"gene($j)=${genes(j)} may start at dna($i)")
                          }
                      }

                      inProgress = inProgress.flatMap { ip =>
                          val geneIndex = i - ip.dnaStartIndex
                          val inProgressGene = genes(ip.igene)
                          val geneCh = inProgressGene.charAt(geneIndex)
                          if (dnaCh == geneCh) {
                              if (geneIndex == inProgressGene.length - 1) {
                                  totalHealth += health(ip.igene)
                                  //println(f"gene(${ip.igene})=${genes(ip.igene)} ended at dna($i)")
                                  None
                              } else {
                                  Some(ip)
                              }
                          } else None
                      }
                  }

                  totalHealth
      */
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt
    val genes = stdin.readLine.split(" ")
    val health = stdin.readLine.split(" ").map(_.trim.toInt)
    val healthifier = Healthifier(genes, health)

    // number of DNA sequences
    val s = stdin.readLine.trim.toInt

    val inputs: Seq[(Int, Int, String)] =
      (1 to s).map { case (i: Int) =>
        val firstLastd = stdin.readLine.split(" ")
        val first = firstLastd(0).trim.toInt
        val last = firstLastd(1).trim.toInt
        val dna = firstLastd(2)
        (first, last, dna)
      }

    val healths = inputs./*par.*/map { case (first, last, dna) =>
      healthifier.computeHealth(first, last, dna)
      //println(f"Health of strand `$dna` is $dHealth")
      //(math.min(dHealth, agg._1), math.max(dHealth, agg._2))
    }

    val minMax = (healths.min, healths.max)

    println(f"${minMax._1} ${minMax._2}")
  }
}

