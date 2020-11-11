import scala.part1._

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(99)
    val (r, _) = Chap6.ints(20)(rng)
    println(r)
  }
}