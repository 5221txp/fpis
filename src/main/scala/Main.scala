import scala.part1._

object Main {
  def main(args: Array[String]): Unit = {
    val stream = Stream2(1, 2, 3, 4, 5, 0)
    println(stream.append(stream).toList)
    println(Stream2.mapWithUnfold(Stream2.constant(3))(i => i + 1).take(10).toList)
  }
}