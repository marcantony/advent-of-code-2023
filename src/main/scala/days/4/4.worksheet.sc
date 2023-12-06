import scala.io.Source
import days.support.Utils

def parseCard(card: String): Option[(Set[Int], Set[Int])] = {
    def parseNums(s: String): Set[Int] = s.split(' ').filterNot(_.isEmpty()).map(_.toInt).toSet
    card match {
        case s"Card $n: $winningNumsString | $yourNumsString" =>
            Some((parseNums(winningNumsString), parseNums(yourNumsString)))
        case _ => None
    }
}

def scoreCard(card: String): Option[Int] = {
    val intersection = parseCard(card).map { case (winningNums, yourNums) => winningNums.intersect(yourNums) }
    intersection.map(nums => if (nums.isEmpty) 0 else Math.pow(2, nums.size - 1).intValue)
}

val testInput = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin.linesIterator.toSeq

def part1(cards: Seq[String]): Option[Int] = {
    val listOfScores = cards.map(scoreCard(_)).foldLeft[Option[List[Int]]](Some(List()))((acc, el) => for {
        a <- acc
        e <- el
    } yield e :: a)
    listOfScores.map(_.sum)
}

part1(testInput)

val cards = Utils.readFile("src/main/scala/days/4/cards.txt")

part1(cards)
