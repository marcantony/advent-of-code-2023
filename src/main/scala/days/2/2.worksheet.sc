import scala.io.Source
sealed trait ColorCount
case class Red(n: Int) extends ColorCount
case class Green(n: Int) extends ColorCount
case class Blue(n: Int) extends ColorCount
object ColorCount {
    def apply(s: String): Option[ColorCount] = s match {
        case s"$n red" => n.toIntOption.map(Red(_))
        case s"$n green" => n.toIntOption.map(Green(_))
        case s"$n blue" => n.toIntOption.map(Blue(_))
        case _ => None
    }
}

ColorCount("1 red")
ColorCount("2 green")
ColorCount("3 blue")
ColorCount("143 red")
ColorCount("4 asdf")
ColorCount("5red")
ColorCount("q green")

case class Drawing(red: Red, green: Green, blue: Blue)
object Drawing {
    val EmptyDrawing = Drawing(Red(0), Green(0), Blue(0))
    def apply(s: String): Option[Drawing] = {
        val splitByColor = s.split(',').map(_.trim())
        splitByColor.map(ColorCount(_)).foldLeft[Option[Drawing]](Some(EmptyDrawing))((acc, colorCount) => for {
            d <- acc
            c <- colorCount
        } yield c match
            case r: Red => d.copy(red = r)
            case g: Green => d.copy(green = g)
            case b: Blue => d.copy(blue = b)
        )
    }
}

Drawing("3 blue, 4 red")
Drawing("1 red, 2 green, 6 blue")
Drawing("13 green")
Drawing("1 red, a blue")

case class Game(id: Int, drawings: Seq[Drawing]) {
    def obeysLimit(r: Red, g: Green, b: Blue): Boolean = {
        drawings.filter(d => d.red.n > r.n || d.green.n > g.n || d.blue.n > b.n).isEmpty
    }

    def minCubeSet: (Red, Green, Blue) = {
        val minSet = drawings.foldLeft((0, 0, 0))((acc, d) =>
            (Math.max(acc._1, d.red.n), Math.max(acc._2, d.green.n), Math.max(acc._3, d.blue.n)))
        (Red(minSet._1), Green(minSet._2), Blue(minSet._3))
    }
}
object Game {
    def apply(s: String): Option[Game] = s match {
        case s"Game $idString: $drawingsString" =>
            val drawings = drawingsString.split(';').map(_.trim()).map(Drawing(_)).foldLeft[Option[List[Drawing]]](Some(List()))((listAccum, drawing) => for {
                l <- listAccum
                d <- drawing
            } yield d :: l)
            for {
                id <- idString.toIntOption
                d <- drawings
            } yield Game(id, d.reverse)
        case _ => None
    }
}

Game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
Game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
Game("Game 3: hello")
Game("asdf")

def testPart1(games: Seq[Game]): Int = {
    val r = Red(12)
    val g = Green(13)
    val b = Blue(14)

    games.filter(_.obeysLimit(r, g, b)).map(_.id).sum
}

val testInput = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
""".trim().split('\n').map(_.trim()).map(Game(_)).map(_.get).toSeq

testPart1(testInput)

val gamesFile = Source.fromFile("src/main/scala/days/2/games.txt")
val lines = gamesFile.getLines().toList
gamesFile.close()
val fullInput = lines.map(Game(_)).map(_.get)

testPart1(fullInput)

def testPart2(games: Seq[Game]): Int = {
    games.map(_.minCubeSet).map(t => t._1.n * t._2.n * t._3.n).sum
}

testPart2(testInput)
testPart2(fullInput)
