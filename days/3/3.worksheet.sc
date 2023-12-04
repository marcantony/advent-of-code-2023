import scala.io.Source

case class ParsedNumber(row: Int, startIndex: Int, endIndex: Int, value: Int)
def findPartNumbers(schematic: String): Iterable[Int] = {
    val numberPattern = """\d+""".r
    val allNumbers = schematic.linesIterator.zipWithIndex
        .flatMap((line, row) => numberPattern.findAllMatchIn(line).map((_, row)))
        .map((regexMatch, row) => ParsedNumber(row, regexMatch.start, regexMatch.end, regexMatch.matched.toInt))

    val isAdjacentInSchema = isPartNumber(schematic) _
    allNumbers.filter(isAdjacentInSchema).map(_.value).toSeq
}

def isPartNumber(schematic: String)(number: ParsedNumber): Boolean = {
    println(s"Checking if $number is a part number")
    val schematicLines = schematic.linesIterator.toIndexedSeq
    
    val yValuesToCheck = number.row - 1 to number.row + 1
    val xValuesToCheck = number.startIndex - 1 to number.endIndex

    println(s"X values: $xValuesToCheck")
    println(s"Y values: $yValuesToCheck")

    val indicesToCheck = (for { x <- xValuesToCheck; y <- yValuesToCheck } yield (x, y))
        .tapEach(i => println(s"Validating $i"))
        .filter((x,  y) => {
            val line = schematicLines.lift(y)
            x >= 0 && line.map(l => x < l.length()).getOrElse(false)
        }).tapEach(i => println(s"Will check $i"))

    !indicesToCheck.filter { case (x, y) => {
        val c = schematicLines(y)(x)
        !c.isDigit && c != '.'
    }}.isEmpty
}

val testInput = """467..114..
                  |...*......
                  |..35..633.
                  |......#...
                  |617*......
                  |.....+.58.
                  |..592.....
                  |......755.
                  |...$.*....
                  |.664.598..""".stripMargin

findPartNumbers(testInput).tapEach(n => println(s"$n is a part number")).sum

val schematicFile = Source.fromFile("days/3/schematic.txt")
val schematic = schematicFile.mkString
schematicFile.close()

findPartNumbers(schematic).sum
