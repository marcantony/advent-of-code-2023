import scala.io.Source

case class ParsedNumber(row: Int, startIndex: Int, endIndex: Int, value: Int) {
    def isAdjacentTo(x: Int, y: Int) = x >= startIndex - 1 && x <= endIndex && y >= row - 1 && y <= row + 1
}

def findPartNumbers(schematic: String): Iterable[ParsedNumber] = {
    val numberPattern = """\d+""".r
    val allNumbers = schematic.linesIterator.zipWithIndex
        .flatMap((line, row) => numberPattern.findAllMatchIn(line).map((_, row)))
        .map((regexMatch, row) => ParsedNumber(row, regexMatch.start, regexMatch.end, regexMatch.matched.toInt))

    val isAdjacentInSchema = isPartNumber(schematic) _
    allNumbers.filter(isAdjacentInSchema).toSeq
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

findPartNumbers(testInput).map(_.value).tapEach(n => println(s"$n is a part number")).sum

val schematicFile = Source.fromFile("src/main/scala/days/3/schematic.txt")
val schematic = schematicFile.mkString
schematicFile.close()

findPartNumbers(schematic).map(_.value).sum

def findAllInSchematic(schematic: String)(symbol: Char): Iterable[(Int, Int)] =
    schematic.linesIterator.zipWithIndex.flatMap((line, row) => {
        val symbolIndices = line.zipWithIndex.filter((c, _) => c == symbol).map(_._2)
        symbolIndices.map { (_, row) }
    }).toSeq

findAllInSchematic(testInput)('*')

def findGearRatios(schematic: String): Iterable[Int] = {
    val partNumbers = findPartNumbers(schematic)
    val possibleGears = findAllInSchematic(schematic)('*')

    val possibleGearsWithValues = (
        for {
            part <- partNumbers
            possibleGear <- possibleGears
        } yield if (part.isAdjacentTo(possibleGear._1, possibleGear._2)) Some((possibleGear, part)) else None)
        .flatten
        .groupMap(_._1)(_._2)
    println(possibleGearsWithValues)
    
    possibleGearsWithValues.collect {
            case (gear, parts) if parts.size == 2 => parts.map(_.value).product
        }
}

findGearRatios(testInput).sum

findGearRatios(schematic).sum
