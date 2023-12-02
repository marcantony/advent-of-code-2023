import scala.io.Source

val part1ValueExtractor: String => Option[Int] = line => {
    val predicate: Char => Boolean = _.isDigit
    val leftDigit = line.find(predicate)
    val rightDigit = line.reverse.find(predicate)

    for {
        l <- leftDigit
        r <- rightDigit
    } yield l.asDigit * 10 + r.asDigit
}

val part2ValueExtractor: String => Option[Int] = line => {
    val prefixToValue: PartialFunction[String, Int] = ({
        case Seq('o', 'n', 'e', _*) => 1
        case Seq('t', 'w', 'o', _*) => 2
        case Seq('t', 'h', 'r', 'e', 'e', _*) => 3
        case Seq('f', 'o', 'u', 'r', _*) => 4
        case Seq('f', 'i', 'v', 'e', _*) => 5
        case Seq('s', 'i', 'x', _*) => 6
        case Seq('s', 'e', 'v', 'e', 'n', _*) => 7
        case Seq('e', 'i', 'g', 'h', 't', _*) => 8
        case Seq('n', 'i', 'n', 'e', _*) => 9
        case Seq(c, _*) if c.isDigit => c.asDigit
    }: PartialFunction[Seq[Char], Int]).compose(s => (s: Seq[Char]))
    val tails = line.tails.toSeq
    val leftDigit = tails.collectFirst(prefixToValue)
    val rightDigit = tails.reverseIterator.collectFirst(prefixToValue)

    for {
        l <- leftDigit
        r <- rightDigit
    } yield l * 10 + r
}

def findTotalCalibrationValue(lines: Iterable[String], valueExtractor: String => Option[Int]): Option[Int] = {
    lines
        .map(valueExtractor)
        .fold(Some(0))((acc, numOpt) => for {
            runningSum <- acc
            num <- numOpt
        } yield runningSum + num)
}

findTotalCalibrationValue("""
    |1abc2
    |pqr3stu8vwx
    |a1b2c3d4e5f
    |treb7uchet
    """.stripMargin.trim.split('\n'), part1ValueExtractor)

findTotalCalibrationValue("""
    |1abc2
    |pqr3stu8vwx
    |asdf
    |treb7uchet
    """.stripMargin.trim.split('\n'), part1ValueExtractor)

findTotalCalibrationValue("""
    |1abc2
    |pqr3stu8vwx
    |a1b2c3d4e5f
    |treb7uchet
    """.stripMargin.trim.split('\n'), part2ValueExtractor)

findTotalCalibrationValue("""
    |two1nine
    |eightwothree
    |abcone2threexyz
    |xtwone3four
    |4nineeightseven2
    |zoneight234
    |7pqrstsixteen
    """.stripMargin.trim.split('\n'), part2ValueExtractor)

val calibrationDoc = Source.fromFile("""days\1\calibration.txt""")
val lines = calibrationDoc.getLines().toList
calibrationDoc.close()

findTotalCalibrationValue(lines, part1ValueExtractor)
findTotalCalibrationValue(lines, part2ValueExtractor)
