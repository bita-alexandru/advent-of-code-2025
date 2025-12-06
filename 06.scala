package adventofcode.day06

def input(isExample: Boolean) =
    val inputFile = if isExample then "example.txt" else "input.txt"
    scala.io.Source.fromFile(inputFile)
        .getLines()
        .map(_.toSeq)
        .toSeq
        .reverse match
            case ops +: numbers => (ops, numbers.reverse)
end input

def part1(isExample: Boolean = false) =
    val (ops, numbers) = input(isExample) match
        case (ops, numbers) =>
            (
                ops.toString.split("\\s+").filter(_.strip().nonEmpty),
                numbers.map(_.toString.split("\\s+").filter(_.strip().nonEmpty))
            )
    val (cols, rows) = (ops.length, numbers.length)
    (0 until cols).map: col =>
        val selected = (0 until rows).map(numbers(_)(col).toLong)
        if ops(col) == "+" then selected.sum
        else selected.product
    .sum
end part1

def part2(isExample: Boolean = false) =
    val (ops, numbers) = input(isExample)
    val (rows, cols) = (numbers.length, ops.length)
    (0 until cols)
        .foldLeft((prevOp = ops.head, prevNums = Seq[Long]().empty, total = 0L)): (acc, col) =>
            val numStr = (0 until rows).map(numbers(_)(col)).mkString.strip()
            val numLong = if numStr.isEmpty() then 0 else numStr.toLong
            val newOp = if ops(col) == ' ' then acc.prevOp else ops(col)
            val newNums = if numStr.isEmpty() then Seq[Long]().empty
                else numLong +: acc.prevNums
            val toAdd = if col == cols - 1|| numStr.isEmpty() then
                newOp match
                    case '+' => acc.prevNums.sum + (if col == cols - 1 then numLong else 0)
                    case '*' => acc.prevNums.product * (if col == cols - 1 then numLong else 1)
                else 0
            (newOp, newNums, acc.total + toAdd)
        .total
end part2

@main def main() =
    println(part1())
    println(part2())
end main