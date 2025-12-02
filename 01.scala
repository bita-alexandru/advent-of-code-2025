package adventofcode.day01

import scala.io.Source
import java.io.File

def input(isExample: Boolean) =
    val inputFile = if isExample then "example.txt" else "input.txt"
    Source.fromFile(new File(inputFile))
        .getLines()
            .map:
                line =>
                    val (direction, value) = line.splitAt(1)
                    (direction, value.toInt)
            .toList
end input

def part1(isExample: Boolean = false) = 
    input(isExample).foldLeft((50, 0)):
        case ((currVal, currRes), (direction, value)) =>
            val newVal =
                if direction == "L" then
                    (100 + currVal - value) % 100
                else
                    (currVal + value) % 100
            (newVal, currRes + (if newVal == 0 then 1 else 0))
end part1

def part2(isExample: Boolean = false) =
    input(isExample).foldLeft((50, 0)):
        case ((currVal, currRes), (direction, value)) =>
            val wholeTurns = value / 100
            val partTurn = value % 100
            val (newVal, cntZero) =
                if direction == "L" then
                    ((100 + currVal - partTurn) % 100,
                        if partTurn > 0 && currVal > 0 && currVal - partTurn <= 0 then 1 else 0)
                else
                    ((currVal + value) % 100,
                        if partTurn > 0 && currVal + partTurn >= 100 then 1 else 0)
            (newVal, currRes + cntZero + wholeTurns)
end part2

@main def main() =
    println(part1())
    println(part2())
end main