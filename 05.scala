package adventofcode.day05

import scala.collection.Searching.Found

def input(isExample: Boolean) =
    val inputFile = if isExample then "example.txt" else "input.txt"
    val Array(ranges, ids) = scala.io.Source.fromFile(inputFile)
        .mkString
        .split("\\R{2}")
    (
        ranges
            .split("\\R")
            .map:
                case s"$min-$max" => (BigInt(min.strip()), BigInt(max.strip()))
            .sorted
            .toSeq,
        ids
            .split("\\R")
            .map(id => BigInt(id.strip()))
            .toSeq
    )
end input

def part1(isExample: Boolean = false) = 
    val (ranges, ids) = input(isExample)
    ids
        .filter: id =>
            ranges.exists: range =>
                range._1 <= id && id <= range._2
        .length
end part1

def part2(isExample: Boolean = false) =
    val (ranges, _) = input(isExample)
    ranges.foldLeft((BigInt(0), BigInt(0)), BigInt(0)): (acc, curr) =>
        val (prev, total) = acc
        val newRange = (curr._1.max(prev._1), curr._2.max(prev._2))

        if curr._1 >= prev._1 && curr._2 <= prev._2 then
            (newRange, total)
        else
            val toAdd = curr._2 - curr._1 + 1
                - (if prev._2 >= curr._1 then prev._2 - curr._1 + 1 else 0)
            (newRange, total + toAdd)
    ._2
end part2

@main def main() =
    println(part1())
    println(part2())
end main