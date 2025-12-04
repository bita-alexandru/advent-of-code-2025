package adventofcode.day04

import scala.io.Source
import scala.annotation.tailrec
import scala.runtime.LazyBoolean
import scala.runtime.LazyVals

def input(isExample: Boolean) =
    val inputFile = if isExample then "example.txt" else "input.txt"
    Source.fromFile(inputFile)
        .getLines()
        .map(_.strip())
        .toSeq
end input

def part1(isExample: Boolean = false) = 
    val diagram = input(isExample)
    {
        for row <- 0 until diagram.length
            col <- 0 until diagram(row).length
            if diagram(row)(col) == '@'
            if Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
                .foldLeft(0): (acc, d) =>
                    val rowd = row + d._1
                    val cold = col + d._2
                    if rowd < 0 || rowd >= diagram.length || cold < 0 || cold >= diagram(0).length then
                        acc
                    else
                        acc + (if diagram(rowd)(cold) == '@' then 1 else 0)
            < 4
        yield (row, col)
    }.length
end part1

def part2(isExample: Boolean = false) =
    val diagram = input(isExample)

    @tailrec
    def countRemoved(removedSet: Set[(Int, Int)] = Set.empty): Int =
        val removed =
            for row <- 0 until diagram.length
                col <- 0 until diagram(row).length
                if diagram(row)(col) == '@' && !removedSet.contains((row, col))
                if Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
                    .foldLeft(0): (acc, d) =>
                        val rowd = row + d._1
                        val cold = col + d._2
                        if rowd < 0 || rowd >= diagram.length || cold < 0 || cold >= diagram(0).length then
                            acc
                        else
                            acc + (if diagram(rowd)(cold) == '@' && !removedSet.contains((rowd, cold)) then 1 else 0)
                < 4
            yield (row, col)
        if removed.length == 0 then removedSet.size
        else
            countRemoved(removedSet.union(removed.toSet))
    end countRemoved

    countRemoved()
end part2

@main def main() =
    println(part1())
    println(part2())
end main