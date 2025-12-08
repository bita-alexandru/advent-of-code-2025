package adventofcode.day08

import scala.math.{sqrt, pow}

def input(isExample: Boolean) =
    val inputFile = if isExample then "example.txt" else "input.txt"
    scala.io.Source.fromFile(inputFile)
        .getLines()
        .map(_.split(",").map(_.toInt).toSeq)
        .toSeq.combinations(2)
        .map:
            case Seq(a, b) => (a = a, b = b, dist = sqrt(pow(a(0) - b(0), 2) + pow(a(1) - b(1), 2) + pow(a(2) - b(2), 2)))
        .toSeq
        .sortBy(_.dist)
end input

def part1(isExample: Boolean = false) =
    val ordJunc = input(isExample)
    var circId = 0
    var juncCircMap = collection.mutable.Map[Seq[Int], Int]().empty
    var circJuncsMap = collection.mutable.Map[Int, Set[Seq[Int]]]().empty
    var (connections, pos) = (0, 0)
    while connections < (if isExample then 10 else 1000) && pos < ordJunc.length do
        ordJunc(pos) match
            case (a, b, _) if !juncCircMap.contains(a) && !juncCircMap.contains(b) =>
                connections += 1
                juncCircMap.addOne(a -> circId)
                juncCircMap.addOne(b -> circId)
                circJuncsMap(circId) = Set(a, b)
                circId += 1
            case (a, b, _) if juncCircMap.contains(a) && !juncCircMap.contains(b) =>
                connections += 1
                juncCircMap.addOne(b -> juncCircMap(a))
                circJuncsMap(juncCircMap(a)) = circJuncsMap(juncCircMap(a)).incl(b)
            case (a, b, _) if !juncCircMap.contains(a) && juncCircMap.contains(b) =>
                connections += 1
                juncCircMap.addOne(a -> juncCircMap(b))
                circJuncsMap(juncCircMap(b)) = circJuncsMap(juncCircMap(b)).incl(a)
            case (a, b, _) if juncCircMap(a) == juncCircMap(b) => 
                connections += 1
            case (a, b, _) if juncCircMap(a) != juncCircMap(b) =>
                connections += 1
                val aCircId = juncCircMap(a)
                val bCircId = juncCircMap(b)
                circJuncsMap(aCircId) = circJuncsMap(aCircId).union(circJuncsMap(bCircId))
                circJuncsMap(bCircId).foreach(juncCircMap(_) = aCircId)
                circJuncsMap.remove(bCircId)
        pos += 1
    circJuncsMap.toSeq.sortBy(_._2.size).takeRight(3).map(_._2.size).product
end part1

def part2(isExample: Boolean = false) =
    val ordJunc = input(isExample)
    val points = ordJunc.flatMap(p => Seq(p.a, p.b)).distinct
    var circId = 0
    var juncCircMap = collection.mutable.Map[Seq[Int], Int]().empty
    var circJuncsMap = collection.mutable.Map[Int, Set[Seq[Int]]]().empty
    var (pos, res) = (0, -1L)
    while pos < ordJunc.length && res == -1L do
        ordJunc(pos) match
            case (a, b, _) if !juncCircMap.contains(a) && !juncCircMap.contains(b) =>
                juncCircMap.addOne(a -> circId)
                juncCircMap.addOne(b -> circId)
                circJuncsMap(circId) = Set(a, b)
                circId += 1
            case (a, b, _) if juncCircMap.contains(a) && !juncCircMap.contains(b) =>
                juncCircMap.addOne(b -> juncCircMap(a))
                circJuncsMap(juncCircMap(a)) = circJuncsMap(juncCircMap(a)).incl(b)
                if circJuncsMap(juncCircMap(a)).size == points.length then
                    res = 1L * a(0) * b(0)
            case (a, b, _) if !juncCircMap.contains(a) && juncCircMap.contains(b) =>
                juncCircMap.addOne(a -> juncCircMap(b))
                circJuncsMap(juncCircMap(b)) = circJuncsMap(juncCircMap(b)).incl(a)
                if circJuncsMap(juncCircMap(b)).size == points.length then
                    res = 1L * a(0) * b(0)
            case (a, b, _) if juncCircMap(a) == juncCircMap(b) => {}
            case (a, b, _) if juncCircMap(a) != juncCircMap(b) =>
                val aCircId = juncCircMap(a)
                val bCircId = juncCircMap(b)
                circJuncsMap(aCircId) = circJuncsMap(aCircId).union(circJuncsMap(bCircId))
                circJuncsMap(bCircId).foreach(juncCircMap(_) = aCircId)
                circJuncsMap.remove(bCircId)
                if circJuncsMap(aCircId).size == points.length then
                    res = 1L * a(0) * b(0)
        pos += 1
    res
end part2

@main def main() =
    println(part1())
    println(part2())
end main