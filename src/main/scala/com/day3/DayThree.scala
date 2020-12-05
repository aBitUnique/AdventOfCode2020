package com.day3

import scala.io.Source

object DayThree {

  val testSlope = List(
    "..##.........##.........##.........##.........##.........##.......",
  "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
  ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
  "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
  ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
  "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
  ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
  ".#........#.#........#.#........#.#........#.#........#.#........#",
  "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
  "#...##....##...##....##...##....##...##....##...##....##...##....#",
  ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#")

  val slopeTemplate: List[String] = Source.fromResource("daythree.txt").getLines.toList

  def numberOfTrees(slope: List[String], movementVertical: Int = 1, movementHorizontal: Int = 1, slopeVertical: Int = 0, slopeHorizontal: Int = 0, treesHit: Int = 0): Int = {

    val testString = "..##.........##.........##.........##.........##.........##......."

    val slopeTemplateHeight: Double = slope.size
    val slopeTemplateWidth: Double = slope(0).size
    val repeatFactor: Double = ((slopeTemplateHeight.toDouble / slopeTemplateWidth.toDouble).ceil * movementHorizontal)
    val trueSlope: List[String] = slope.map(x => x * repeatFactor.toInt)

     slopeVertical match {
      case _ if (slope.size <= slopeVertical) => treesHit
      case _ => trueSlope(slopeVertical).charAt(slopeHorizontal) match {
        case '#' => numberOfTrees(slope, movementVertical, movementHorizontal, slopeVertical + movementVertical, slopeHorizontal + movementHorizontal, treesHit + 1)
        case _ => numberOfTrees(slope, movementVertical, movementHorizontal, slopeVertical + movementVertical, slopeHorizontal + movementHorizontal, treesHit)
      }

    }

  }

  def totalNumberOfTrees(slope: List[String], movementList: List[(Int, Int)], slopesCalculated: Int = 0, total: Double = 1): Double = {
    slopesCalculated match {
      case _ if (slopesCalculated == movementList.size) => total
      case _ => println(slopesCalculated)
        totalNumberOfTrees(slope, movementList, slopesCalculated + 1, total * numberOfTrees(slope, movementList(slopesCalculated)._2, movementList(slopesCalculated)._1))
    }
  }

  println(totalNumberOfTrees(slopeTemplate, List((1,1),(3,1),(5,1),(7,1),(1,2))))

}
