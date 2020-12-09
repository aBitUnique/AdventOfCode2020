package com.day9

import scala.annotation.tailrec
import scala.io.Source

object DayNine extends App {

  val portOutputData: List[BigDecimal] = Source.fromResource("daynine.txt").getLines.toList.map(x => BigDecimal(x))

  @tailrec
  def findWeakness(portOutput: List[BigDecimal], currentListEntry: Int = 25): BigDecimal = {
    canBeSummedFromPrevious(portOutput.slice((currentListEntry-25), currentListEntry.toInt), portOutput(currentListEntry.toInt)) match {
      case false => portOutput(currentListEntry.toInt)
      case true => findWeakness(portOutput, currentListEntry + 1)
    }
  }

  @tailrec
  def canBeSummedFromPrevious(listOfNumbers: List[BigDecimal], targetNumber: BigDecimal): Boolean = {
    listOfNumbers.size match {
      case 1 => false
      case _ => listOfNumbers.distinct.tail.map(x => x + listOfNumbers.head).contains(targetNumber) match {
        case true => true
        case false => canBeSummedFromPrevious(listOfNumbers.tail, targetNumber)
      }
    }
  }

  @tailrec
  def canBeSummedFromContiguous(listOfNumbers: List[BigDecimal], targetNumber: BigDecimal, currentStartPoint: Int = 0, sliceSize: Int = 2): BigDecimal = {
    listOfNumbers.slice(currentStartPoint, currentStartPoint + sliceSize).sum match {
      case x if (x == targetNumber) && (listOfNumbers.slice(currentStartPoint, currentStartPoint + sliceSize).distinct != 1) =>
        listOfNumbers.slice(currentStartPoint, currentStartPoint + sliceSize).max + listOfNumbers.slice(currentStartPoint, currentStartPoint + sliceSize).min
      case x if x > targetNumber =>
        canBeSummedFromContiguous(listOfNumbers, targetNumber, currentStartPoint + 1)
      case x if x < targetNumber =>
        canBeSummedFromContiguous(listOfNumbers, targetNumber, currentStartPoint, sliceSize + 1)
      case _ => canBeSummedFromContiguous(listOfNumbers, targetNumber, currentStartPoint + 1)
    }
  }

  println(findWeakness(portOutputData))

  println(canBeSummedFromContiguous(portOutputData, findWeakness(portOutputData)))

}
