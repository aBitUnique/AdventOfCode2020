package com.day5

import scala.annotation.tailrec
import scala.io.Source

object DayFive extends App {

  val boardingPassData: List[String] = Source.fromResource("dayfive.txt").getLines.toList

  @tailrec
  def getSeatNumber(binarySeatCode: String, rows: List[Int] = Range(0,128).toList, columns: List[Int] = Range(0,8).toList): (Int, Int) = {

    binarySeatCode.isEmpty match {
      case true => (rows(0),columns(0))
      case false => {
        binarySeatCode.charAt(0) match {
          case 'F' => getSeatNumber(binarySeatCode.drop(1), rows.take(rows.size / 2), columns)
          case 'B' => getSeatNumber(binarySeatCode.drop(1), rows.takeRight(rows.size / 2), columns)
          case 'L' => getSeatNumber(binarySeatCode.drop(1), rows, columns.take(columns.size / 2))
          case 'R' => getSeatNumber(binarySeatCode.drop(1), rows, columns.takeRight(columns.size / 2))
        }
      }
    }
  }

  @tailrec
  def boardingPassSeatNumberTotals(binarySeatCodes: List[String], seatNumberTotals: List[Int] = List()): List[Int] = {
    binarySeatCodes.isEmpty match {
      case true => seatNumberTotals.sorted
      case false =>
        boardingPassSeatNumberTotals(binarySeatCodes.drop(1),seatNumberTotals ++ List((getSeatNumber(binarySeatCodes.head)._1 * 8) + getSeatNumber(binarySeatCodes.head)._2))
    }
  }

  def findMySeatId(binarySeatCodes: List[String]) = {
    val allSeats = Range(boardingPassSeatNumberTotals(binarySeatCodes).min, boardingPassSeatNumberTotals(binarySeatCodes).max + 1)
    allSeats.toSet.diff(boardingPassSeatNumberTotals(boardingPassData).toSet)
  }

  println(boardingPassSeatNumberTotals(boardingPassData).max)
  println(findMySeatId(boardingPassData))

}
