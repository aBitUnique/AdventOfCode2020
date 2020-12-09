package com.day8

import scala.io.Source

object DayEight extends App {

  val videoGameInstructions: List[String] = Source.fromResource("dayeight.txt").getLines.toList

  val instructionsList = videoGameInstructions.map(x => x.split(" ").toList)

  def instructionParser(instructionList: List[List[String]], instructionPosition: Int = 0, instructionsHit: List[Int] = List(), total: Int = 0): Unit = {
    instructionPosition match {
      case x if (instructionPosition <= (instructionList.size - 1)) =>
        instructionList(instructionPosition) match {
        case x if instructionsHit.contains(instructionPosition) => println(s"The broken total is $total")
        case x => x.head match {
          case "nop" => instructionParser (instructionList, instructionPosition + 1, instructionsHit ++ List (instructionPosition), total)
          case "acc" => instructionParser (instructionList, instructionPosition + 1, instructionsHit ++ List (instructionPosition), total + x (1).toInt)
          case "jmp" => instructionParser (instructionList, instructionPosition + x (1).toInt, instructionsHit ++ List (instructionPosition), total)
        }
    }
      case x if (x == instructionList.size) => println (s"The fixed total is $total")
      case x if (x >= (instructionList.size + 1)) => println (s"This one goes beyond our list")
    }
  }

  def instructionSwitcher(instructionList: List[List[String]], instructionChanged: Int = 0): Unit = {
    instructionChanged match {
      case _ if (instructionChanged < instructionList.size) => {
        instructionList(instructionChanged) match {
          case x => x.head match {
            case "nop" => instructionParser(instructionList.updated(instructionChanged, List("jmp", instructionList(instructionChanged)(1))))
              instructionSwitcher(instructionList, instructionChanged + 1)
            case "jmp" => instructionParser(instructionList.updated(instructionChanged, List("nop", instructionList(instructionChanged)(1))))
              instructionSwitcher(instructionList, instructionChanged + 1)
            case "acc" =>
              instructionSwitcher(instructionList, instructionChanged + 1)
          }
        }
      }
      case _ => println("End.")
    }
  }

  instructionParser(instructionsList)
  instructionSwitcher(instructionsList)

}
