package com.day8

import scala.io.Source

object DayEight extends App {

  val videoGameInstructions: List[String] = Source.fromResource("dayeight.txt").getLines.toList

  val instructionsList = videoGameInstructions.map(x => x.split(" ").toList)

  def instructionParser(instructionList: List[List[String]], instruction: Int = 0, instructionsHit: List[Int] = List(), total: Int = 0): Unit = {
    instruction match {
      case x if (instruction <= (instructionList.size - 1)) =>
        instructionList(instruction) match {
        case x if instructionsHit.contains(instruction) => println(s"The broken total is $total")
        case x => x.head match {
          case "nop" => instructionParser (instructionList, instruction + 1, instructionsHit ++ List (instruction), total)
          case "acc" => instructionParser (instructionList, instruction + 1, instructionsHit ++ List (instruction), total + x (1).toInt)
          case "jmp" => instructionParser (instructionList, instruction + x (1).toInt, instructionsHit ++ List (instruction), total)
        }
    }
      case x if (x == instructionList.size) => println (s"The fixed total is $total")
      case x if (x >= (instructionList.size + 1)) => println (s"This one goes beyond our list")
    }
  }

  def instructionSwitcher(instructionList: List[List[String]], instruction: Int = 0): Unit = {
    instruction match {
      case _ if (instruction < instructionList.size) => {
        instructionList(instruction) match {
          case x => x.head match {
            case "nop" => instructionParser(instructionList.updated(instruction, List("jmp", instructionList(instruction)(1))))
              instructionSwitcher(instructionList, instruction + 1)
            case "jmp" => instructionParser(instructionList.updated(instruction, List("nop", instructionList(instruction)(1))))
              instructionSwitcher(instructionList, instruction + 1)
            case "acc" =>
              instructionSwitcher(instructionList, instruction + 1)
          }
        }
      }
      case _ => println("End.")
    }
  }

  instructionParser(instructionsList)
  instructionSwitcher(instructionsList, 0)


}
