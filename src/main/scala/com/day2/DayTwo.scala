package com.day2

import java.util.regex.Pattern
import scala.io.Source

object DayTwo {

  val passwordList: List[String] = Source.fromResource("daytwo.txt").getLines.toList
  val passwords = passwordList.map(x => (""":(.*)""".r findAllIn x).next.drop(2))
  val lowerBound = passwordList.map(x => ("""\d+""".r findAllIn x).toList(0).toInt)
  val upperBound = passwordList.map(x => ("""\d+""".r findAllIn x).toList(1).toInt)
  val letter = passwordList.map(x => (""".[a-z]""".r findFirstIn x).getOrElse(throw new Exception).trim)

  def taskOne(): Unit = {

    val truePasswords = for {
      i <- 0 to (passwordList.size-1)
      if ((passwords(i).count(_ == letter(i).charAt(0)) >= (lowerBound(i))) && (passwords(i).count(_ == letter(i).charAt(0)) <= (upperBound(i))))
    } yield 1

    println(truePasswords.sum)

  }

  def taskTwo(): Unit = {

    val truePasswords = for {
      i <- 0 to (passwordList.size-1)
      if ((passwords(i).charAt(lowerBound(i)-1) == letter(i).charAt(0)) ^ (passwords(i).charAt(upperBound(i)-1) == letter(i).charAt(0)))
    } yield 1

    println(truePasswords.sum)

  }

  taskOne()
  taskTwo()

}
