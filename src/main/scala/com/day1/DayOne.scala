package com.day1

import scala.io.Source

object DayOne {

  def newPrint(numbers: Int, total: Int): Unit = {

    val listOne: Seq[Int] = Source.fromResource("dayone.txt").getLines.toList.map(s => s.toInt)

    val answer = numbers match {
      case 2 => for (a <- listOne; b <- listOne if (a + b == total) && listOne.indexOf(b) > listOne.indexOf(a)) yield a * b
      case 3 => for (a <- listOne; b <- listOne; c <- listOne if (a + b + c == total) && listOne.indexOf(c) > listOne.indexOf(b) && listOne.indexOf(b) > listOne.indexOf(a)) yield a * b * c
    }

    println(answer)

  }

  newPrint(2, 2020)

}


