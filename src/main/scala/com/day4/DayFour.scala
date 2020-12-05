package com.day4

import java.io

import com.day4.DayFour.passportBatch

import scala.io.Source

object DayFour extends App {

  val passportBatch: List[String] = Source.fromResource("dayfour.txt").getLines.toList

  val passportData = parsePassportBatch(passportBatch)

  println(passportData.filter(x => x.fullPassport(x)).size)
  println(passportData.filter(x => x.fullPassport(x)).filter(x => x.checkValidity(x)).size)


  def parsePassportBatch(passportBatch: List[String]) = {
    val passportList = passportBatch.map(x => if (x.isEmpty) "," else x + " ").foldLeft(new StringBuilder())(_ append _).toString().split(',').toList.map(x => x.split(" ").toList)
    val passportItems: Seq[List[Array[String]]] = passportList.map(x => x.map(x => x.split(':')))
    val y: Seq[List[(String, String)]] = passportItems.map(x => x.map(x => (x(0) -> x(1))))
    val z = y.map(x => Passport(x.toMap))
    z
  }

  case class Passport (byr: Option[String] = None, iyr: Option[String] = None, eyr: Option[String] = None, hgt: Option[String] = None, hcl: Option[String] = None, ecl: Option[String] = None, pid: Option[String] = None, cid: Option[String] = None) {

    def fullPassport(passport: Passport): Boolean = {
      if (
          (passport.byr.isDefined &&
          passport.iyr.isDefined &&
          passport.eyr.isDefined) &&
          passport.hgt.isDefined &&
          passport.hcl.isDefined &&
          passport.ecl.isDefined &&
          passport.pid.isDefined
      ) true else false
    }

    def checkValidity(passport: Passport): Boolean = {

      if (
        (if (passport.byr.get.toInt <= 2002 && passport.byr.get.toInt >= 1920) true else false) &&
        (if (passport.iyr.get.toInt <= 2020 && passport.iyr.get.toInt >= 2010) true else false) &&
        (if (passport.eyr.get.toInt <= 2030 && passport.eyr.get.toInt >= 2020) true else false) &&
        (if ((passport.hgt.get.takeRight(2) == "cm" && passport.hgt.get.dropRight(2).toInt >= 150 && passport.hgt.get.dropRight(2).toInt <= 193) ||
            (passport.hgt.get.takeRight(2) == "in" && passport.hgt.get.dropRight(2).toInt >= 59 && passport.hgt.get.dropRight(2).toInt <= 76)) true else false) &&
        (if (passport.hcl.get.charAt(0) == '#' && passport.hcl.get.size == 7 && passport.hcl.get.drop(1).forall(_.isLetterOrDigit)) true else false) &&
        (if (List("amb","blu","brn","gry","grn","hzl","oth").contains(passport.ecl.get)) true else false) &&
        (if (passport.pid.get.size == 9 && passport.pid.get.forall(_.isDigit)) true else false)
      ) true else false

    }

  }

  object Passport {
    def apply(x: Map[String, String]): Passport = {
      Passport(
        x.get("byr"),
        x.get("iyr"),
        x.get("eyr"),
        x.get("hgt"),
        x.get("hcl"),
        x.get("ecl"),
        x.get("pid"),
        x.get("cid")
      )
    }
  }
}
