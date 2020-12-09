package com.day7

import scala.annotation.tailrec
import scala.io.Source

object DaySeven extends App {

  val luggageRules: List[String] = Source.fromResource("dayseven.txt").getLines.toList

  val y = luggageRules.map(x => x.replace("contain", ",")
    .replace(".","")
    .replace("bags","")
    .replace("bag","")
    .replace("no other","")
    .replace(" ","")
    .split(",").toList
  )

  val superBags = y.map(x => x.head)
  val subBags = y.map(x => x.tail)
  val bagMap = superBags.zip(subBags).toMap


  def duplicateN[A](n: Int, l: List[A]):List[A] = {
    l.flatMap{ e => List.fill(n)(e) }
  }

  def getSubBags(subBagsList: List[String], withValues: Boolean) ={
    if (withValues) subBagsList.flatMap(x => duplicateN(if (x(0).isDigit) x(0).asDigit else 1, bagMap.getOrElse(if (x(0).isDigit) x.tail else x, List(""))))
    else subBagsList.flatMap(x => bagMap.getOrElse(if (x(0).isDigit) x.tail else x, List("")))
  }

  @tailrec
  def getAllSubBags(subBagsList: List[String], withValues: Boolean = false, currentResults: List[String] = List()): List[String] = {
    getSubBags(subBagsList, withValues) match {
      case List() => currentResults ++ subBagsList
      case _ => getAllSubBags(getSubBags(subBagsList, withValues), withValues, currentResults ++ subBagsList)
    }
  }

  println(subBags.map(x => getAllSubBags(x).filter(x => x.endsWith("shinygold"))).filterNot(x => x.isEmpty).size)
  println(getAllSubBags(bagMap.getOrElse("shinygold", List()), true).map(x => x(0).asDigit).sum)

}
