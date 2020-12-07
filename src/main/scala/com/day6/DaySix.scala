package com.day6

import scala.io.Source

object DaySix extends App {

  val surveyResponseData: List[String] = Source.fromResource("daysix.txt").getLines.toList

  def concatLists(surveyResponse: List[String], intermediateResult: List[String] = List(), resultsList: List[List[String]] = List()): List[List[String]] = {
    surveyResponse.size match {
      case 0 => resultsList ++ List(intermediateResult)
      case _ => surveyResponse.head match {
        case "" => concatLists(surveyResponse.tail, List(), resultsList ++ List(intermediateResult))
        case _ => concatLists(surveyResponse.tail, intermediateResult ++ List(surveyResponse.head), resultsList)
      }
    }
  }

  def questionsAnsweredYes(groupResponses: List[List[String]]) = {
    groupResponses.map(x => x.map(x => x.toSet).reduceLeft(_.union(_)).size).sum
  }

  def questionsAnsweredYesByAll(groupResponses: List[List[String]]) = {
    groupResponses.map(x => x.map(x => x.toSet).reduceLeft(_.intersect(_)).size).sum
  }

  println(questionsAnsweredYes(concatLists(surveyResponseData)))
  println(questionsAnsweredYesByAll(concatLists(surveyResponseData)))

}
