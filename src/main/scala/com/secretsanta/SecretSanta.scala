package com.secretsanta

import scala.annotation.tailrec
import scala.util.Random

object SecretSanta extends App {

  val entrantList = List("Name1", "Name2", "Name3", "Name4")

  @tailrec
  def pullName(giftBuyers: List[String], giftReceivers: List[String], results: List[(String, String)] = List()): List[(String, String)] = {

    giftBuyers.size match {
      case 1 => results ++ List((giftBuyers(0), giftReceivers(0)))
      case _ =>
        val buyer: String = giftBuyers(scala.util.Random.nextInt(giftBuyers.size))
        val receiverList = giftReceivers.filter(x => x != buyer)
        val receiver: String = receiverList(scala.util.Random.nextInt(receiverList.size))
        pullName(giftBuyers.filter(x => x != buyer), giftReceivers.filter(x => x != receiver), results ++ List((buyer, receiver)))
    }

  }

  pullName(entrantList, entrantList).foreach { case (buyer, receiver) => println(s"Hi $buyer, \n" +
    s"I'm struggling to think of a gift for $receiver this year (I'm surprised they're even on the list!). I'm trusting you to buy a gift for $receiver this year.\nThanks, \nSanta") }
}
