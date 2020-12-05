package com.secretsanta

import scala.util.Random

object SecretSanta extends App {

  val entrantList = List("Sofia", "Dom", "Elisa", "Katy", "Rach", "Joe", "Andrea", "Voula", "Abi", "Fae", "Fidelma", "Bryn", "Eve", "Charly", "Alex")

  def pullName(giftBuyers: List[String], giftReceivers: List[String], results: List[(String, String)] = List()): List[(String, String)] = {

    giftBuyers.size match {
      case 1 => results ++ List((giftBuyers(0), giftReceivers(0)))
      case _ =>
        val buyer: String = giftBuyers(scala.util.Random.nextInt(giftBuyers.size))
        val receiverList = giftBuyers.filter(x => x != buyer)
        val receiver: String = giftReceivers(scala.util.Random.nextInt(receiverList.size))
        pullName(giftBuyers.filter(x => x != buyer), giftReceivers.filter(x => x != receiver), results ++ List((buyer, receiver)))
    }

  }

  pullName(entrantList, entrantList).foreach { case (buyer, receiver) => println(s"$buyer will gift a present to $receiver") }

}
