package fr.uge.td.scala

class Bike (val color: String, val wheelSize: Int, val speed: Int = 0){
  def speedUp(value: Int): Either[String, Bike] = {
    if (value < 0) Left("Valeur négatif")
    else Right(new Bike(color, wheelSize, speed + value))
  }
  def brake(value: Int): Option[Bike] = {
    if(value < 0) None
    else Some(new Bike(color, wheelSize, speed - value))
  }


}

object Bike{
  def main(args: Array[String]): Unit = {
    val bike = new Bike("Blue", 10)
    println(bike.speed)
    val bike2 = bike.speedUp(10)
    println(bike2.isRight)
    val bike3 = bike.brake(10)
    println(bike3.isEmpty)
    val bike4 = bike.speedUp(-10)
    println(bike4.left.e)
    val bike5 = bike.brake(-20)
    println(bike5.isEmpty)
  }
}

/*EXERCICE 2
  2.1
      sealed : Permet de faire en sorte de ne pas avoir d'héritage et de prévenir le compilateur pour les switch case
      case : Pas de new, ajout de factory method pour construteur et une method copy
            pour la comparaison on compare la structure et pas la référence
      Accepter un type et ses sous-types
      Nothing : Remplace le null pour la tail de la liste chainés

* */