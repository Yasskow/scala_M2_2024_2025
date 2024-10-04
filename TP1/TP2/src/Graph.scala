package fr.uge.td.scala

import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.Map;

object Graph {

  def remove_center(tuple : List[(Int, Int, Int)]) : List[(Int, Int)] = {
    tuple.map{ case (x, _, z) => (x, z)};
  }

  def extract_racine(tuple : List[(Int, Int)]) : List[Int] = {
    val list = tuple.map{ case(_, x) => x};
    val list2 = tuple.map{ case(x, _) => x};
    list2.filter( x => !list.contains(x)).distinct;
  }

  def extract_leaf(tuple : List[(Int, Int)]) : List[Int] = {
    val list = tuple.map{ case(_, x) => x};
    val list2 = tuple.map{ case(x, _) => x};
    list.filter(x => !list2.contains(x)).distinct;
  }

  def join(pair1 : List[(Int,Int)], pair2: List[(Int,Int)]) : List[(Int,Int)] = {
    var tmp = pair1;
    var res = List[(Int, Int)]();
    do {
      res = tmp;
      tmp = (tmp.flatMap{ case (x, y) => pair2.map{ case (a, b) => if (y == a) (x, b) else (0,0)}.filterNot(_==(0,0))}:++pair2:++pair1).distinct
    }while(tmp.size != res.size)
    tmp
  }

  def removeOld(pair1 : List[(Int,Int)], pair2: List[(Int,Int)]) : List[(Int,Int)] ={
    join(pair1, pair2).filter(x => !pair1.contains(x) && !pair2.contains(x));
  }

  def rootedGraph(pair: List[(Int, Int)], root : Int) : List[(Int, Int)] = {
    var racine = extract_racine(pair);
    if(!racine.contains(root)){
      List[(Int, Int)]()
    }
   var filterPair = pair.filter( x => x._1 == root);
    join(filterPair, pair).filter(x => x._1 == root)

  }

  def main(args: Array[String]): Unit = {
    val n_uplet = List((1,0,5),(5,1,8),(8,2,1),(2,0,6),(3,0,6),(6,1,9),(5,1,9),(9,3,11),(9,4,12),(4,0,7),(7,1,9),(7,2,10),
      (14,1,15),(15,1,16),(14,1,16),(17,0,18),(18,0,19),(19,1,20),(20,0,17));
    val uplet = remove_center(n_uplet);

    println(extract_racine(uplet));
    println(extract_leaf(uplet));
    println(join(List((1,2), (3,4)), List((2,5), (1,2), (2,8), (5,7))));
    println(removeOld(List((1,2), (3,4)), List((2,5), (1,2), (2,8), (5,7))))
    println(rootedGraph(uplet, 1))
  }
}
