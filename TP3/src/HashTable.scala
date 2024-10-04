import java.io.RandomAccessFile
import scala.collection.mutable
import scala.io.Source

class HashTable(val file : String) {
  val offset : Int = 20
  val threshold : Double = 0.4
  private val mapStore : Map[String, Int] = Map[String, Int]()
  val fileOpen = new RandomAccessFile(file, "rw");
  private var nbElem = 0;



  def add(key : String, value : Int) : Either[String, Unit] = {
    val valueToString = value.toString;
    if(valueToString.length > offset){
      return Left("Your value is > to the offset")
    }else {
      val diffStar = offset - valueToString.length

      fileOpen.write((valueToString + "*"*diffStar).getBytes)

      mapStore + (key -> key * nbElem);

      nbElem += 1
    }
    Right()
  }
}
object HashTable{
  def main(args: Array[String]): Unit = {
    var test = new HashTable("test.txt")
    test.add("Coucou", 5454)
    test.add("titi", 121212)
  }
}
