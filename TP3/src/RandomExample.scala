import java.util.TimerTask
import scala.util.Random

class RandomExample(val hashTable: HashTable) extends TimerTask{
  val keys = List("abc", "def", "efg", "hij", "klm", "nop", "qrs", "tuv", "aaa", "ccc")

  override def run(): Unit = {
    val randomKey = Random.nextInt(10)
    val randomValue = Random.nextInt(9999999)
    hashTable.add(keys(randomKey), randomValue)
  }
}
