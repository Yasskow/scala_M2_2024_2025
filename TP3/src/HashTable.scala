import java.io.{File, RandomAccessFile}
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Timer
import scala.collection.mutable


class HashTable(val file : String) {
  private val offsetValue : Int = 20
  private val offsetKey : Int = 3
  private val totalOffset : Int = offsetValue + offsetKey + 1
  private val threshold : Double = 150
  private var mapStore : Map[String, Int] = Map.empty[String, Int]
  private val fileOpen = new RandomAccessFile(file, "rw");
  private var nbOffset = 0
  private var nbCopy = 1

  def add(key : String, value : Int) : Either[String, Unit] = {
    val valueToString = value.toString;
    val currentFileSize = fileOpen.length()
    if(valueToString.length > offsetValue){
      return Left("Your value or/and key are > to the offset")
    } else if(key.length != offsetKey){
      return Left("Your ket is in the wrong format")
    }
    else {
      if(currentFileSize >= threshold){
        checkThreadHold()
      }
      val diffStar = offsetValue - valueToString.length

      fileOpen.write((key + ":" + valueToString + "*" * diffStar).getBytes)

      mapStore = mapStore + (key -> nbOffset)

      nbOffset += totalOffset
      println("Added : " + key + " : " + get(key))
    }
    Right()
  }

  def get(key: String) : Option[Int] = {
    if(mapStore.contains(key)){
      Some(
        {
          fileOpen.seek(mapStore(key))
          val buffer = new Array[Byte](totalOffset)
          fileOpen.read(buffer)
          val str = new String(buffer)
          val value = str.replace("*", "").replace(key + ":", "")
          fileOpen.seek(nbOffset)
          value.toInt
        }
      )
    }else{
      val directory = new File(".")
      val files = directory.listFiles()
      None
    }
  }


  private def checkThreadHold() : Unit = {
    if(mapStore.nonEmpty){
      archive()
    }
  }
  private def archive() : Unit = {
    println("Size Before Archive : " + mapStore.size)
    val date = LocalDateTime.now();
    val formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmssSSS")
    val nameOfArchive = "Log_" + formatter.format(date)
    nbOffset = 0;
    Files.copy(Paths.get(file), Paths.get(nameOfArchive), StandardCopyOption.REPLACE_EXISTING)
    fileOpen.setLength(0)
    mapStore = Map.empty[String, Int]
    println("In Archive")
    println("Size after archive : " + mapStore.size)
  }
}

object HashTable{
  def main(args: Array[String]): Unit = {
    var hash = new HashTable("test.txt")
//    hash.add("def",1)
//    hash.add("def",2)
//    hash.add("def",3)
//    hash.add("def",4)
//    hash.add("def",5)
//    hash.add("def",6)
//    hash.add("abc",200)
//    hash.add("abc",300)
//    hash.add("abc",400)
//    hash.add("abc",500)
//    hash.add("def",9)
//    hash.add("def",10)
//    hash.add("def",11)
//    hash.add("def",12)
//    hash.add("def",13)
//    hash.add("def",14)
    val timer = new Timer()
    timer.schedule(new RandomExample(hash), 10, 500)
  }
}