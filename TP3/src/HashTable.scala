import java.io.RandomAccessFile
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.time.{ LocalDateTime}
import java.time.format.DateTimeFormatter


class HashTable(val file : String) {
  val offset : Int = 20
  val threshold : Double = 0.4
  private var mapStore : Map[String, Int] = Map[String, Int]()
  val fileOpen = new RandomAccessFile(file, "rw");
  private var nbOffset = 0
  private var nbCopy = 1

  def add(key : String, value : Int) : Either[String, Unit] = {
    val valueToString = value.toString;
    if(valueToString.length > offset){
      return Left("Your value is > to the offset")
    }else {
      val diffStar = offset - valueToString.length

      fileOpen.write((valueToString + "*"*diffStar).getBytes)

     mapStore = mapStore + (key -> nbOffset);

      nbOffset += offset
      println("TEST")
      checkThreadHold()
    }
    Right()
  }


  private def checkThreadHold() : Unit = {
    if(mapStore.nonEmpty){
      if((mapStore.size.toDouble*offset / nbOffset.toDouble) < threshold){
        archive()
      }
    }
  }
  private def archive() : Unit = {
    var values : String = ""
    val date = LocalDateTime.now();
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH:mm:ss")
    var nameOfArchive = "Log_Archive_" + formatter.format(date)
    val buffer = new Array[Byte](offset)
    nbOffset = 0;
    if(Files.exists(Paths.get(nameOfArchive))){
      nameOfArchive = "Log_Archive_" + formatter.format(date) + "(" + nbCopy + ")"
      nbCopy += 1
    }else{
      nbCopy = 1
    }
    val path = Files.copy(Paths.get(file), Paths.get(nameOfArchive), StandardCopyOption.REPLACE_EXISTING)
    println(path.toString)

    mapStore.foreach( f => {
      fileOpen.seek(f._2)
      fileOpen.read(buffer)
      values = values + new String(buffer)
      mapStore = mapStore + (f._1 -> nbOffset)
      nbOffset += offset
    })
    fileOpen.setLength(0)
    fileOpen.write(values.getBytes)
  }
}

object HashTable{
  def main(args: Array[String]): Unit = {
    var hash = new HashTable("test.txt")
    hash.add("abc",100)
    hash.add("def",1)
    hash.add("def",2)
    hash.add("def",3)
    hash.add("def",4)
    hash.add("def",5)
    hash.add("def",6)
    hash.add("abc",200)
    hash.add("abc",300)
    hash.add("abc",400)
    hash.add("abc",500)
    hash.add("def",9)
    hash.add("def",10)
    hash.add("def",11)
    hash.add("def",12)
    hash.add("def",13)
    hash.add("def",14)
  }
}
