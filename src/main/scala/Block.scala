case class Block(
  index: Long,
  previousHash: String,
  timestamp: Long,
  data: String
)
{

  def hash =
    Block.sha256Hash(index+previousHash+timestamp+data)

  def generate(data: String, previousBlock: Block = this): Block = {
    import java.util.Date
    Block(
      previousBlock.index +1,
      previousBlock.hash,
      new Date().getTime,
      data
    )
  }

  override def toString: String = {
    s"index=${index}, hash=$hash, previousHash=$previousHash, timestamp=$timestamp, data=$data"
  }

}

object Block {

  val genesisBlock = Block(0, "", 0, "")
  val sha256 = java.security.MessageDigest.getInstance("SHA-256")

  def sha256Hash(text: String) : String =
    String.format("%064x",
      new java.math.BigInteger(1,
        sha256.digest(text.getBytes("UTF-8"))))

  def validate(_old: Block, _new: Block): Option[Block] =
    (_new.previousHash.equals(_old.hash)) match {
      case true => Some(_new)
      case _ => None
    }

  def main(args: Array[String]): Unit = {

    val block1 = Block.genesisBlock.generate("lol")
    val block2 = block1.generate("pouet")
    val block3 = block2.generate("pouet pouet")
    val block4 = block1.generate("noo")
    val blockChain: Seq[Block] = Seq(block1, block2, block3, block4)

    val validatedBlockChain = blockChain.foldLeft(Seq[Option[Block]]())((acc, b) => {
      acc match {
        case x::xs if x.nonEmpty => validate(x.get,b) +: acc
        case _ => acc:+validate(genesisBlock, b)
      }
    })

    print(validatedBlockChain.map(_.toString).mkString("\n"))

  }

}




