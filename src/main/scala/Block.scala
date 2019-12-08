case class Block(
  index: Long,
  previousHash: String,
  timestamp: Long,
  data: String
) {
  def hash =
    Block.sha256Hash(index+previousHash+timestamp+data)
}

object Block {

  val genesisBlock = Block(0, "", 0, "")

  def generate(previousBlock: Block, data: String): Block = {
    import java.util.Date
    Block(
      previousBlock.index +1,
      previousBlock.hash,
      new Date().getTime,
      data
    )
  }

  def sha256Hash(text: String) : String =
    String.format("%064x",
      new java.math.BigInteger(1,
        java.security.MessageDigest.getInstance("SHA-256")
        .digest(text.getBytes("UTF-8"))))

  def main(args: Array[String]): Unit = {


    val blockChain = Seq(Block.genesisBlock)

  }

}




