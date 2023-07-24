object labsheet_6 {

  def caesarEncrypt(plaintext: String, shift: Int): String = {
    val encryptedText = plaintext.map {
      case x if x.isLetter =>
        val base = if (x.isUpper) 'A' else 'a'
        ((x - base + shift) % 26 + base).toChar
      case other => other
    }
    encryptedText
  }

  def caesarDecrypt(ciphertext: String, shift: Int): String = {
    caesarEncrypt(ciphertext, 26 - shift)
  }

  def cipher(processFunc: (String, Int) => String, text: String, shift: Int): String = {
    processFunc(text, shift)
  }

  def main(args: Array[String]): Unit = {
    val shiftValue = 3
    print("Enter a string here : ")
    val textToEncrypt = scala.io.StdIn.readLine()

    val encryptedText = cipher(caesarEncrypt, textToEncrypt, shiftValue)
    println("Encrypted Text: " + encryptedText)

    val decryptedText = cipher(caesarDecrypt, encryptedText, shiftValue)
    println("\n****Lets Decrypted it again****\n")
    println("Decrypted Text: " + decryptedText)
  }
}
