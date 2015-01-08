package poliglot

object Input {
  val punctuation = Seq('.',  '-', ',', '!', '(', ')', '/', ':', '„', '”', '?',
    '\'', '—', '’', ''')

  val delims = """\[\] .,?!-"""
  val wordRegex = s"""[^$delims]+|[.,?!-]+""".r

  def tokens(input: String) =
    wordRegex
      .findAllMatchIn(input)
      .map(_.toString())
}
