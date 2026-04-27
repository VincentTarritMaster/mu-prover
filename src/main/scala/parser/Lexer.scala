package prover

/** An iterator over the tokens of a source file.
 *
 *  @param source The source file from which the tokens of this iterator are extracted.
 */
final class Lexer(val source: SourceFile):

  /** The current position of the lexer. */
  private var position: SourceFile.Index = 0

  /** Advances to the next token and returns it, or returns `None` if no next token exists. */
  def next(): Option[Token] =
    discardWhitespacesAndComments()
    peek().map { (head) =>
      if head == '!' then
        takeBang()
      else if Lexer.isLetterOrUnderscore(head) then
        takeKeywordOrIdentifier()
      else if Lexer.isOperator(head) then
        takeOperator()
      else
        takePunctuation()
    }

  /** Returns the next character without consuming it or `None` if the input has been consumed. */
  private def peek(): Option[Char] =
    if position != source.end then Some(source.text(position)) else None

  /** Discards all whitespaces and comments preceding the next token. */
  private def discardWhitespacesAndComments(): Unit =
    if (position != source.end) && source.text(position).isWhitespace then
      discard()
      discardWhitespacesAndComments()
    else if takePrefix("//").isDefined then
      Lexer.newline.findFirstMatchIn(source.text.subSequence(position, source.end))
        .map((m) => discard(m.end))
      discardWhitespacesAndComments()
    else
      ()

  /** Discards `count` characters. */
  private def discard(count: Int = 1): Unit =
    require(count > 0)
    position = Math.min(position + count, source.end)

  /** Returns the longest prefix of the input whose characters all satisfy `predicate` and advances
   *  the position of `this` until after that prefix.
   */
  private def takeWhile(predicate: Char => Boolean): CharSequence =
    val start = position
    while (position != source.end) && predicate(source.text(position)) do
      position += 1
    source.text.subSequence(start, position)

  /** If the input starts with `prefix`, returns the current position and advances to the position
   *  until after `prefix`. Otherwise, returns `None`.
   */
  private def takePrefix(prefix: String): Option[SourceFile.Index] =
    def loop(i: Int, j: Int): Option[SourceFile.Index] =
      if i == prefix.length then
        val start = position
        position = j
        Some(start)
      else if (j != source.end) && (prefix(i) == source.text(j)) then
        loop(i + 1, j + 1)
      else
        None
    loop(0, position)

  /** Consumes and returns a bang token. */
  private def takeBang(): Token =
    val start = position
    require(peek() == Some('!'))
    discard()
    Token(Token.bang, spanFrom(start))

  /** Consumes and returns a keyword or identifier. */
  private def takeKeywordOrIdentifier(): Token = {
    val start = position
    val text = takeWhile(Lexer.isLetterOrUnderscore)

    val tag: Token.Tag = text match
      case "_" => Token.underscore
      case "fun" => Token.fun
      case "in" => Token.in
      case "let" => Token.let
      case _ => Token.identifier

    assert(!text.isEmpty)
    Token(tag, spanFrom(start))
  }

  /** Consumes and returns an operator. */
  private def takeOperator(): Token = {
    val start = position
    val text = takeWhile(Lexer.isOperator)

    val tag: Token.Tag = text match
      case "=" => Token.equal
      case "->" => Token.thinArrow
      case "=>" => Token.thickArrow
      case  _ => Token.error

    assert(!text.isEmpty)
    Token(tag, spanFrom(start))
  }

  /** Consumes and returns a punctuation token. */
  private def takePunctuation(): Token = {
    val tag: Token.Tag = peek().get match
      case '[' => Token.leftBracket
      case ']' => Token.rightBracket
      case '(' => Token.leftParenthesis
      case ')' => Token.rightParenthesis
      case '.' => Token.dot
      case ',' => Token.comma
      case ':' => Token.colon
      case  _ => Token.error

    val start = position
    discard()
    Token(tag, spanFrom(start))
  }

  /** Returns a source span covering the range from `start` to the current position. */
  private def spanFrom(start: SourceFile.Index): SourceSpan =
    SourceSpan(start, position, source)

object Lexer:

  /** A regular expression matching newlines. */
  val newline = "\\R".r

  /** Returns `true` iff `c` is a letter or the underscore (i.e., `_`). */
  private def isLetterOrUnderscore(c: Char): Boolean =
    c == '_' || c.isLetter

  /** Returns `true` iff `c` may be part of an operator. */
  private def isOperator(c: Char): Boolean =
    ">=-".contains(c)

end Lexer
