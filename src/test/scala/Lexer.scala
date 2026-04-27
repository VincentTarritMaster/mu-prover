import prover.{Lexer, SourceFile, Token}

final class LexerTests extends munit.FunSuite:

  test("error"):
    val tokens = Lexer(SourceFile("test", "1"))
    assert(tokens.next().map((t) => t.tag == Token.error).getOrElse(false))

  test("bang"):
    val tokens = Lexer(SourceFile("test", "!"))
    assert(tokens.next().map((t) => t.tag == Token.bang).getOrElse(false))

  test("underscore"):
    val tokens = Lexer(SourceFile("test", "_"))
    assert(tokens.next().map((t) => t.tag == Token.underscore).getOrElse(false))

  test("equal"):
    val tokens = Lexer(SourceFile("test", "="))
    assert(tokens.next().map((t) => t.tag == Token.equal).getOrElse(false))

  test("arrows"):
    val tokens = Lexer(SourceFile("test", "-> =>"))
    val found = collect(tokens).map((t) => t.tag)
    val expected = IArray(
      Token.thinArrow,
      Token.thickArrow)
    assert(found.sameElements(expected))

  test("keywords"):
    val tokens = Lexer(SourceFile("test", "fun in let foo _abc"))
    val found = collect(tokens).map((t) => t.tag)
    val expected = IArray(
      Token.fun,
      Token.in,
      Token.let,
      Token.identifier,
      Token.identifier)
    assert(found.sameElements(expected))

  test("punctuation"):
    val tokens = Lexer(SourceFile("test", "[]().,:"))
    val found = collect(tokens).map((t) => t.tag)
    val expected = IArray(
      Token.leftBracket,
      Token.rightBracket,
      Token.leftParenthesis,
      Token.rightParenthesis,
      Token.dot,
      Token.comma,
      Token.colon)
    assert(found.sameElements(expected))

  /** Returns a list with all the tokens left in `tokens`. */
  private def collect(tokens: Lexer): List[Token] =
    def loop(accumulator: List[Token]): List[Token] =
      tokens.next() match
        case Some(t) => loop(t :: accumulator)
        case None => accumulator.reverse
    loop(Nil)

end LexerTests
