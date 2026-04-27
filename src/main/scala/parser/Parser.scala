package prover

import Syntax.{Term, Type}

/** The parsing of a source file.
 *
 *  @param source The source file being parsed.
 */
final class Parser private (source: SourceFile):

  /** The tokens in the input. */
  private val tokens = Lexer(source)

  /** The position immediately after the last consumed token. */
  private var position: SourceFile.Index = 0

  /** The next token to consume, if already extracted from the source. */
  private var lookahead: Option[Token] = None

  /** Parses a term expression. */
  private def parseTerm(): Syntax[Term] =
    val head = parseCompoundTerm()
    parseOptionalTrailingAscription() match
      case Some(t) =>
        Syntax(Syntax.Ascription(head, t), spanFrom(head.span.start))
      case None =>
        head

  /** Parses a simple term or an application. */
  private def parseCompoundTerm(): Syntax[Term] = {
    def loop(head: Syntax[Term]): Syntax[Term] =
      if take(Token.leftBracket).isDefined then
        val (ts, _) = parseCommaSeparatedReversed(Token.rightBracket) { () => parseType() }
        expect(Token.rightBracket, "']'")
        val next = ts.foldRight(head) { (t, h) =>
          Syntax(Syntax.TypeApplication(h, t), spanFrom(head))
        }
        loop(next)

      else if !nextIsTerminator() then
        val argument = parseTermAtom()
        loop(Syntax(Syntax.TermApplication(head, argument), spanFrom(head)))

      else
        head

    loop(parseTermAtom())
  }

  /** Parses a simple term. */
  private def parseTermAtom(): Syntax[Term] =
    peek().map((t) => t.tag) match
      case Some(Token.bang) =>
        parseAssertion()
      case Some(Token.underscore) =>
        parseElidedTerm()
      case Some(Token.identifier) =>
        parseIdentifier(Syntax.TermIdentifier.apply)
      case Some(Token.leftBracket) =>
        parseTypeAbstraction()
      case Some(Token.leftParenthesis) =>
        parseAbstractionOrParenthesizedTerm()
      case Some(Token.let) =>
        parseBinding()
      case _ =>
        throw expected("term")

  /** Parses an assertion. */
  private def parseAssertion(): Syntax[Term] =
    val head = expect(Token.bang, "'!'")
    Syntax(Syntax.Assertion, head.span)

  /** Parses an elided term. */
  private def parseElidedTerm(): Syntax[Term] =
    val head = expect(Token.underscore, "'!'")
    Syntax.summon(head.span)

  /** Parses a type abstraction. */
  private def parseTypeAbstraction(): Syntax[Term] = {
    // '[' identifier (',' identifier)* ','? ']'
    val opener = expect(Token.leftBracket, "'['")
    val (ps, trailingComma) = parseCommaSeparatedReversed(Token.rightBracket) { () =>
      parseIdentifier(Syntax.TypeIdentifier.apply)
    }
    val closer = expect(Token.rightBracket, "']'")

    // '=>' term
    expect(Token.thickArrow, "'=>'")
    ps.foldLeft(parseTerm()) { (b, p) =>
      Syntax(Syntax.TypeAbstraction(p, b), spanFrom(opener))
    }
  }

  /** Parses a term abstraction or a parenthesized term. */
  private def parseAbstractionOrParenthesizedTerm(): Syntax[Term] = {
    val opener = expect(Token.leftParenthesis, "'('")
    def span = spanFrom(opener)

    // term (':' type)?
    val x = parseCompoundTerm()
    val a = parseOptionalTrailingAscription()

    def parenthesized(): Syntax[Term] = a match
      case Some(t) => Syntax(Syntax.Ascription(x, t), span)
      case _ => x

    // If the term is a simple identifier, we may be parsing an abstraction.
    if x.value.isInstanceOf[Syntax.TermIdentifier] then
      val p = x.asInstanceOf[Syntax[Syntax.TermIdentifier]]

      // If we parse a comma, commit to parsing an abstraction.
      if take(Token.comma).isDefined then
        val (ps, _) = parseCommaSeparatedReversed(Token.rightParenthesis) { () =>
          val i = parseIdentifier(Syntax.TermIdentifier.apply)
          val t = parseOptionalTrailingAscription()
          (i, t)
        }
        expect(Token.rightParenthesis, "')'")

        // '=>' term
        expect(Token.thickArrow, "'=>'")
        val rest = ps.foldLeft(parseTerm()) { (b, q) =>
          Syntax(Syntax.TermAbstraction(q._1, Syntax.Type.orElided(q._2, q._1.span), b), span)
        }
        Syntax(Syntax.TermAbstraction(p, Syntax.Type.orElided(a, p.span), rest), span)

      // Otherwise, parse a right parenthesis and look at the next token to decide.
      else
        expect(Token.rightParenthesis, "')'")
        if take(Token.thickArrow).isDefined then
          val body = parseTerm()
          Syntax(Syntax.TermAbstraction(p, Syntax.Type.orElided(a, p.span), body), span)
        else
          parenthesized()

    // Otherwise, check if we've parsed an ascription or a bare term.
    else
      expect(Token.rightParenthesis, "')'")
      parenthesized()
  }

  /** Parses a binding expression (e.g., `let x : T = y in z`). */
  private def parseBinding(): Syntax[Term] = {
    val opener = expect(Token.let, "'let'")

    // identifier (':' type)?
    val p = parseIdentifier(Syntax.TermIdentifier.apply)
    val a = parseOptionalTrailingAscription()

    // '=' term 'in' term
    expect(Token.equal, "'='")
    val v = parseTerm()
    expect(Token.in, "'in'")
    val b = parseTerm()

    // Desugar the binding.
    val span = spanFrom(opener)
    val abstraction = Syntax(Syntax.TermAbstraction(p, Syntax.Type.orElided(a, p.span), b), span)
    Syntax(Syntax.TermApplication(abstraction, v), span)
  }

  /** Parses a trailing type ascription (e.g., `: T`) iff the next token is a colon. */
  private def parseOptionalTrailingAscription(): Option[Syntax[Type]] =
    take(Token.colon).map(_ => parseType())

  /** Parses a type expression. */
  private def parseType(): Syntax[Type] =
    val lhs = parseTypeAtom()
    if take(Token.thinArrow).isDefined then
      val rhs = parseType()
      Syntax(Syntax.Arrow(lhs, rhs), spanFrom(lhs))
    else
      lhs

  /** Parses a simple type expression. */
  private def parseTypeAtom(): Syntax[Type] =
    peek().map((t) => t.tag) match
      case Some(Token.identifier) =>
        parseIdentifier(Syntax.TypeIdentifier.apply)
      case Some(Token.underscore) =>
        parseElidedType()
      case Some(Token.leftBracket) =>
        parseForAll()
      case Some(Token.leftParenthesis) =>
        parseUnitParenthesizedType()
      case _ =>
        throw expected("type")

  /** Parses an elided term. */
  private def parseElidedType(): Syntax[Type] =
    val head = expect(Token.underscore, "'!'")
    Syntax(Syntax.ElidedType, head.span)

  /** Parses a universal type. */
  private def parseForAll(): Syntax[Type] = {
    // '[' identifier (',' identifier)* ','? ']'
    val opener = expect(Token.leftBracket, "'['")
    val (ps, trailingComma) = parseCommaSeparatedReversed(Token.rightBracket) { () =>
      parseIdentifier(Syntax.TypeIdentifier.apply)
    }
    val closer = expect(Token.rightBracket, "']'")
    val span = spanFrom(opener)

    // '=>' type
    expect(Token.thickArrow, "'=>'")
    ps.foldLeft(parseType()) { (b, p) =>
      Syntax(Syntax.ForAll(p, b), span)
    }
  }

  /** Parses a parenthesized type expression. */
  private def parseUnitParenthesizedType(): Syntax[Type] = {
    val opener = expect(Token.leftParenthesis, "'('")

    if take(Token.rightParenthesis).isDefined then
      Syntax(Syntax.UnitType, spanFrom(opener))
    else
      val enclosed = parseType()
      expect(Token.rightParenthesis, "')'")
      enclosed
  }

  /** Parses a simple identifier.
   *
   *  @param make A closure that accepts a string and returns an instance of `T`, which is the
   *    payload of the syntax tree parsed by this method.
   */
  private def parseIdentifier[T <: Syntax.Identifier](make: String => T): Syntax[T] =
    val t = expect(Token.identifier, "identifier")
    Syntax(make(t.text.toString()), t.span)

  /** Parses a list of instance of `T` separated by commas, returning the instances parsed in
   *  reverse order along with the trailing comma, if any.
   *
   *  The method parses the longest sequence until either a token having tag `rightDelimiter` has
   *  been reached or all tokens of the input have been consumed. An error is reported if a comma
   *  is missing between two instances of `T`.
   *
   *  @param isRightDelimiter A closure that accepts a token and returns whether it delimits the
   *    end of the comma separated list.
   *  @param parseTree A closure that parses a single instance of `T`.
   */
  private def parseCommaSeparatedReversed[T](
      rightDelimiter: Token.Tag
  )(parseTree: () => T): (List[T], Option[Token]) =
    def loop(accumulator: List[T], lastComma: Option[Token]): (List[T], Option[Token]) =
      peek() match
        case Some(head) if head.tag != rightDelimiter =>
          if !accumulator.isEmpty && !lastComma.isDefined then throw expected("','")
          loop(parseTree() :: accumulator, take(Token.comma))
        case _ =>
          (accumulator, lastComma)
    loop(Nil, None)

  /** Parses an instance of `T` in parentheses, using `parseTree` to parse the instance. */
  private def parseParenthesized[T](parseTree: () => T): (Token, T, Token) =
    val s = expect(Token.leftParenthesis, "'('")
    val t = parseTree()
    val e = expect(Token.leftParenthesis, "')'")
    (s, t, e)

  /// Returns `true` iff the next token has tag `k`, without consuming that token.
  private def nextIs(k: Token.Tag): Boolean =
    peek().map((t) => t.tag == k).getOrElse(false)

  private def nextIsTerminator(): Boolean =
    peek() match
      case Some(t) =>
        t.isAnyOf(Token.rightBracket, Token.rightParenthesis, Token.colon, Token.in)
      case _ =>
        true

  /** Returns the next token without consuming it. */
  private def peek(): Option[Token] =
    if !lookahead.isDefined then lookahead = tokens.next()
    lookahead

  /** Consumes and returns the next token. */
  private def take(): Option[Token] =
    val head = lookahead.orElse(tokens.next())
    lookahead = None
    position = head.map((t) => t.span.end).getOrElse(tokens.source.end)
    head

  /** Consumes and returns the next token iff it has tag `k`. */
  private def take(k: Token.Tag): Option[Token] =
    if nextIs(k) then take() else None

  /** Parses a token with tag `k`, which is described by `s`. */
  private def expect(k: Token.Tag, s: String): Token =
    take(k) match
      case Some(t) => t
      case None => throw expected(s)

  /** Returns a source span from `s` to the current position. */
  private def spanFrom(s: SourceFile.Index): SourceSpan =
    SourceSpan(s, position, tokens.source)

  /** Returns a source span from the first position of `t` to the current position. */
  private def spanFrom(t: Token): SourceSpan =
    spanFrom(t.span.start)

  /** Returns a source span from the start of `n`'s span to the current position. */
  private def spanFrom[T](n: Syntax[T]): SourceSpan =
    spanFrom(n.span.start)

  /** Returns a parse error reporting that `s` was expected at `site`. */
  private def expected(s: String, site: SourceSpan): Diagnostic =
    Diagnostic(s"expected ${s}", site)

  /** Returns a parse error reporting that `s` was expected at the current position. */
  private def expected(s: String): Diagnostic =
    expected(s, SourceSpan(position, position, tokens.source))

object Parser:

  /** Parses and returns the program in `source`. */
  def parse(source: SourceFile): Syntax[Term] =
    val parser = Parser(source)
    parser.parseTerm()

end Parser
