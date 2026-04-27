package prover

/** A terminal symbol of the syntactic grammar.
 *
 *  @param tag The tag of the token.
 *  @param span The site from which the token was extracted.
 */
case class Token(tag: Token.Tag, span: SourceSpan):

  /** Returns the text from which this token was extracted. */
  def text: CharSequence =
    span.text

  /** Returns `true` if the tag of this token is contained in `ks`. */
  def isAnyOf(ks: Token.Tag*): Boolean =
    ks.contains(tag)

object Token:

  /** The tag of a token. */
  opaque type Tag = Byte

  val error             : Tag = 0x00
  val underscore        : Tag = 0x01
  val identifier        : Tag = 0x02
  val fun               : Tag = 0x03
  val in                : Tag = 0x04
  val let               : Tag = 0x05
  val equal             : Tag = 0x06
  val thinArrow         : Tag = 0x07
  val thickArrow        : Tag = 0x08
  val dot               : Tag = 0x09
  val comma             : Tag = 0x0a
  val colon             : Tag = 0x0b
  val bang              : Tag = 0x0c
  val leftBracket       : Tag = 0x0d
  val rightBracket      : Tag = 0x0e
  val leftParenthesis   : Tag = 0x0f
  val rightParenthesis  : Tag = 0x10

  /** Returns a closure that accepts a token and returns `true` iff that token has tag `k`. */
  def hasTag(k: Tag): Token => Boolean =
    (t: Token) => t.tag == k

end Token
