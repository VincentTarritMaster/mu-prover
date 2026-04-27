package prover

/** A node in an abstract syntax tree.
 *
 *  @param value The payload of the tree.
 *  @param span The site from which the tree was parsed.
 */
final class Syntax[+T](val value: T, val span: SourceSpan):

  /** Returns a textual representation of this tree. */
  override def toString(): String =
    value.toString()

object Syntax:

  /** Returns an expression of the "summon" operator at `span`. */
  def summon(span: SourceSpan): Syntax[TypeAbstraction] =
    val x = Syntax(TermIdentifier("x"), span)
    val a = Syntax(TypeIdentifier("α"), span)
    Syntax(TypeAbstraction(a, Syntax(TermAbstraction(x, a, x), span)), span)

  /** A simple identifier, which may bethe payload of a term or a type. */
  sealed trait Identifier:

    /** The value of this identifier. */
    def value: String

    override def toString(): String =
      value

  end Identifier

  /** The payload of a syntax tree representing a term. */
  sealed trait Term

  /** A term identifier. */
  case class TermIdentifier(value: String) extends Identifier with Term

  /** A term abstraction. */
  case class TermAbstraction(
      parameter: Syntax[TermIdentifier], ascription: Syntax[Type], body: Syntax[Term]
  ) extends Term:

    override def toString(): String =
      s"(${parameter} : ${ascription}) => $body"

  end TermAbstraction

  /** A term application. */
  case class TermApplication(
      abstraction: Syntax[Term], argument: Syntax[Term]
  ) extends Term:

    override def toString(): String =
      s"${abstraction.toString.disambuiguated} ${argument.toString.disambuiguated}"

  end TermApplication

  /** A type abstraction. */
  case class TypeAbstraction(
      parameter: Syntax[TypeIdentifier], body: Syntax[Term]
  ) extends Term:

    override def toString(): String =
      s"[${parameter}] => ${body}"

  end TypeAbstraction

  /** A type application. */
  case class TypeApplication(
      abstraction: Syntax[Term], argument: Syntax[Type]
  ) extends Term:

    override def toString(): String =
      s"${abstraction.toString.disambuiguated} [${argument}]"

  end TypeApplication

  /** A type ascription. */
  case class Ascription(
      lhs: Syntax[Term], rhs: Syntax[Type]
  ) extends Term:

    override def toString(): String =
      s"${lhs.toString.disambuiguated} : ${rhs.toString.disambuiguated}"

  end Ascription

  /** An assertion (e.g., `!`). */
  case object Assertion extends Term:

    override def toString(): String =
      "!"

  end Assertion

  /** The payload of a syntax tree representing a type. */
  sealed trait Type

  object Type:

    /** Returns `tree` if it is defined or an elided type at `span` otherwise. */
    def orElided(tree: Option[Syntax[Type]], span: SourceSpan): Syntax[Type] =
      tree match
        case Some(t) => t
        case None => Syntax(ElidedType, span)

  end Type

  /** A type identifier. */
  case class TypeIdentifier(value: String) extends Identifier with Type

  case class SynthesizedType(value: prover.Type) extends Type:

    override def toString(): String =
      s"<${value}>"

  end SynthesizedType

  /** The type of a function. */
  case class Arrow(
      from: Syntax[Type], to: Syntax[Type]
  ) extends Type:

    override def toString(): String =
      s"${from.toString.disambuiguated} -> ${to.toString.disambuiguated}"

  end Arrow

  /** The type of a type abstraction. */
  case class ForAll(
      parameter: Syntax[TypeIdentifier], body: Syntax[Type]
  ) extends Type:

    override def toString(): String =
      s"[${parameter}] => ${body}"

  end ForAll

  /** The unit type. */
  case object UnitType extends Type:

    override def toString(): String =
      "()"

  end UnitType

  /** An elided type, left to be inferred. */
  case object ElidedType extends Type:

    override def toString(): String =
      "_"

  end ElidedType

end Syntax
