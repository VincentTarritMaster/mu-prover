package prover

/** A typing envronment, keeping track of term and type variables having been introduced.
 *
 *  @param terms A mapping from term variable to its type.
 *  @param types A list of type variables having been introduced, in reverse order.
 */
final class Environment private (
    terms: Map[Syntax.TermIdentifier, Type], types: List[Syntax.TypeIdentifier]
):

  /** Returns the type of the variable `x`, if any. */
  def typeOf(n: Syntax.TermIdentifier): Option[Type] =
    terms.get(n)

  /** Returns a copy of `this` in which `n` is assigned to `t`. */
  def introducing(n: Syntax.TermIdentifier, t: Type): Environment =
    new Environment(terms.updated(n, t), types)

  /** Returns the type denoted by the variable `x`, if any. */
  def typeOf(n: Syntax.TypeIdentifier): Option[Type] =
    def loop(ts: List[Syntax.TypeIdentifier], i: Int): Option[Type] =
      ts match
        case x :: xs => if x == n then Some(Type.Variable.Bound(i)) else loop(xs, i + 1)
        case _ => None
    loop(types, 0)

  /** Returns a copy of `this` in which `n` is introduced. */
  def introducing(n: Syntax.TypeIdentifier): Environment =
    new Environment(terms, n :: types)

  override def toString(): String = {
    val builder = StringBuilder("[")

    // Add terms.
    val ts = terms.toArray
    scala.util.Sorting.stableSort(ts, (a, b) => a._1.value < b._1.value)
    builder.append(ts.map((k, v) => s"${k} : ${v}").mkString(", "))

    // Add types.
    if !builder.isEmpty then builder.append(", ")
    builder.append(types.reverse.mkString(", "))

    builder.append("]").result()
  }

object Environment:

  /** Creates an empty environment. */
  def apply(): Environment =
    new Environment(Map(), Nil)

end Environment
