package prover

/** A substitution table, mapping unification variables to their respective assignments and terms
 *  to their respective elaborations.
 *
 *  @param types A mapping from a unification variable to its assignment.
 *  @param elaborations A mapping from a term to its elaboration.
 */
final class Solution private (
    types: Map[Type.Variable.Unification, Type],
    val elaborations: Map[Syntax[Syntax.Term], Syntax[Syntax.Term]]
):

  /** Returns the transitive closure of `this`. */
  def optimized: Solution =
    val builder = Map.newBuilder[Type.Variable.Unification, Type]
    for (t, u) <- types do
      builder.addOne(t -> walk(u))
    new Solution(builder.result(), elaborations)

  /** Returns a copy of `this` where `t` maps to `u`, in which `t` does not occur. */
  def updated(t: Type.Variable.Unification, u: Type): Solution =
    def loop(lhs: Type.Variable.Unification): Solution = types.get(lhs) match
      case Some(v: Type.Variable.Unification) =>
        loop(v)
      case Some(v) =>
        require(walk(u) == walk(v), "invalid assignment")
        this
      case None =>
        val rhs = walk(u)
        require(!rhs.contains(lhs))
        new Solution(types.updated(lhs, rhs), elaborations)
    loop(t)

  /** Returns a copy of `this` recording that `t` elaborates to `u`. */
  def updated(t: Syntax[Syntax.Term], u: Syntax[Syntax.Term]): Solution =
    new Solution(types, elaborations.updated(t, u))

  /** Returns `t` in which every type variable has been replaced by its assignment in `this`. */
  def apply(t: Type): Type =
    t.map(walk)

  /** Returns the elaboration of `t`, or `t` if no such elaboration exists. */
  def apply(t: Syntax[Syntax.Type]): Syntax[Syntax.Type] =
    t.value match
      case n: Syntax.SynthesizedType =>
        Syntax(Syntax.SynthesizedType(this(n.value)), t.span)
      case n: Syntax.Arrow =>
        Syntax(Syntax.Arrow(this(n.from), this(n.to)), t.span)
      case n: Syntax.ForAll =>
        Syntax(n.copy(body = this(n.body)), t.span)
      case _ => t

  /** Returns the elaboration of `t`, or `t` if no such elaboration exists. */
  def elaborated(t: Syntax[Syntax.Term]): Syntax[Syntax.Term] = {
    def elaborate(u: Syntax[Syntax.Term]): Syntax[Syntax.Term] =
      u.value match
        case n: Syntax.TermAbstraction =>
          Syntax(n.copy(body = elaborated(n.body)), u.span)
        case n: Syntax.TermApplication =>
          Syntax(Syntax.TermApplication(elaborated(n.abstraction), elaborated(n.argument)), u.span)
        case n: Syntax.TypeAbstraction =>
          Syntax(n.copy(body = elaborated(n.body)), u.span)
        case n: Syntax.TypeApplication =>
          Syntax(Syntax.TypeApplication(elaborated(n.abstraction), this(n.argument)), u.span)
        case n: Syntax.Ascription =>
          Syntax(n.copy(lhs = elaborated(n.lhs)), u.span)
        case _ => u

    elaborations.get(t) match
      case Some(u) =>
        val s = new Solution(types, elaborations.removed(t))
        s.elaborated(u)
      case None =>
        elaborate(t)
  }

  /** If `t` is a type variable assigned in `this`, returns the type to which it maps, following
   *  each mapping transitively. Otherwise, returns `t`.
   *
   *  If `self` contains `a -> b` and `b -> c`, then `this.walk(a) = c`.
   */
  def walk(t: Type): Type = t match
    case u: Type.Variable.Unification => types.get(u) match
      case Some(v) => walk(v)
      case _ => t
    case _ => t

object Solution:

  /** Creates an empty solution. */
  def apply(): Solution =
    new Solution(Map(), Map())

end Solution
