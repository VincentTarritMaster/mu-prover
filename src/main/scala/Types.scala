package prover

import collection.mutable

/** The tree representation of a type. */
sealed trait Type:

  /** Returns `true` iff `this` is not a universal type. */
  def isSimple: Boolean =
    !this.isInstanceOf[Type.ForAll]

  /** Returns `this` recursively transformed by `transform`.
   *
   *  The structure of `this` is visited in pre-order. `transform` is applied after all children
   *  of a type tree have been visited. As a consequence, `map` is always applied on transformed
   *  children. For example, if `this` is an arrow type `Arrow(T, U)`, then `map(this)` evaluates
   *  to `transform(Arrow(transform(T), transform(T)))`.
   */
  def map(transform: Type => Type): Type =
    transform(this)

  /** Returns `true` iff `that` occurs in `this`. */
  def contains(that: Type.Variable.Unification): Boolean =
    false

object Type:

  /** The unit type. */
  case object Unit extends Type:

    override def toString(): String =
      "()"

  end Unit

  /** The type of a function. */
  case class Arrow(from: Type, to: Type) extends Type:

    override def map(transform: Type => Type): Type =
      transform(Arrow(transform(from), transform(to)))

    override def contains(that: Type.Variable.Unification): Boolean =
      from.contains(that) || to.contains(that)

    override def toString(): String =
      val t = from.toString.disambuiguated
      val u = to.toString.disambuiguated
      s"${t} -> ${u}"

  end Arrow

  /** The type of a type abstraction. */
  case class ForAll(body: Type) extends Type:

    /** Returns the body of `this` in which occurrences of the type variable introduced by `this`
     *  have been replaced by `argument`.
     *
     *  For example, if `this` is `Λ.α0 -> U`, then `this(T)` is `T -> U`.
     */
    def apply(argument: Type): Type =
      body.map { (t) => t match
        case Variable.Bound(i) => if i == 0 then argument else Variable.Bound(i - 1)
        case _ => t
      }

    override def map(transform: Type => Type): Type =
      transform(ForAll(transform(body)))

    override def contains(that: Type.Variable.Unification): Boolean =
      body.contains(that)

    override def toString(): String =
      def loop(builder: StringBuilder, b: Type): String = b match
        case t: ForAll =>
          loop(builder.append("Λ"), t.body)
        case _ =>
          builder.append(s".${b}").result()
      loop(StringBuilder(), this)

  end ForAll

  /** A type variable.
   *
   *  There are two forms of type variables: bound variables and unification variables. The former
   *  denote variables introduced by a binder (e.g., `T` in `[T] => T`) whereas the latter denote
   *  free variables, open for unification (e.g., `T` in `() => T`).
   *
   *  Bound variables are represented using De Brujin indices and unification variables are
   *  identified by a unique number.
   */
  sealed trait Variable extends Type

  object Variable:

    /** A type variable introduced by a bindinder. */
    case class Bound(id: Int) extends Variable:

      override def toString(): String =
        s"α${id}"

    end Bound

    /** A type variable occurring free. */
    case class Unification(id: Int) extends Variable:

      override def contains(that: Type.Variable.Unification): Boolean =
        this == that

      override def toString(): String =
        s"%${id}"

    end Unification

  end Variable

end Type
