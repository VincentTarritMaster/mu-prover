package prover

import collection.mutable

/** The type checking of a program. */
final class Typer private ():

  /** A set of constraints collected during the visit of the syntax tree. */
  private val constraints = mutable.ArrayBuffer[Typer.Constraint]()

  /** The identity of the next fresh unification variable. */
  private var nextVariableIdentifier = 0

  /** Returns the inferred type of `syntax`, reading the typeof variables from `environment`.
   *
   *  This method visits `syntax` recursively and records type constraints based on the structure
   *  of the term. These constraints are facts that must hold for `syntax` to be well-typed. They
   *  are discharged by `solveConstraints` which, if successful, returns a substitution table
   *  assigning each unification variable created by this method.
   */
  private def visit(
      syntax: Syntax[Syntax.Term], expected: Option[Type], environment: Environment
  ): Type = {
    
    syntax.value match

    case ti: Syntax.TermIdentifier =>
      val tpe = environment.typeOf(ti).getOrElse {
        throw Diagnostic(s"unbound variable: ${ti.value}", syntax.span)
      }
      expected.foreach(constrain(syntax, tpe, _))
      tpe

    case Syntax.TermAbstraction(param, ascription, body) =>
      val paramType = visit(ascription, environment)
      val newEnv = environment.introducing(param.value, paramType)
      val bodyType = visit(body, None, newEnv)

      val funType = Type.Arrow(paramType, bodyType)
      expected.foreach(constrain(syntax, funType, _))
      funType

    case Syntax.TermApplication(fun, arg) =>
      val funType = visit(fun, None, environment)
      val argType = visit(arg, None, environment)

      val resultType = Type.Unit

      constrain(syntax, funType, Type.Arrow(argType, resultType))
      expected.foreach(constrain(syntax, resultType, _))

      resultType

    case Syntax.Ascription(term, tpeSyntax) =>
      val tpe = visit(tpeSyntax, environment)
      val termType = visit(term, Some(tpe), environment)

      constrain(term, termType, tpe)
      expected.foreach(constrain(syntax, tpe, _))

      tpe

    case Syntax.TypeAbstraction(typeParam, body) =>
      val newEnv = environment.introducing(typeParam.value)
      val bodyType = visit(body, None, newEnv)

      val result = Type.ForAll(bodyType)
      expected.foreach(constrain(syntax, result, _))
      result

    case Syntax.TypeApplication(term, tpeSyntax) =>
      val funType = visit(term, None, environment)
      val argType = visit(tpeSyntax, environment)

      funType match
        case fa @ Type.ForAll(_) =>
          val instantiated = fa(argType)        
          expected.foreach(constrain(syntax, instantiated, _))
          instantiated

        case _ =>
          val resultType = fresh()
          constrain(syntax, funType, Type.ForAll(resultType))
          resultType

    case Syntax.Assertion =>
      expected.foreach(constrain(syntax, Type.Unit, _))
      Type.Unit
  }

  /** Returns the type of `syntax`, reading the type of variables from `environment`. */
  private def visit(
    syntax: Syntax[Syntax.Type],
    environment: Environment
  ): Type = {
    (syntax.value: Syntax.Type) match
      case Syntax.UnitType =>
        Type.Unit

      case ti: Syntax.TypeIdentifier =>
        environment.typeOf(ti).getOrElse {
          throw Diagnostic.undefinedSymbol(ti, syntax.span)
        }

      case Syntax.Arrow(from, to) =>
        Type.Arrow(
          visit(from, environment),
          visit(to, environment)
        )

      case Syntax.ForAll(param, body) =>
        val newEnv = environment.introducing(param.value)
        Type.ForAll(visit(body, newEnv))

      case Syntax.SynthesizedType(t) =>
        t

      case Syntax.ElidedType =>
        fresh()
  }

  /** Solves the constraints that have been recorded by `visit` and returns a substitution table
   *  assigning the unification variables that were introduced.
   */
  private def solveConstraints(): Solution = {
    Solution()
  }

  /** Returns a substitution table `σ` such that `σ(lhs) = σ(rhs)`, or `None` if no such
   *  substitution table can be constructed. */
  private def unify(
      lhs: Type, rhs: Type, solution: Solution
  ): Option[Solution] = {
    ???
  }

  /** Inserts a constraint stating that `found`, which is the inferred type of `term`, must be
   *  coercible to `expected`.
   *
   *  No constraint is inserted if `found` is equal to `expected`.
   */
  private def constrain(term: Syntax[Syntax.Term], found: Type, expected: Type): Unit =
    if found != expected then
      constraints.append(Typer.Constraint(term, found, expected))

  /** Returns a fresh variable identifier. */
  private def fresh(): Type.Variable.Unification =
    val variable = Type.Variable.Unification(nextVariableIdentifier)
    nextVariableIdentifier += 1
    variable

object Typer:

  /** A map from a term to its coercion, as found during type inference. */
  private type Coercions = List[(Syntax[Syntax.Term], Syntax[Syntax.Term])]

  /** A constraint specifying that `found`, which is the inferred type of `term`, must be equal or
   *  coercible to `expected`.
   */
  private case class Constraint(term: Syntax[Syntax.Term], found: Type, expected: Type):

    override def toString(): String =
      s"${found} = ${expected}"

  end Constraint

  /** Returns a pair (`x`, `t`) where `t` is the type of `program` and `x` is its elaboration. */
  def check(program: Syntax[Syntax.Term]): (Syntax[Syntax.Term], Type) =
    val typer = Typer()
    val t = typer.visit(program, None, Environment())
    val s = typer.solveConstraints().optimized
    (s.elaborated(program), s(t))

end Typer
