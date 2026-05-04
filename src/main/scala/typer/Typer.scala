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
      expected.foreach(constrain(syntax, tpe, _, environment))
      tpe

    case Syntax.TermAbstraction(param, ascription, body) =>
      val paramType = visit(ascription, environment)
      val newEnv = environment.introducing(param.value, paramType)
      val bodyType = visit(body, None, newEnv)

      val funType = Type.Arrow(paramType, bodyType)
      expected.foreach(constrain(syntax, funType, _, environment))
      funType

    case Syntax.TermApplication(fun, arg) =>
      val funType = visit(fun, None, environment)
      val argType = visit(arg, None, environment)

      val resultType = fresh()

      constrain(syntax, funType, Type.Arrow(argType, resultType), environment)
      expected.foreach(constrain(syntax, resultType, _, environment))

      resultType

    case Syntax.Ascription(term, tpeSyntax) =>
      val tpe = visit(tpeSyntax, environment)
      val termType = visit(term, Some(tpe), environment)

      constrain(term, termType, tpe, environment)
      expected.foreach(constrain(syntax, tpe, _, environment))

      tpe

    case Syntax.TypeAbstraction(typeParam, body) =>
      val newEnv = environment.introducing(typeParam.value)
      val bodyType = visit(body, None, newEnv)

      val result = Type.ForAll(bodyType)
      expected.foreach(constrain(syntax, result, _, environment))
      result

    case Syntax.TypeApplication(term, tpeSyntax) =>
      val funType = visit(term, None, environment)
      val argType = visit(tpeSyntax, environment)

      funType match
        case fa @ Type.ForAll(_) =>
          val instantiated = fa(argType)        
          expected.foreach(constrain(syntax, instantiated, _, environment))
          instantiated

        case _ =>
          val resultType = fresh()
          constrain(syntax, funType, Type.ForAll(resultType), environment)
          resultType

    case Syntax.Assertion =>
      expected.foreach(constrain(syntax, Type.Unit, _, environment))
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
    constraints.foldLeft(Solution()) { (solution, constraint) =>
      unify(constraint.found, constraint.expected, solution)
        .orElse(coerce(constraint, solution))
        .getOrElse {
          val found = solution(constraint.found)
          val expected = solution(constraint.expected)
          throw Diagnostic(s"type mismatch: found ${found}, expected ${expected}", constraint.term.span)
        }
    }
  }

  /** Returns a substitution table `σ` such that `σ(lhs) = σ(rhs)`, or `None` if no such
   *  substitution table can be constructed. */
  private def unify(
      lhs: Type, rhs: Type, solution: Solution
  ): Option[Solution] = {
    val left = solution.walk(lhs)
    val right = solution.walk(rhs)

    if left == right then
      Some(solution)
    else
      (left, right) match
        case (u: Type.Variable.Unification, t) =>
          if t.contains(u) then None else Some(solution.updated(u, t))

        case (t, u: Type.Variable.Unification) =>
          if t.contains(u) then None else Some(solution.updated(u, t))

        case (Type.Arrow(lf, lt), Type.Arrow(rf, rt)) =>
          unify(lf, rf, solution).flatMap(unify(lt, rt, _))

        case (Type.ForAll(lb), Type.ForAll(rb)) =>
          unify(lb, rb, solution)

        case _ =>
          None
  }

  /** Attempts to elaborate `constraint.term` into a term whose type is `constraint.expected`. */
  private def coerce(
      constraint: Typer.Constraint, solution: Solution
  ): Option[Solution] = {
    def candidate(
        term: Syntax[Syntax.Term], found: Type, expected: Type, state: Solution, fuel: Int
    ): Option[(Syntax[Syntax.Term], Solution)] =
      if fuel < 0 then
        None
      else
        unify(found, expected, state).map(term -> _).orElse {
          state.walk(found) match
            case Type.Arrow(from, to) =>
              inhabit(from, state, fuel - 1, false).flatMap { (argument, s0) =>
                val applied = Syntax(Syntax.TermApplication(term, argument), term.span)
                candidate(applied, to, expected, s0, fuel - 1)
              }

            case fa @ Type.ForAll(_) =>
              val argumentType = fresh()
              val instantiated = fa(argumentType)
              val applied = Syntax(
                Syntax.TypeApplication(term, Syntax(Syntax.SynthesizedType(argumentType), term.span)),
                term.span
              )
              candidate(applied, instantiated, expected, state, fuel - 1)

            case _ =>
              None
        }

    def inhabit(
        expected: Type, state: Solution, fuel: Int, allowOriginalTerm: Boolean
    ): Option[(Syntax[Syntax.Term], Solution)] =
      if fuel < 0 then
        None
      else
        (if allowOriginalTerm then
           candidate(constraint.term, constraint.found, expected, state, fuel)
         else
           None
        ).orElse {
          constraint.environment.termBindings.iterator.flatMap { (name, tpe) =>
            val identifier = Syntax(name, constraint.term.span)
            candidate(identifier, tpe, expected, state, fuel)
          }.toSeq.headOption
        }
          .orElse {
            constraint.term.value match
              case Syntax.Assertion =>
                Some(constraint.term -> state)
              case _ =>
                None
          }

    val fuel = constraint.environment.termBindingCount + 2
    inhabit(constraint.expected, solution, fuel, true).map { (term, state) =>
      state.updated(constraint.term, term)
    }
  }

  /** Inserts a constraint stating that `found`, which is the inferred type of `term`, must be
   *  coercible to `expected`.
   *
   *  No constraint is inserted if `found` is equal to `expected`.
   */
  private def constrain(
      term: Syntax[Syntax.Term], found: Type, expected: Type, environment: Environment
  ): Unit =
    if found != expected then
      constraints.append(Typer.Constraint(term, found, expected, environment))

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
  private case class Constraint(
      term: Syntax[Syntax.Term], found: Type, expected: Type, environment: Environment
  ):

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
