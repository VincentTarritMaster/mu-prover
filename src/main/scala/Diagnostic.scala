package prover

/** An error that occurred during the processing of a program.
 *
 *  @param description A description of the error that occured.
 *  @param span The source code identified as the cause of the error.
 */
case class Diagnostic(description: String, span: SourceSpan) extends Error

object Diagnostic:

  /** Creates an instance diagnosing an undefined symbol `name` at `span`. */
  def undefinedSymbol(name: Syntax.Identifier, span: SourceSpan): Diagnostic =
    Diagnostic(s"undefined symbol: '${name.value}'", span)

end Diagnostic
