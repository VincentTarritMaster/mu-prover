package prover

import java.io.FileNotFoundException
import scala.io.AnsiColor.{BOLD, GREEN, RED, RESET}

/** Returns a source file with the contents of the file at `filepath`, or reports a failure on
 *  the standard error and returns `None` if the file couldn't be read.
 */
def source(filepath: String): Option[SourceFile] =
  try Some(SourceFile.contentsOf(filepath))
  catch
    case e: FileNotFoundException => error(None, s"no such file: ${e.getMessage()}") ; None
    case e => error(None, e.toString) ; None

/** Logs a success to the standard output. */
def success(message: String): Unit =
  println(s"${GREEN}${BOLD}success:${RESET} ${message}")

/** Logs an error to the standard error at `location`. */
def error(location: Option[SourceSpan], message: String): Unit =
  location match
    case Some(l) =>
      System.err.println(s"${l}: ${RED}${BOLD}error:${RESET} ${message}")
    case _ =>
      System.err.println(s"${RED}${BOLD}error:${RESET} ${message}")

/** Logs a diagnostic to the standard error. */
def render(d: Diagnostic): Unit = {
  // Log the message of the diagnostic.
  error(Some(d.span), d.description)

  // Log the line at which the diagnostic occurred.
  val source = d.span.source
  val text = source.lineContents(source.lineContaining(d.span.start) )
  System.err.println(text)

  // Log the column atwhich the diansotic occurred.
  val location = source.lineAndColumn(d.span.start)
  System.err.print(" " * (location.column - 1))
  System.err.println("^")
}

/** The entry point of the application.
 *
 *  @param filepath The path to the input file.
 */
@main def Main(filepath: String): Unit =
  source(filepath).map { (s) =>
    try
      val program = Parser.parse(s)
      success(program.toString)
      // val (elaborated, result) = Typer.check(program)
      // success(s"${elaborated.toString.disambuiguated} : ${result}")
    catch
      case e: Diagnostic => render(e)
  }
