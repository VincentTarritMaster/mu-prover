package prover

import collection.immutable.ArraySeq
import collection.mutable.ArrayBuilder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** A source file.
 *
 *  @param name The name of the file.
 *  @param text The contents of the file.
 */
final class SourceFile(val name: String, val text: String):

  /** The start position of each line. */
  val lineStarts: ArraySeq[SourceFile.Index] = SourceFile.lineBoundaries(text)

  /** Returns the 1-based line and column numbers corresponding to `i`.
    *
    * - Requires: `i` is a valid index in `contents`.
    * - Complexity: O(log N) + O(C) where N is the number of lines in `this` and C is the returned
    *   column number.
    */
  def lineAndColumn(i: SourceFile.Index): SourceFile.LineAndColumn =
    val l = lineContaining(i)
    val c = (i - lineStarts(l - 1)) + 1
    SourceFile.LineAndColumn(l, c)

  /** Returns the line containing `i`. */
  def lineContaining(i: SourceFile.Index): Int =
    lineStarts.partitioningIndexWhere((l) => l > i)

  /** Returns the contents of the line in `this` at 1-based index `i`. */
  def lineContents(i: Int): CharSequence =
    val s = lineStarts(i - 1)
    val e = if i < lineStarts.length then lineStarts(i) else end
    text.subSequence(s, e)

  /** Returns the contents of `this` covered by `s`. */
  def apply(s: SourceSpan): CharSequence =
    text.subSequence(s.start, s.end)

  /** Returns the index immediately after the last character in the file. */
  def end: SourceFile.Index =
    text.length

  /** Returns a span covering the whole contents of `this`. */
  def span: SourceSpan =
    SourceSpan(0, end, this)

  /** Returns a hash of the salient parts of `this`. */
  override def hashCode: Int =
    name.hashCode ^ text.hashCode

  /** Returns `true` iff `this` is equal to `other`. */
  override def equals(that: Any): Boolean =
    that match
      case other: SourceFile => (this.name == other.name) && (this.text == other.text)
      case _ => false

object SourceFile:

  /** The position of a character in a source file. */
  type Index = Int

  /** The 1-based line and column indices of a character in a source file. */
  case class LineAndColumn(line: Int, column: Int)

  /** Returns the 0-based indices of the start of each line in `text`, in order. */
  def lineBoundaries(text: String): ArraySeq[SourceFile.Index] = {
    val builder = ArrayBuilder.ofInt()
    builder.addOne(0)

    def loop(i: Int): Unit =
      Lexer.newline.findFirstMatchIn(text.subSequence(i, text.length)) match
        case Some(m) => builder.addOne(i + m.end) ; loop(i + m.end)
        case None => ()
    loop(0)

    ArraySeq.unsafeWrapArray(builder.result())
  }

  /** Creates a source file with the contents at `filepath`. */
  def contentsOf(filepath: String): SourceFile =
    val text = Files.readString(Paths.get(filepath), StandardCharsets.UTF_8)
    SourceFile(filepath, text)

end SourceFile
