package prover

/** A range of positions in a source file.
 *
 *  @param start The lower bound of the range.
 *  @param end The non-inclusive upper bound of the range.
 */
case class SourceSpan(start: SourceFile.Index, end: SourceFile.Index, source: SourceFile):

  /** The text covered by this span. */
  def text: CharSequence =
    source(this)

  /** Returns a textual representation of `this`. */
  override def toString(): String = {
    val s = source.lineAndColumn(start)
    val o = StringBuilder(s"${source.name}:${s.line}.${s.column}")

    if start != end then
      val e = source.lineAndColumn(end - 1)
      if s.line == e.line then
        o.append(s"-${e.column}")
      else
        o.append(s"-${e.line}.${e.column}")

    o.result()
  }

end SourceSpan
