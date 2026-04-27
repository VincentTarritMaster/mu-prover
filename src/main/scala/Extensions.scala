package prover

/** Returns the start of the partition in `self` whose elements satisfy `isOnRHS`.
 *
 *  `self` must already be partitioned according `isOnRHS`. That is, there exists `i` such that
 *  `self.drop(i).forall(isOnRHS)` and `isOnRHS(self(j))`for any positive `j` less than `i`.
 *
 *   - Complexity: O(log N) where N is the lenght of `this`.
 */
extension [T](self: IndexedSeq[T]) def partitioningIndexWhere(isOnRHS: T => Boolean): Int =
  def find(l: Int, n: Int): Int =
    if n == 0 then l else
      val h = n / 2
      val m = l + h
      if isOnRHS(self(m)) then find(l, h) else find(m + 1, n - (h + 1))
  find(0, self.length)

/** Returns `self` enclosed in parentheses iff `self` contains a space and is not already some
 *  parenthesized substring. Otherwise, returns `self`. */
extension (self: String) def disambuiguated: String = {
    val l = self.length

    def loop(i: Int, opened: Int): String =
      if i == l then
        if (opened == 0) then self else s"(${self})"
      else if self(i) == '(' then
        if (opened > 0) || (i == 0) then loop(i + 1, opened + 1) else s"(${self})"
      else if self(i) == ')' then
        if (opened > 0) then loop(i + 1, opened - 1) else s"(${self})"
      else if self(i) == ' ' then
        if (opened > 0) then loop(i + 1, opened) else s"(${self})"
      else if self(i).isLetterOrDigit || (self(i) == '%') || (self(i) == '!') then
        loop(i + 1, opened)
      else
        s"(${self})"

    loop(0, 0)
  }
