object P06 {
  def isPalindromeHelper[A](l:List[A], r:List[A]):Boolean = (l, r) match {
    case (Nil, Nil)       => true
    case (hl::tl, hr::tr) =>
      if (hl == hr)
        isPalindromeHelper(tl, tr)
      else
        false
    case (_, _)           => false
  }

  def isPalindrome[A](l:List[A]):Boolean = isPalindromeHelper(l, l.reverse)
}
