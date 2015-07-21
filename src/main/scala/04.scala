object P04 {
    def lengthHelper[A](k:Int, l:List[A]):Int = (k, l) match {
        case (k, Nil)  => k
        case (k, _::t) => lengthHelper(k+1, t)
    }

    def length[A](l:List[A]):Int = lengthHelper(0, l)
}
