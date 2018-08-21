public fun <T, R, L> Collection<Tuple3<T, R, L>>.unzip3(): Tuple3<List<T>, List<R>, List<L>> {
    val expectedSize = this.size
    val listT = ArrayList<T>(expectedSize)
    val listR = ArrayList<R>(expectedSize)
    val listL = ArrayList<L>(expectedSize)
    for (pair in this) {
        listT.add(pair._1)
        listR.add(pair._2)
        listL.add(pair._3)
    }
    return Tuple3(listT, listR, listL)
}
