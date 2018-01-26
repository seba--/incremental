val set1 = Seq(1, 2, 3)
val set2 = List[(Int, Symbol)]((1, 'a), (2, 'b), (1, 'c))

 val map2 = set2.toMap
var map3 = Map[Int, Symbol]()
val map1 = map2 foreach (x => map3 += (x._1 -> 'd))
map3
