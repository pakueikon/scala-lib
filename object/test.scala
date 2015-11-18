object test {
	def main(args: Array[String]): Unit = {
	  val obj = callTest
	  idx = obj.i
	}
	def callTest():test = {
		val obj = new test(1,2)
		obj
	}

}
case class test(i:Int,j:Int){
	val a = i
	val b = j
	println("clsss test")
}