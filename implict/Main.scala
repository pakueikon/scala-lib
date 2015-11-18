import scala.collection.JavaConversions._

object Main{
	implicit val default1 = 100
	implicit val default2 = "test"
	implicit val default3 = 200.1
    def doubleInt(implicit x: Int) = x * x
	def hoge(implicit x: Int, s: String): String = s * x
	def main(args: Array[String]) = {
            println(testReturn())
			println(doubleInt)
			println(hoge(1,"hogehoge"))
    }
    implicit class TimeInt(i: Int) {
        def days = i * 1000 * 60 * 60 * 24
        def hours = i * 1000 * 60 * 60
        def minutes = i * 1000 * 60
        val seconds = i * 1000
    }
    def testReturn() : String = {
        println("start")

        println("end")
        return "return 1"
    }
}
