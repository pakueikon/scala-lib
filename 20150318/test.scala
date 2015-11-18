import   scala.util.control.Breaks.{break, breakable}

object test {
	def main(args: Array[String]) {

		testBreak()
		testDo()
	}
	//break確認
	def testBreak() = {
	var n = 0
	println("break確認------")
		breakable {
			while(true) {
				println(n)
				n += 1
				if (n >= 5) break
			}
		}
	println()
	println()
	}
	//do式
	def testDo() = {
	var n = 0
	println("do確認------")
	do{
		println(n)
		n+=1
	}while(n<=4)
	println()
	println()
	}
}

