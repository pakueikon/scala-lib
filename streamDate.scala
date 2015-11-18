import java.util.{Calendar, Date}
import java.text.SimpleDateFormat

object streamDate{
	def days(begin:Date):Stream[Date] = {
	      val cal = Calendar.getInstance
	        cal.setTime(begin)
	          cal.add(Calendar.DAY_OF_MONTH, 1)
	            begin #:: days(cal.getTime)
	}
def parseDateString(d: String, format: String): Date = {
    val cal = Calendar.getInstance()
    cal.setTime(new SimpleDateFormat(format).parse(d))
    cal.getTime()
  }
def main(args: Array[String]){
	// from から to までの全ての日で処理を実行 "yyyy-MM-dd"
	val from = parseDateString("20150628","yyyyMMdd")
    val to = parseDateString("20150729","yyyyMMdd")
    val SIMPLE_DATE_FORMAT = new SimpleDateFormat("yyyyMMdd")
    var str = ""
    var str1 = "2015062820150629201506302015070120150702201507032015070420150705201507062015070720150708201507092015071020150711201507122015071320150714201507152015071620150717201507182015071920150720201507212015072220150723201507242015072520150726201507272015072820150729"
	days(from).takeWhile{ ! _.after(to) }.foreach{ date =>
	    println(SIMPLE_DATE_FORMAT.format(date.getTime()))
	    str += SIMPLE_DATE_FORMAT.format(date.getTime())

	}
	if (!str.equals(str1))println("出た!!!!!!!!!!")
}
	
}
