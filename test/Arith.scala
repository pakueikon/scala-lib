import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.matching.Regex._
import scala.collection

object main {

  //括弧のペアで分割可否をチェック
  def checkBracketPair(str:String) : Boolean = {
    ((str count {c=> c == '('}) == (str count {c=> c == ')'})) && ((str count {c=> c == '['}) == (str count {c=> c == ']'}))
  }
  //文字列挿入
  def insert(s: String, i: String, p: Int) = List(s take p, i, s drop p).mkString
  //カラム名（DB上）、カラムデータ型、analyze(Esの検索タイプ)
  case class ColumnsTypeMySQL(ColName:String,ColType:String)
  //検索モード
  object SearchMode {
    val Perfact = "*"
    val Kuromoji = ""
    val Ngram = "+"
  }
  object ParamType {
    val STRING = "String"
    val INT = "Int"
    val LONG = "Long"
    val DOUBLE = "Double"
    val BOOLEAN = "Boolean"
  }

  val ERR_MSG = "%s : the search mode unsupported"
  //対象カラムの型を取得Mysql
  val ColumnsMapMySQL = Map(
    "spam_user"   -> ColumnsTypeMySQL("spamUser",ParamType.INT),
    "entry_count" -> ColumnsTypeMySQL("entryCount",ParamType.INT),
    "friend"      -> ColumnsTypeMySQL("friend",ParamType.INT),
    "follower"    -> ColumnsTypeMySQL("follower",ParamType.INT),
    "favorite"    -> ColumnsTypeMySQL("favorite",ParamType.INT),
    "bio"         -> ColumnsTypeMySQL("bio",ParamType.STRING),
    "gender"      -> ColumnsTypeMySQL("gender",ParamType.STRING),
    "generation"  -> ColumnsTypeMySQL("generation",ParamType.STRING),
    "pref"        -> ColumnsTypeMySQL("pref",ParamType.STRING),
    "occupation"  -> ColumnsTypeMySQL("occupation",ParamType.STRING),
    "author_id"   -> ColumnsTypeMySQL("authorId",ParamType.STRING),
    "nickname"    -> ColumnsTypeMySQL("nickname",ParamType.STRING),
    "image_url"   -> ColumnsTypeMySQL("imageUrl",ParamType.STRING)
  )
  //カラムデータ型、検索カラム(same,kuromoji,ngram)
  case class ColumnsTypeES(ColType:String,ColSearchMode:String)
  //対象カラムの型を取得ES
  val ColumnsMapEs = Map(
    "url"         -> ColumnsTypeES(ParamType.STRING,"url,'',''"),
    "title"       -> ColumnsTypeES(ParamType.STRING,"'',title,ntitle"),
    "body"        -> ColumnsTypeES(ParamType.STRING,"'',body,nbody"),
    "created_at"   -> ColumnsTypeES(ParamType.STRING,"created_at,'',''"),
    "created_at_utc"   -> ColumnsTypeES(ParamType.STRING,"created_at_utc,'',''"),
    "domain"   -> ColumnsTypeES(ParamType.STRING,"domain,'',''"),
    "siteid"   -> ColumnsTypeES(ParamType.STRING,"siteid,'',''"),
    "authorid"   -> ColumnsTypeES(ParamType.STRING,"authorid,'',''"),
    "article_mod_date"   -> ColumnsTypeES(ParamType.STRING,"article_mod_date,'',''"),
    "entry_count"   -> ColumnsTypeES(ParamType.INT,"entry_count,'',''"),
    "gender"   -> ColumnsTypeES(ParamType.INT,"gender,'',''"),
    "generation"   -> ColumnsTypeES(ParamType.INT,"generation,'',''"),
    "pref"   -> ColumnsTypeES(ParamType.INT,"pref,'',''"),
    "sentiment"   -> ColumnsTypeES(ParamType.INT,"sentiment,'',''"),
    "hour"   -> ColumnsTypeES(ParamType.INT,"hour,'',''"),
    "day_of_week"   -> ColumnsTypeES(ParamType.INT,"day_of_week,'',''"),
    "nickname"   -> ColumnsTypeES(ParamType.STRING,"'','',nickname"),
    "in_reply_to"   -> ColumnsTypeES(ParamType.STRING,"in_reply_to,'',''"),
    "retweeted_url"   -> ColumnsTypeES(ParamType.STRING,"retweeted_url,'',''"),
    "rel_flag"   -> ColumnsTypeES(ParamType.STRING,"rel_flag,'',''"),
    "img_url"   -> ColumnsTypeES(ParamType.STRING,"img_url,'',''"),
    "source_url"   -> ColumnsTypeES(ParamType.STRING,"source_url,'',''"),
    "to_user"   -> ColumnsTypeES(ParamType.STRING,"to_user,'',''"),
    "lang"   -> ColumnsTypeES(ParamType.STRING,"lang,'',''"),
    "metadata"   -> ColumnsTypeES(ParamType.STRING,"metadata,'',''"),
    "spamtweet"   -> ColumnsTypeES(ParamType.BOOLEAN,"spamtweet,'',''"),
    "botuser"   -> ColumnsTypeES(ParamType.BOOLEAN,"botuser,'',''"),
    "spamuser"   -> ColumnsTypeES(ParamType.BOOLEAN,"spamuser,'',''"),
    "follower"   -> ColumnsTypeES(ParamType.INT,"follower,'',''"),
    "friend"   -> ColumnsTypeES(ParamType.INT,"friend,'',''"),
    "listed_count"   -> ColumnsTypeES(ParamType.INT,"listed_count,'',''"),
    "klout_score"   -> ColumnsTypeES(ParamType.INT,"klout_score,'',''"),
    "mention_flag"   -> ColumnsTypeES(ParamType.INT,"mention_flag,'',''"),
    "account_flag"   -> ColumnsTypeES(ParamType.INT,"account_flag,'',''"),
    "rt_count"   -> ColumnsTypeES(ParamType.INT,"rt_count,'',''"),
    "tweet_type"   -> ColumnsTypeES(ParamType.INT,"tweet_type,'',''"),
    "occupation"   -> ColumnsTypeES(ParamType.INT,"occupation,'',''"),
    "bio"   -> ColumnsTypeES(ParamType.STRING,"'','',bio"),
    "user_id"   -> ColumnsTypeES(ParamType.INT,"user_id,'',''"),
    "timezone"   -> ColumnsTypeES(ParamType.STRING,"timezone,'',''"),
    "favorite"   -> ColumnsTypeES(ParamType.INT,"favorite,'',''"),
    "media_url"   -> ColumnsTypeES(ParamType.STRING,"media_url,'',''"),
    "expanded_url"   -> ColumnsTypeES(ParamType.STRING,"'','',expanded_url_ngram"),
    "expanded_url_type"   -> ColumnsTypeES(ParamType.INT,"expanded_url_type,'',''"),
    "hash_tag"   -> ColumnsTypeES(ParamType.STRING,"hash_tag,'',''"),
    "old_oem_id"   -> ColumnsTypeES(ParamType.INT,"old_oem_id,'',''"),
    "service_id"   -> ColumnsTypeES(ParamType.INT,"service_id,'',''"),
    "oem_id"   -> ColumnsTypeES(ParamType.INT,"oem_id,'',''"),
    "preset_id"   -> ColumnsTypeES(ParamType.INT,"preset_id,'',''"),
    "search_type_id"   -> ColumnsTypeES(ParamType.INT,"search_type_id,'',''"),
    "commentnum"   -> ColumnsTypeES(ParamType.INT,"commentnum,'',''"),
    "comment"   -> ColumnsTypeES(ParamType.STRING,"'','',comment"),
    "comment_mod_date"   -> ColumnsTypeES(ParamType.STRING,"comment_mod_date,'',''"),
    "trackbacknum"   -> ColumnsTypeES(ParamType.INT,"trackbacknum,'',''"),
    "trackback_mod_date"   -> ColumnsTypeES(ParamType.STRING,"trackback_mod_date,'',''"),
    "imgnum"   -> ColumnsTypeES(ParamType.INT,"imgnum,'',''"),
    "img"   -> ColumnsTypeES(ParamType.STRING,"'','',img"),
    "img_mod_date"   -> ColumnsTypeES(ParamType.STRING,"img_mod_date,'',''"),
    "linknum"   -> ColumnsTypeES(ParamType.INT,"linknum,'',''"),
    "link_mod_date"   -> ColumnsTypeES(ParamType.STRING,"link_mod_date,'',''"),
    "splog_content"   -> ColumnsTypeES(ParamType.INT,"splog_content,'',''"),
    "splog_content_mod_date"   -> ColumnsTypeES(ParamType.STRING,"splog_content_mod_date,'',''"),
    "splog_copy"   -> ColumnsTypeES(ParamType.INT,"splog_copy,'',''"),
    "splog_copy_mod_date"   -> ColumnsTypeES(ParamType.STRING,"splog_copy_mod_date,'',''"),
    "splog_sp"   -> ColumnsTypeES(ParamType.INT,"splog_sp,'',''"),
    "birthyear"   -> ColumnsTypeES(ParamType.INT,"birthyear,'',''"),
    "age"   -> ColumnsTypeES(ParamType.INT,"age,'',''"),
    "author_home_url"   -> ColumnsTypeES(ParamType.STRING,"'',author_home_url,''"),
    "author_home_title"   -> ColumnsTypeES(ParamType.STRING,"'',author_home_title,''"),
    "author_pic_url"   -> ColumnsTypeES(ParamType.STRING,"'',author_pic_url,''"),
    "linknum"   -> ColumnsTypeES(ParamType.INT,"linknum,'',''"),
    "link"   -> ColumnsTypeES(ParamType.STRING,"link,'',''"),
    "link_mod_date"   -> ColumnsTypeES(ParamType.STRING,"link_mod_date,'',''")
  )
  //トップレベルのOR,ANDで区切る
  def partitionString(target:String) : (ArrayBuffer[String],ArrayBuffer[String]) ={
    var temp_where = ""
    val top_list = target.split(" ")
    val where = ArrayBuffer[String]()
    val logic = ArrayBuffer[String]()
    top_list.foreach(s=>
      s match {
        case "OR" | "AND" => {
          //temp_whereの(),[]がペアされた場合
          checkBracketPair(temp_where) match{
            case true =>{
              logic += s
              where += temp_where
              temp_where = ""
            }
            case _ => temp_where += " " + s
          }
        }
        case _ => {
          temp_where += " " + s
        }
      }
    )
    where += top_list(top_list.size-1) // 最後の条件を補完
    logic += ""
    (where,logic)
  }

  //マップから検索クエリに使うカラム名を取得(ES)
  def getFieldForES(field:String):String={
    ""
  }

  //マップから型を取得検索方法可否をチェック(MYSQL)
  def getFieldForMySQL(field:String):String={
    val s = field(0)
    val k = if ((s == SearchMode.Ngram(0)) || (s == SearchMode.Perfact(0))) field drop(1) else field
    val c = ColumnsMapMySQL.get(k).getOrElse(null).ColType
    val msg = if (c!=null){
      if (((c == ParamType.INT) && (s == SearchMode.Ngram(0))) || (!(SearchMode.Ngram+SearchMode.Perfact).contains(s))) ERR_MSG.format(k) + "\n\r"
      else ""
    }else "%s : the field unsupported".format(k) + "\n\r"
    msg
  }

  //各フィールドの検索モードをチェックしエラーを返す
  def checkFieldSearchMode(list:ArrayBuffer[String]):String = {
    var msg = ""
    list.foreach { target =>
      val str_regex: Regex = """[^()\s]+[:]""".r
      val str_match = str_regex.findAllIn(target)
      str_match.foreach{s=>
        msg += getFieldForMySQL(s dropRight(1))
      }
    }
    msg
  }
  //フィールドのカンマ区切りを分割  TODO トップレベルのみ対応、複合条件の場合は未対応
  def partitionColumns(list:ArrayBuffer[String]):ArrayBuffer[String] = {
    val r = ArrayBuffer[String]()
    list.foreach { target =>
      val cnt = target count { c => c == ',' }
      if (cnt > 0) {
        var str_change = ""
        val str_regex: Regex = """[^()]+[,]+[^()]+[:]+[^()]+""".r
        val str_match = str_regex.findFirstMatchIn(target).get.toString
        val s = str_match.split(",")
        val sk = s(s.size - 1).substring(s(s.size - 1).indexOf(":"), s(s.size - 1).length)
        //println(sk)
        for (ii <- 0 to s.size - 2) {
          str_change += "(" + s(ii).trim + sk + ") AND "
        }
        str_change = "(" + str_change + "(" + s(s.size - 1) + "))"
        //println(str_change)
        r += str_change
      } else r+= target
    }
    r
  }
  //条件ごとのOR,ANDを解析 A:B OR C  →　(A:B OR A:C)
  //正規表現マッチング参考
  //https://www.safaribooksonline.com/library/view/scala-cookbook/9781449340292/ch01s07.html
  def partitionOrAnd(list:ArrayBuffer[String]):ArrayBuffer[String] = {
    val r = ArrayBuffer[String]()
    list.foreach { _target =>
      var target = _target
      try {
        val str_regex: Regex = """[^(),]+[:]+[^()\[\]]+[OR|AND][^():\[\]]+""".r
        val str_match = str_regex.findAllIn(target)
        str_match.foreach { s =>
          var str_change = ""
          //println(s)
          val str_split: String = {
            if (s contains "OR") {
              "OR"
            }
            else if (s contains "AND") {
              "AND"
            }
            else {
              ""
            }
          }
          //println(str_split)
          if (str_split != "") {
            val sl = s.split(str_split)
            val sk = sl(0).substring(0, sl(0).indexOf(":") + 1)
            //println(sk)
            for (ii <- 1 to sl.size - 1) {
              str_change += " " + str_split + " " + sk + sl(ii).trim
              //println(str_split + " " + sk + sl(ii).trim)
            }
            str_change = sl(0) + str_change
            //println(sl)
            //println(str_change)
            target = target.replace(s, str_change)
          } else {
            target
          }
        }
        //println(">>>>>>>>>>>>>"+target)
        r += target
      } catch {
        case e: Exception => {
          println("!!!!!!!!!!!!partitionKeywords Error : " + e.getMessage())
          r += target
        }
      }
    }
    r
  }
  def partitionKeywords2gram(_target:String):String = {
    //println(target)
    var target = _target
    val cnt = target count {c=> c == ':'}
    try{
      val str_regex : Regex = """[^(),]+[:\[]+[^()\[\]]+[OR|AND][^():\[\]]+[\]]""".r
      val str_match = str_regex.findAllIn(target)
      //println(str_match.mkString(","))
      str_match.foreach{_s=>
        val s = _s.trim.replace("[","").replace("]","")
        var str_change = ""
        //println(s)
        val str_split:String = {
          if(s contains "OR") {"OR"}
          else if(s contains "AND")  {"AND"}
          else {""}
        }
        //println(str_split)
        if (str_split!=""){
          val sl = s.split(str_split)
          val sk = sl(0).substring(0,sl(0).indexOf(":") + 1)
          //println(sk)
          for(ii <- 1 to sl.size-1){
            str_change += " " + str_split + " " + sk + "[" + sl(ii).trim + "]"
            //println(str_split + " " + sk + sl(ii).trim)
          }
          val ss = insert(sl(0), "[", sl(0).indexOf(":")+1).trim + "]"
          str_change = ss + str_change
          //println(sl)
          //println(str_change)
          target = target.replace(_s,str_change)
        }else{
          target
        }
      }
      //println(">>>>>>>>>>>>>"+target)
      target
    }catch{
      case e:Exception => {
        println("!!!!!!!!!!!!partitionKeywords Error : "+e.getMessage())
        target
      }
    }
  }

  //クエリをMYSQLの表現にパース  NOTあり文
  def partitionLogicNot(_target:String):String={
    var target = _target
    val str_regex : Regex = """[^(),\sANDOR]+:NOT\s+[^()\s]*""".r
    val str_match = str_regex.findAllIn(target)
    str_match.foreach{sm=>
      //println(sm)
      //フィールド抽出
      val s = sm drop(1)
      val sl = sm(0).toString //検索モード
      val f = ColumnsMapMySQL.get(s.split(":")(0)).get
      f match {
        case f:ColumnsTypeMySQL => {
          var str_change = ""
          val field = f.ColName
          val ftype = f.ColType
          //キー
          val keyword = s.split(" ")(1)
          //条件を作成
          val swhere = {
            if (ftype == ParamType.INT){
              keyword
            }else{
              "'%s'".format(keyword)
            }
          }
          sl match {
            case SearchMode.Perfact => {//完全一致
              str_change = "%s<>%s".format(field,swhere)
            }
            case _ => {
              str_change = "%s not like %s".format(field,"'%"+keyword+"%'")
            }
          }
          target = target.replace(sl+s,str_change)
        }
        case _ => {}
      }
    }
    target
  }
  //クエリをMYSQLの表現にパース
  def partitionLogic(_target:String):(String,String)={
    var target = _target
    val str_regex : Regex = """[^()\sANDOR]+:+[^()\s]*""".r
    val str_match = str_regex.findAllIn(target)
    var msg = ""
    str_match.foreach{sm=>
      //println(sm)
      //フィールド抽出
      val s = sm drop(1)
      val sl = sm(0).toString //検索モード
      val f = ColumnsMapMySQL.get(s.split(":")(0)).get
      f match {
        case f:ColumnsTypeMySQL => {
          var str_change = ""
          val field = f.ColName
          val ftype = f.ColType
          //キー
          val keyword = s.split(":")(1)
          //条件を作成
          val swhere = {
            if (ftype == ParamType.INT){
              keyword
            }else{
              "'%s'".format(keyword)
            }
          }
          val term = keyword contains ".."  //範囲検索ありなし
          sl match {
            case SearchMode.Perfact => {//完全一致
              if (term) {
                val _s = keyword.split("[..]")
                _s.size match {
                  case 1 => str_change = "%s>=%s".format(field, _s(0))                    // 1..
                  case _ => {
                    if (_s(0) == "") str_change = "%s<=%s".format(field, _s(2))           // ..1
                    else str_change = "%s BETWEEN %s AND %s".format(field, _s(0), _s(2))  // 1..100
                  }
                }
              }else str_change = "%s=%s".format(field,swhere)
            }
            case _ => {
              if (term) msg = ERR_MSG.format(field)
              else str_change = "%s like %s".format(field,"'%"+keyword+"%'")
            }
          }
          target = target.replace(sl+s,str_change)
        }
        case _ => {}
      }
    }
    (target,msg)
  }

  def main(args:Array[String]) {
    val str =
s"""
 (((+bio:ラーメン OR らーめん)
  AND (*gender:おいしい OR うまい))
  OR (+generation:NOT rakuten AND NOT amazon))
 AND *spam_user,*entry_count:123
 AND (*follower:..100 OR 10000..99999)
 AND (+nickname,*bio:NOT ニコラ OR NOT にこら)
 AND (*follower,*favorite:10..)
 AND (*entry_count:NOT 1)
 AND :)""".replace(":)","sentiment=positive").replace(":(","sentiment=negative").replace("(:)","sentiment=neutral")

    val part:(ArrayBuffer[String],ArrayBuffer[String]) = partitionString(str)
    var str_where = part._1
    val str_logic = part._2
    str_where = partitionColumns(str_where)
    val msg = checkFieldSearchMode(str_where)
    if (msg == ""){
      str_where = partitionOrAnd(str_where)
      //str_where.foreach(println)
      //var sql = ""
      val sql = for(i <- 0 to str_where.size - 1) yield str_where(i) + " " + str_logic(i) + " "
      val str_sql = partitionLogicNot(sql.mkString((" ")))
      val result = partitionLogic(str_sql)
      println("--------------result--------------")
      println(result._1)
      println("---------------Error--------------")
      println(msg)
      println(result._2)
    }else{
      println(msg)
    }

  }
}
