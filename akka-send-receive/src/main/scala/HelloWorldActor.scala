import akka.actor._

class HelloWorldActor(name :String) extends Actor {
  /**
   * Actor初期化時処理
   */
  override def preStart = {println(name + " is started.")  }

  /**
   * メッセージ受信時処理
   */
  def receive = {
    case msg: String  => {
      println("HelloWorldActor: Hello world! " + msg + " My name is " + name)
      "HelloWorldActor: Hello world! " + msg + " My name is " + name
    }
  }

  /**
   * Actor終了時処理
   */
  override def postStop = {println(name + " is stopped.")  }
}
object HelloWorldApp extends App {
  override def main(args: Array[String]): Unit = {
    val system = ActorSystem.apply("HelloWorldApp")
    val helloWorldActor = system.actorOf(Props.apply(new HelloWorldActor("actor1")), "HelloWorldActor")

    val result1 = helloWorldActor ! """Test1"""
    val result2 = helloWorldActor ! """Test2"""

    println("Test1 result is " + result1)
    println("Test2 result is " + result2)

    Thread.sleep(5000)
    system.shutdown()
  }
}