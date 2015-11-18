import akka.actor._

object Main extends App{
  val system = ActorSystem("system")

  val ref = system.actorOf(Props[MyActor])

  val result = ref ? "Hello Akka world!"

  Thread.sleep(2000)
  println(result)
  system.shutdown()
}
class MyActor extends Actor{
  def receive : Receive = {
    case msg:String => {
    	println(msg)
    	sender ! "Hello too"
    }
  }
}