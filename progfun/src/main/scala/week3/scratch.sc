import week3._
new Rational(1,2)

def error(msg: String) = throw new Error(msg)

//error("test")

val x = null
val y: String = x

// doesn't work:
//val z: Int = null

if(true) 1 else false