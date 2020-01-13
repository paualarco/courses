



def general(f: (Int, Int)=> Int)(a: Int, b: Int): Int = {
  f(a,b)
}

def prod(a:Int, b:Int):Int = {
  if(a > b) 1 else a * prod(a+1,b)
}

def sum(a:Int, b:Int):Int = {
  if(a > b) 0 else a + sum(a+1,b)
}


def factorial(x: Int) = general(sum)(1,x)
factorial(5)
//product(f)(1,3)

