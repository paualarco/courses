
/*
Synchronous
The coffee seller takes an order and waits for the coffee maker to finish his work, before to take another order
If we add one coffee seller and maker it won't be very resource efficient
Asynchronous
Now when the coffee seller receives an order, does not wait until the coffee maker finishes the coffee, and takes another coffee.

*/

//Synchronous

def coffeBreak(): Unit = {
  val coffee = makeCoffee()
  drink(coffee)
  chatWithColleagues()
}

//Asynchronous with CallBacks

def makeCoffee(coffeeDone: Coffee => Unit): {
  val coffe = ???
  coffeDone(coffe)
}

def coffeeBreak(): Unit