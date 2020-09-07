
object testi extends App {

var first = 20
var second = 10

if (first < second) {
  first = first / 2
}

if (first < 2 * second) {
  first = first * 2
  second = second / 2
} else {
  first = first + 1
  second = second - 1
}

val theyAreTheSame = (first == second)

if (theyAreTheSame) {
  first = first + 1
}

println(first + " " + second)
}