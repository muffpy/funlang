// a simple factorial program
// (including a tail recursive version)


def fact(n: Int) : Int =
  if n == 0 then 1 else n * fact(n - 1);

def facT(n: Int, accT: Int) : Int =
  if n == 0 then accT else facT(n - 1, n * accT);

def fTTTT(n: Int) : Int = facT(n, 1);

def top() : Void = {
  print_int(fact(6));
  print_char(',');
  print_int(fTTTT(6));
  print_char('\n')
};

top()