#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./target/debug/rusted_c "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected but got $actual"
    exit 1
  fi
}

assert 0 "return 0;"
assert 42 "return 42;"
assert 2 "return 1+1;"
assert 5 "return 10-5;"
assert 15 "return 1+2+3+4+5;"
assert 2 "return 1 + 1 ;"
assert 153 'return 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17;'
assert 10 'return 2*3+4;'
assert 14 'return 2+3*4;'
assert 26 'return 2*3+4*5;'
assert 5 'return 50/10;'
assert 9 'return 6*3/2;'
assert 21 '1+2; return 5+20-4;'
assert 2 'a=2; return a;'
assert 10 'a=2; b=3+2; return a*b;'
assert 10 'return (1+4)*2;'
#assert 2 'if (1) return 2; return 3;'
#assert 3 'if (0) return 2; return 3;'

echo "PASSED!!"
