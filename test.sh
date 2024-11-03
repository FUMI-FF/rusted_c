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

assert 0 "0"
assert 42 "42"
assert 2 "1+1"
assert 5 "10-5"
assert 15 "1+2+3+4+5"
assert 2 " 1 + 1 "
assert 153 '1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17'
assert 10 '2*3+4'
assert 14 '2+3*4'
assert 26 '2*3+4*5'
assert 5 '50/10'
assert 9 '6*3/2'

echo "PASSED!!"
