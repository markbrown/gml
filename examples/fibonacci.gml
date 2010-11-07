{ /self /n /f
  n 1 lessi
    {}
    { f apply f n 1 subi self self apply }
  if
} /repeat

{ repeat repeat apply } /repeat

{ /n
  { /b /a a b a b addi } n repeat apply
} /fib

[ 1 1 10 fib apply ]

