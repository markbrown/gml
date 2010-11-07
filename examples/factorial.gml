{ /self /n
  n 2 lessi
    { 1 }
    { n 1 subi self self apply n muli }
  if
} /fac

{ fac fac apply } /fac

8 fac apply

