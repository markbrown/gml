
1.0 0.1 0.1 point /red
1.0 1.0 0.1 point /yellow

{ /v /u /face red 0.8 0.5 2.5 } /sr
{ /v /u /face yellow 0.8 0.5 2.5 } /sy

% trig
{ /a a sin a cos divf }
/tan

% repeat loop
{ /self /n /f
  n 1 lessi
    {}
    { f apply f n 1 subi self self apply }
  if
} /repeat

{ repeat repeat apply } /repeat

% vertex radius (u) and face radius (i) for 12- and 20-hedra
15.0 sqrt 3.0 sqrt addf 4.0 divf /u12
10.0 2.0 5.0 sqrt mulf addf sqrt 4.0 divf /u20
250.0 110.0 5.0 sqrt mulf addf sqrt 20.0 divf /i12
3.0 sqrt 3.0 5.0 sqrt addf mulf 12.0 divf /i20

% unit dodecahedron with the given surface
{ /surf

  % 180 - dihedral angle
  0.2 sqrt acos
  /theta

  % distance from center of face to center of edge
  0.5 36.0 tan apply divf
  /d

  surf plane
    theta rotatex
    0.0 0.0 d translate
  /p1

  surf plane
  { 72.0 rotatey p1 intersect } 5 repeat apply
  0.0 i12 0.0 translate
  /half

  half half 180.0 rotatex intersect
} /dodeca

% unit icosahedron with the given surface
{ /surf

  % dihedral angle - 180
  -41.81 /theta

  % distance from center of face to center of edge
  -0.5 60.0 tan apply divf
  /d

  % rotate forward and either left or right
  { rotatey
    0.0 0.0 d translate
    theta rotatex
    surf plane intersect
    0.0 0.0 d translate
  } /rot

  { 60.0 rot apply } /left
  { -60.0 rot apply } /right

  % hamiltonian
  surf plane
    left apply right apply right apply right apply
    left apply left apply left apply right apply
    left apply right apply left apply right apply
    right apply right apply left apply left apply
    left apply right apply left apply

  % center the shape
  0.0 i20 0.0 translate
  i20 u20 divf acos negf rotatex
} /icosa

% merge the dual shapes
{ /t
  sr dodeca apply
  1.0 t subf i12 divf t u12 divf addf uscale

  sy icosa apply
  1.0 t subf u20 divf t i20 divf addf uscale

  intersect

  0.4 uscale
  20.0 rotatey
} /dual

0.0 dual apply
-2.0 0.0 0.0 translate

0.25 dual apply
-1.0 0.0 0.0 translate
union

0.5 dual apply
union

0.75 dual apply
1.0 0.0 0.0 translate
union

1.0 dual apply
2.0 0.0 0.0 translate
union

/row

row 10.0 rotatex 0.0 1.5 0.0 translate
row 40.0 rotatex 0.0 0.0 0.0 translate union
row 70.0 rotatex 0.0 -1.5 0.0 translate union

0.0 0.0 6.0 translate

/scene

[
  -1.0 3.0 -2.0 point
  1.0 1.0 1.0 point
  pointlight
] /lights

{ /c c c c point } /grey

0.3 grey apply lights scene 3 50.0 934 700 "poly.ppm" render

