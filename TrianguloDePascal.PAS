program TrianguloDePascal;
const
  N = 10;
var
  a: array[1..N, 1..N] of longint;
  i, j, sp: integer;
begin
  { Inicializar en 0 }
  for i := 1 to N do
    for j := 1 to N do
      a[i, j] := 0;

  { Base del triángulo }
  a[1, 1] := 1;

  for i := 2 to N do
  begin
    a[i, 1] := 1;
    for j := 2 to i do
      a[i, j] := a[i-1, j-1] + a[i-1, j];
  end;

  { Imprimir centrado }
  for i := 1 to N do
  begin
    for sp := 1 to N - i do
      write('   ');  { 3 espacios }
    for j := 1 to i do
      write(a[i, j]:6); { formato ancho 6 }
    writeln;
  end;

  readln;
end.
