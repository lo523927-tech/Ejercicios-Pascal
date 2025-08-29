program Figuras;
uses crt;
var
  i,j: integer;
begin
  clrscr;

  writeln('Cuadro:');
  for i:=1 to 5 do
  begin
    for j:=1 to 5 do
      write('* ');
    writeln;
  end;

  writeln;
  writeln('Triangulo:');
  for i:=1 to 5 do
  begin
    for j:=1 to i do
      write('* ');
    writeln;
  end;

  writeln;
  writeln('Piramide:');
  for i:=1 to 5 do
  begin
    for j:=1 to 5-i do
      write(' ');
    for j:=1 to 2*i-1 do
      write('*');
    writeln;
  end;

  readln;
end.
