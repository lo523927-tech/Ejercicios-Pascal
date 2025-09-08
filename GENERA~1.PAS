program Generarcontrasena;

var
  minusculas, mayusculas, numeros, simbolos, caracteres, password: string;
  longitud, i, index: integer;
  resp: char;

begin
  randomize;  { inicializa números aleatorios }

  minusculas := 'abcdefghijklmnopqrstuvwxyz';
  mayusculas := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  numeros := '0123456789';
  simbolos := '!@#$%^&*()-_=+[]{};:,.<>?';

  write('Ingrese la longitud de la contraseña (8-16): ');
  readln(longitud);

  caracteres := minusculas;  { minúsculas siempre }

  write('¿Incluir mayúsculas? (s/n): ');
  readln(resp);
  if (resp = 's') or (resp = 'S') then
    caracteres := caracteres + mayusculas;

  write('¿Incluir números? (s/n): ');
  readln(resp);
  if (resp = 's') or (resp = 'S') then
    caracteres := caracteres + numeros;

  write('¿Incluir símbolos? (s/n): ');
  readln(resp);
  if (resp = 's') or (resp = 'S') then
    caracteres := caracteres + simbolos;

  { Asegurarse que hay caracteres disponibles }
  if Length(caracteres) = 0 then
  begin
    writeln('No hay caracteres disponibles para generar la contraseña.');
    exit;
  end;

  password := '';
  for i := 1 to longitud do
  begin
    index := 1 + Random(Length(caracteres));  { Random devuelve 0..N-1 }
    password := password + caracteres[index];
  end;

  writeln('Contraseña generada: ', password);
  writeln('Presione ENTER para salir...');
  readln;
end.
