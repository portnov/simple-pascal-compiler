program Hello;

var
  i, j: integer;
  z: integer;
  n: integer;

function odd(x: integer): boolean;
begin
  return (x%2 = 1);
end;

procedure hello(a: integer);
begin
  writeln("Hello!");
  printInt(a);
  if a = 11
    then exit;
  if odd(a)
    then writeln(" is odd.")
    else writeln(" is even.");
end;

begin
  z := 1;
  j := 3 + z*2;
  write("Enter number: ");
  n := readInt();
  for i := j*2 to n do
    begin
    if i = 14
      then continue;
    hello(i);
    if i = 12
      then writeln("Dozen.");
    if i = 20
      then break;
    end;
end.
