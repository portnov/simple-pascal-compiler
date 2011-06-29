program Hello;

var
  i, j: integer;
  z: integer;
  n: integer;
  arr: array [5] of integer;

function odd(x: integer): boolean;
begin
  return (x%2 = 1);
end;

procedure hello(a: integer);
begin
  writeln("Hello!");
  write(a);
  if a = 11
    then exit;
  if odd(a)
    then writeln(" is odd.")
    else writeln(" is even.");
end;

begin
  z := 1;
  arr[z] := 25;
  writeln(arr);
  j := 3 + z*2;
  write("Enter number: ");
  n := readln();
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
