program Hello;

var
  i: integer;
  j: integer;
  z: integer;
  n: integer;

function odd(x: integer): boolean;
begin
  return (x%2 = 1);
end;

function hello(a: integer): void;
begin
  writeln("Hello!");
  printInt(a);
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
    hello(i);
    end;
end.
