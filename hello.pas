program Hello;

var
  i: integer;
  j: integer;
  z: integer;
  x: string;

function odd(x: integer): boolean;
begin
  return (x%2 = 1);
end;

function hello(a: integer): void;
begin
  writeln("Hello!");
  printInt(a);
  if odd(a)
    then writeln("Odd!")
    else writeln("Even!");
end;

begin
  z := 1;
  j := 3 + z*2;
  for i := j*2 to 15 do
    begin
    hello(i);
    end;
end.
