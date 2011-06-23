program Hello;

var
  i: integer;
  j: integer;
  z: integer;
  x: string;

function hello(a: integer): void;
begin
  writeln("Hello world!");
  printInt(a);
end;

begin
  z := 1;
  j := 3 + z*2;
  for i := j*2 to 15 do
    begin
    hello(i);
    end;
end.
