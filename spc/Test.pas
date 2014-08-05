program Test;

const
  one = 1;
  maxIters = 19 + one;

var
  x,y : integer;

procedure test();
begin
  y := x + 1;
  if y = 0
  then writeln(1)
  else writeln(2);
  writeln(y);
end;

begin
  x := 17;
  test();
end.

