program Test;

const
  one = 1;
  maxIters = 19 + one;

var
  x,y : integer;

procedure test();
begin
  y := x + 1;
  writeln(y);
end;

begin
  x := 17;
  test();
end.

