program Hello;

const
  one = 1;
  maxIters = 19 + one;

type
  MyRecord =
    record
      int: integer;
      str: string;
      z: integer;
    end;

  MyArray = array [5] of integer;

var
  i, j: integer;
  z: integer;
  n: integer;
  arr: MyArray;
  rec: MyRecord;

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

procedure test_laziness(x: integer, y: integer);
begin
  write("Test: ");
  writeln(x);
end;

begin
  z := 1;
  arr[z] := 25;
  arr[4] := z;
  writeln(arr);
  rec.int := 1/0;
  rec.z := 7;
  rec.str := "test";
  test_laziness(rec.z - 1, rec.int);
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
    if i = maxIters
      then break;
    end;
end.

