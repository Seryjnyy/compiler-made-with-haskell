let 
    var n := 0;
    var x := 0;
    var i := 2 * 8 < 1 ? 999 : 0
in
begin
    printint(i);
    getint (n);
    if n < 0 then x := 0 else x := 1;
    i := (51 != 51) + 2;
    printint(i);
    while i <= n do
        begin
            x := x * i;
            i := i + 1;
            printint (x)
        end;
    printint (x)
end