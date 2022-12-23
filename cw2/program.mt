let 
    var n := 0;
    var x := 0;
    var i := 51 != 12
in
begin
    printint(i);
    getint (n);
    if n < 0 then x := 0 else x := 1;
    i := 2;
    while i <= n do
        begin
            x := x * i;
            i := i + 1;
            printint (x)
        end;
    printint (x)
end