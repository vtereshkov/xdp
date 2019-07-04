// Factorization demo


program Factor;



var
  LowBound, HighBound, Number, Dividend, Divisor, MaxDivisor: Integer;
  DivisorFound: Boolean;


begin
WriteLn;
WriteLn('Integer factorization demo');
WriteLn;
Write('From number: '); ReadLn(LowBound);
Write('To number  : '); ReadLn(HighBound);
WriteLn;

if LowBound < 2 then
  begin
  WriteLn('Numbers should be greater than 2');
  ReadLn;
  Halt(1);
  end;

for Number := LowBound to HighBound do
  begin
  Write(Number, ' = ');
  
  Dividend := Number;
  while Dividend > 1 do
    begin
    MaxDivisor := IMin(Round(Sqrt(Dividend)), Dividend - 1);
    Divisor := 1;
    DivisorFound := FALSE;
    
    while (Divisor <= MaxDivisor) and not DivisorFound do
      begin
      Inc(Divisor);
      if Dividend mod Divisor = 0 then DivisorFound := TRUE;
      end;
   
    if not DivisorFound then Divisor := Dividend;                // Prime number

    Write(Divisor, ' ');
    Dividend := Dividend div Divisor;
    end; // while
    
  WriteLn;  
  end; // for     

WriteLn;
WriteLn('Done.');

ReadLn;
end.
