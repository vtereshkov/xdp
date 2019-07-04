// Clock demo program


program Clock;



const
  LHr        =  60;
  LMn        = 100;
  LSec       = 120;
  RTickStart = 130;
  RTickEnd   = 160;
  LTail      = -10;
  RClock     = 170;



var
  Time: Integer;
  Hr, Mn, Sec, HrOld, MnOld, SecOld: Real;





procedure DrawArrow(Value, Rmin, Rmax: Real; Positions: Integer; Color: Integer);
var
  Angle: Real;
begin 
Angle := Pi / 2 - 2 * Pi * Value / Positions;
Line(320 + Round(Rmin * cos(Angle)), 175 - Round(Rmin * sin(Angle)), 
     320 + Round(Rmax * cos(Angle)), 175 - Round(Rmax * sin(Angle)), Color);
end;



procedure DrawDigits(Value: Integer; R: Real; Positions: Integer; Color: Integer);
var
  Angle: Real;
  Digits: string;
begin 
Angle := Pi / 2 - 2 * Pi * Value / Positions;
IStr(Value, Digits);
OutTextXY(320 + Round(R * cos(Angle)), 175 - Round(R * sin(Angle)), Color, Digits);
end;




var
  TickIndex: Integer;
  


begin
SetScreenMode($10);   // 640 x 350 pixels, 16 colors

Circle(320, 175, Round(RClock), 15);

for TickIndex := 0 to 11 do
  DrawArrow(TickIndex, RTickStart,      RTickEnd, 12, 15);    // Draw long ticks

for TickIndex := 0 to 59 do
  DrawArrow(TickIndex, RTickStart + 20, RTickEnd, 60, 15);    // Draw short ticks

repeat
  Time := Round(Timer / 1573032 * 86400);

  Hr  := Time / 3600;
  Mn  := (Time mod 3600) / 60;
  Sec := (Time mod 3600) mod 60;

  if Sec <> SecOld then
    begin
    DrawArrow(SecOld, LTail, LSec, 60, 0 );   // Erase old arrow
    DrawArrow(Sec,    LTail, LSec, 60, 14);   // Draw new arrow

    DrawArrow(MnOld, LTail, LMn, 60, 0 );     // Erase old arrow
    DrawArrow(Mn,    LTail, LMn, 60, 13);     // Draw new arrow

    DrawArrow(HrOld, LTail, LHr, 12, 0 );     // Erase old arrow
    DrawArrow(Hr,    LTail, LHr, 12, 10);     // Draw new arrow
    
    // Refresh hour digits
    for TickIndex := 1 to 12 do
      DrawDigits(TickIndex, RTickStart - 20, 12, 15);
    end;

  HrOld := Hr;  MnOld := Mn;  SecOld := Sec;
until KeyPressed; 

  
SetScreenMode($03);
end.

