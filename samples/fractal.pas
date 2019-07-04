// Mandelbrot set fragment plot program


program Fractal;



const
  ReCmax = 0.08; ReCmin = -0.66; 
  ImCmax = -0.3; ImCmin = -1.25;

  Inf = 200;
  MaxPoints = 45;

  Scale = 320;



function ScreenX(x: Real): Integer;
begin
Result := 410 + Round(Scale * x);
end;



function ScreenY(y: Real): Integer;
begin
Result := 420 + Round(Scale * y);
end;



var
  ReC, ImC, ReZ, ImZ, ReZnew, ImZnew: Real;
  i, x, y, xmin, ymin, xmax, ymax: SmallInt;
  color: ShortInt;
  IsInf: Boolean;
  Palette: array [0..8] of ShortInt;



begin
// Custom palette
Palette[0] := 4 ; Palette[1] := 12; Palette[2] := 13; 
Palette[3] := 14; Palette[4] := 10; Palette[5] := 2 ; 
Palette[6] := 3 ; Palette[7] := 1 ; Palette[8] := 0 ; 

SetScreenMode($10);   // 640 x 350 pixels, 16 colors

// Border lines
xmin := ScreenX(ReCmin) - 1;  ymin := ScreenY(ImCmin) - 1;
xmax := ScreenX(ReCmax) + 1;  ymax := ScreenY(ImCmax) + 1;

Line(xmin, ymin, xmax, ymin, 15);
Line(xmin, ymax, xmax, ymax, 15);
Line(xmin, ymin, xmin, ymax, 15);
Line(xmax, ymin, xmax, ymax, 15);

// Mandelbrot set construction
ReC := ReCmin;

while ReC <= ReCmax do
  begin
  ImC := ImCmin;

  while ImC <= ImCmax do
    begin
    ReZ := 0;  ImZ := 0;
    IsInf := FALSE;
    color := 0;
    i := 1;

    while (i <= MaxPoints) and not IsInf do  
      begin
      ReZnew := ReZ * ReZ - ImZ * ImZ + ReC;
      ImZnew := 2 * ReZ * ImZ + ImC;

      if (abs(ReZnew) > Inf) or (abs(ImZnew) > Inf) then
        begin
        IsInf := TRUE;
        color := Palette[8 - (i - 1) div 5];
        end;

      ReZ := ReZnew;  ImZ := ImZnew;
      Inc(i);
      end; // while i...

    PutPixel(ScreenX(ReC), ScreenY(ImC), color);

    ImC := ImC + 0.001;
    end; // while ImC...
  
  ReC := ReC + 0.001;
  end; // while ReC...

repeat until KeyPressed;
SetScreenMode($03);
end.
