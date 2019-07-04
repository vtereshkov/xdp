// Graphics palette usage demo


program Palette;



const
  VideoBufOrigin = $A0000000;
  Width          = 320;
  Height         = 200;


type
  TVideoBuf = array [0..Height - 1, 0..Width - 1] of ShortInt;
  PVideoBuf = ^TVideoBuf;


procedure SetPalette;
var
  i: Integer;
begin
for i := 0 to 255 do
  begin
  OutP($3C8, i);
  OutP($3C9, i div 8);   // Red
  OutP($3C9, i div 8);   // Green
  OutP($3C9, i div 4);   // Blue
  end;
end;


var
  x, y, i: Integer;
  Color: ShortInt;
  VideoBuf: PVideoBuf;


begin
SetScreenMode($13);     // 320 x 200, 256 colors
SetPalette;

VideoBuf := PVideoBuf(VideoBufOrigin);

i := 0;
repeat
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
      begin
      Color := Round(127.5 * (1 + sin(0.01 * (x + y + i)))) mod 256;
      VideoBuf^[y, x] := Color;
      end;
  Inc(i);
until KeyPressed;

SetScreenMode($03);
end.