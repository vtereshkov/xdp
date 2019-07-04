// System library



const
  pi = 3.1415927;
  
  SEEKSTART = 0;  
  SEEKCUR   = 1;  
  SEEKEND   = 2;




type
  LongInt = Integer;
  
  Single = Real;
  
  PChar = ^Char;
  
  TStream = record
    Data: PChar;
    Index: Integer;
  end;

  PStream = ^TStream;  
  
  TRegisters = record
    AX, BX, CX, DX, DS, Flags: Integer;
  end;  



var
  RandSeed: Integer;
  IOError: Integer;
  LastReadChar: Char;




// System timer and keyboard state


function Timer: Integer;
var
  Reg: TRegisters;
begin
Reg.AX := 0;
Reg.CX := 0;
Reg.DX := 0;
Intr($1A, @Reg);
Result := Reg.CX shl 16 + Reg.DX;
end;



            
function KeyPressed: Boolean;
var
  Reg: TRegisters;
begin
Reg.AX := $0B00;
Intr($21, @Reg);
Result := (Reg.AX and $FF) <> 0;
end;




// Mathematical routines



procedure Randomize;
begin
RandSeed := Timer;
end;




function Random: Real;
begin
RandSeed := 1975433173 * RandSeed;
Result := 0.5 * (RandSeed / $7FFFFFFF + 1.0);
end;




function Min(x, y: Real): Real;
begin
if x < y then Result := x else Result := y;
end;




function IMin(x, y: Integer): Integer;
begin
if x < y then Result := x else Result := y;
end;





function Max(x, y: Real): Real;
begin
if x > y then Result := x else Result := y;
end;




function IMax(x, y: Integer): Integer;
begin
if x > y then Result := x else Result := y;
end;





// File and console I/O routines



procedure ReadConsole(var Ch: Char);
var
  Reg: TRegisters;
begin
Reg.AX := $0100;
Intr($21, @Reg);
Ch := Char(Reg.AX and $FF);
end;




procedure WriteConsole(Ch: Char);
var
  Reg: TRegisters;
begin
Reg.AX := $0200;
Reg.DX := Integer(Ch);
Intr($21, @Reg);
end;




procedure Rewrite(var F: Text; const Name: string);
var
  Reg: TRegisters;
begin
Reg.AX := $3C00;
Reg.CX := $0000;
Reg.DX := Integer(@Name) and $FFFF;
Reg.DS := Integer(@Name) shr 16;
Intr($21, @Reg);
F := Text(Reg.AX);
if Reg.Flags and 1 = 1 then IOError := Reg.AX else IOError := 0;    // Error code
end;




procedure Reset(var F: Text; const Name: string);
var
  Reg: TRegisters;
begin
Reg.AX := $3D02;
Reg.DX := Integer(@Name) and $FFFF;
Reg.DS := Integer(@Name) shr 16;
Intr($21, @Reg);
F := Text(Reg.AX);
if Reg.Flags and 1 = 1 then IOError := Reg.AX else IOError := 0;    // Error code
end;




procedure Close(F: Text);
var
  Reg: TRegisters;
begin
Reg.AX := $3E00;
Reg.BX := Integer(F);
Intr($21, @Reg);
end;




procedure BlockRead(F: Text; Buf: PChar; Len: SmallInt; var LenRead: SmallInt);
var
  Reg: TRegisters;
begin
Reg.AX := $3F00;
Reg.BX := Integer(F);
Reg.CX := Len;
Reg.DX := Integer(Buf) and $FFFF;
Reg.DS := Integer(Buf) shr 16;
Intr($21, @Reg);
LenRead := Reg.AX;
end;



  
procedure BlockWrite(F: Text; Buf: PChar; Len: SmallInt);
var
  Reg: TRegisters;
begin
Reg.AX := $4000;
Reg.BX := Integer(F);
Reg.CX := Len;
Reg.DX := Integer(Buf) and $FFFF;
Reg.DS := Integer(Buf) shr 16;
Intr($21, @Reg);
end;




procedure DeleteFile(const Name: string);
var
  Reg: TRegisters;
begin
Reg.AX := $4100;
Reg.DX := Integer(@Name) and $FFFF;
Reg.DS := Integer(@Name) shr 16;
Intr($21, @Reg);
end;




function SeekFile(F: Text; Pos: Integer; Mode: ShortInt): Integer;
var
  Reg: TRegisters;
begin
Reg.AX := $4200 + Mode;
Reg.BX := Integer(F);
Reg.CX := Pos shr 16;
Reg.DX := Pos and $FFFF;
Intr($21, @Reg);
Result :=  Reg.DX shl 16 + Reg.AX;
if Reg.Flags and 1 = 1 then IOError := Reg.AX else IOError := 0;    // Error code
end;





procedure Seek(F: Text; Pos: Integer);
var
  NewPos: Integer;
begin
NewPos := SeekFile(F, Pos, SEEKSTART);
if NewPos <> Pos then IOError := 1 else IOError := 0;
end;





function FilePos(F: Text): Integer;
begin
Result := SeekFile(F, 0, SEEKCUR);
end;





function EOF(F: Text): Boolean;
var
  OldPos: Integer;
begin
if Integer(F) = 0 then
  Result := FALSE
else
  begin  
  OldPos := SeekFile(F, 0, SEEKCUR);
  Result := SeekFile(F, 0, SEEKEND) = OldPos;
  OldPos := SeekFile(F, OldPos, SEEKSTART);
  end;
end;





function IOResult: Integer;
begin
Result := IOError;
IOError := 0;
end;





procedure WriteCh(F: Text; P: PStream; ch: Char);
var
  Dest: PChar;
begin
if P <> nil then             // String stream output
  begin                      
  Dest := PChar(Integer(P^.Data) + P^.Index);
  Dest^ := ch;
  Inc(P^.Index);
  end
else  
  if Integer(F) = 0 then     // Console output
    WriteConsole(ch)                
  else                       // File output
    BlockWrite(F, @ch, 1);
end;




procedure WriteInt(F: Text; P: PStream; Number: Integer);
var
  Digit, Weight: Integer;
  Skip: Boolean;

begin
if Number = 0 then
  WriteCh(F, P,  '0')
else
  begin
  if Number < 0 then
    begin
    WriteCh(F, P,  '-');
    Number := -Number;
    end;

  Weight := 1000000000;
  Skip := TRUE;

  while Weight >= 1 do
    begin
    if Number >= Weight then Skip := FALSE;

    if not Skip then
      begin
      Digit := Number div Weight;
      WriteCh(F, P,  Char(ShortInt('0') + Digit));
      Number := Number - Weight * Digit;
      end;

    Weight := Weight div 10;
    end; // while
  end; // else

end;





procedure WriteHex(F: Text; P: PStream; Number: Integer; Digits: ShortInt);
var
  i, Digit: ShortInt;
begin
for i := Digits - 1 downto 0 do
  begin
  Digit := (Number shr (i shl 2)) and $0F;
  if Digit <= 9 then Digit := ShortInt('0') + Digit else Digit := ShortInt('A') + Digit - 10;
  WriteCh(F, P,  Char(Digit));
  end; 
end;





procedure WritePointer(F: Text; P: PStream; Number: Integer);
begin
WriteHex(F, P, Number, 8);
end;





procedure WriteReal(F: Text; P: PStream; Number: Real);
const
  FracBits = 16;
var
  Integ, Frac, InvWeight, Digit, IntegExpon: Integer;
  Expon: Real;

begin
// Write sign
if Number < 0 then
  begin
  WriteCh(F, P,  '-');
  Number := -Number;
  end;

// Normalize number
if Number = 0 then Expon := 0 else Expon := ln(Number) / ln(10);
if (Expon > 8) or (Expon < -3) then
  begin
  IntegExpon := Trunc(Expon);
  if IntegExpon < 0 then Dec(IntegExpon);
  Number := Number / exp(IntegExpon * ln(10));
  end
else
  IntegExpon := 0;  

// Write integer part
Integ := Trunc(Number);
Frac  := Round((Number - Integ) * (1 shl FracBits));

WriteInt(F, P, Integ);  WriteCh(F, P, '.');

// Write fractional part
InvWeight := 10;

while InvWeight <= 10000 do
  begin
  Digit := (Frac * InvWeight) shr FracBits;
  if Digit > 9 then Digit := 9;
  WriteCh(F, P,  Char(ShortInt('0') + Digit));
  Frac := Frac - (Digit shl FracBits) div InvWeight;
  InvWeight := InvWeight * 10;
  end; // while

// Write exponent
if IntegExpon <> 0 then 
  begin
  WriteCh(F, P, 'e');  WriteInt(F, P, IntegExpon);
  end;
 
end;




procedure WriteString(F: Text; P: PStream; const s: string);
var
  i: Integer;
begin
i := 0;
while s[i] <> #0 do
  begin
  WriteCh(F, P, s[i]);
  Inc(i);
  end; 
end;




procedure WriteBoolean(F: Text; P: PStream; Flag: Boolean);
begin
if Flag then WriteString(F, P, 'TRUE') else WriteString(F, P, 'FALSE');
end;




procedure WriteNewLine(F: Text; P: PStream);
begin
WriteCh(F, P, #13);  WriteCh(F, P, #10);
end;




procedure ReadCh(F: Text; P: PStream; var ch: Char);
var
  Len: SmallInt;
  Dest: PChar;
begin
if P <> nil then                // String stream input
  begin                      
  Dest := PChar(Integer(P^.Data) + P^.Index);
  ch := Dest^;
  Inc(P^.Index);
  end
else  
  if Integer(F) = 0 then        // Console input
    begin
    ReadConsole(ch);
    if ch = #13 then WriteConsole(#10);
    end 
  else                          // File input
    begin
    BlockRead(F, @ch, 1, Len);
    if ch = #10 then BlockRead(F, @ch, 1, Len);
    if Len <> 1 then ch := #0;
    end;
LastReadChar := ch;             // Required by ReadNewLine
end;




procedure ReadInt(F: Text; P: PStream; var Number: Integer);
var
  Ch: Char;
  Negative: Boolean;

begin
Number := 0;

// Read sign
Negative := FALSE;
ReadCh(F, P, Ch);
if Ch = '+' then
  ReadCh(F, P, Ch)
else if Ch = '-' then   
  begin
  Negative := TRUE;
  ReadCh(F, P, Ch);
  end;

// Read number
while (Ch >= '0') and (Ch <= '9') do
  begin
  Number := Number * 10 + ShortInt(Ch) - ShortInt('0');
  ReadCh(F, P, Ch);
  end; 

if Negative then Number := -Number;
end;




procedure ReadReal(F: Text; P: PStream; var Number: Real);
var
  Ch: Char;
  Negative, ExponNegative: Boolean;
  Weight: Real;
  Expon: Integer;
 
begin
Number := 0;
Expon := 0;

// Read sign
Negative := FALSE;
ReadCh(F, P, Ch);
if Ch = '+' then
  ReadCh(F, P, Ch)
else if Ch = '-' then   
  begin
  Negative := TRUE;
  ReadCh(F, P, Ch);
  end;

// Read integer part
while (Ch >= '0') and (Ch <= '9') do
  begin
  Number := Number * 10 + ShortInt(Ch) - ShortInt('0');
  ReadCh(F, P, Ch);
  end;

if Ch = '.' then                     // Fractional part found
  begin
  ReadCh(F, P, Ch);

  // Read fractional part
  Weight := 0.1;
  while (Ch >= '0') and (Ch <= '9') do
    begin
    Number := Number + Weight * (ShortInt(Ch) - ShortInt('0'));
    Weight := Weight / 10;
    ReadCh(F, P, Ch);
    end;
  end;

if (Ch = 'E') or (Ch = 'e') then     // Exponent found
  begin
  // Read exponent sign
  ExponNegative := FALSE;
  ReadCh(F, P, Ch);
  if Ch = '+' then
    ReadCh(F, P, Ch)
  else if Ch = '-' then   
    begin
    ExponNegative := TRUE;
    ReadCh(F, P, Ch);
    end;

  // Read exponent
  while (Ch >= '0') and (Ch <= '9') do
    begin
    Expon := Expon * 10 + ShortInt(Ch) - ShortInt('0');
    ReadCh(F, P, Ch);
    end;

  if ExponNegative then Expon := -Expon;
  end;
     
if Expon <> 0 then Number := Number * exp(Expon * ln(10));
if Negative then Number := -Number;
end;




procedure ReadString(F: Text; P: PStream; const s: string);
var
  i: Integer;
  Ch: Char;
begin
i := 0;
ReadCh(F, P, Ch);

while Ch <> #13 do
  begin
  s[i] := Ch;
  Inc(i);
  ReadCh(F, P, Ch);
  end;

s[i] := #0;
end;




procedure ReadNewLine(F: Text; P: PStream);
var
  Ch: Char;
begin
Ch := LastReadChar;
while not EOF(F) and (Ch <> #13) do ReadCh(F, P, Ch);
LastReadChar := #0;
end;




// String manipulation routines


function StrLen(const s: string): SmallInt;
begin
Result := 0;
while s[Result] <> #0 do Inc(Result);
end;





procedure StrCopy(var Dest: string; const Source: string);
var
  i: Integer;
begin
i := -1;
repeat
  Inc(i);
  Dest[i] := Source[i];
until Source[i] = #0;
end;





procedure StrCat(var Dest: string; const Source: string);
var
  i, j: Integer;
begin
i := 0;
while Dest[i] <> #0 do Inc(i);
j := -1;
repeat 
  Inc(j);
  Dest[i + j] := Source[j];
until Source[j] = #0;
end;





function StrComp(const s1, s2: string): Integer;
var
  i: Integer;
begin
Result := 0;
i := -1;
repeat 
  Inc(i);
  Result := Integer(s1[i]) - Integer(s2[i]);
until (s1[i] = #0) or (s2[i] = #0) or (Result <> 0);
end;





procedure Val(const s: string; var Number: Real; var Code: Integer);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

ReadReal(Text(0), @Stream, Number);

if Stream.Index - 1 <> StrLen(s) then Code := Stream.Index - 1 else Code := 0;
end;





procedure Str(Number: Real; var s: string);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

WriteReal(Text(0), @Stream, Number);
s[Stream.Index] := #0;
end;





procedure IVal(const s: string; var Number: Integer; var Code: Integer);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

ReadInt(Text(0), @Stream, Number);

if Stream.Index - 1 <> StrLen(s) then Code := Stream.Index - 1 else Code := 0;
end;





procedure IStr(Number: Integer; var s: string);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

WriteInt(Text(0), @Stream, Number);
s[Stream.Index] := #0;
end;




// Graphics routines


procedure SetScreenMode(mode: Integer);
var
  Reg: TRegisters;
begin
Reg.AX := $00 shl 8 + mode;
Intr($10, @Reg);
end;



procedure PutPixel(x, y, clr: Integer);
var
  Reg: TRegisters;
begin
Reg.AX := $0C shl 8 + clr;
Reg.BX := 0;
Reg.CX := x;
Reg.DX := y;
Intr($10, @Reg);
end;



procedure Line(x1, y1, x2, y2, clr: Integer);
var
  x, y, xMax, xMin, yMax, yMin: Integer;
begin
if x1 > x2 then
  begin
  xMax := x1;  xMin := x2;
  end
else
  begin
  xMax := x2;  xMin := x1;
  end;

if y1 > y2 then
  begin
  yMax := y1;  yMin := y2;
  end
else
  begin
  yMax := y2;  yMin := y1;
  end; 
 
if x1 = x2 then
  for y := yMin to yMax do
    PutPixel(x1, y, clr)
else if y1 = y2 then
  for x := xMin to xMax do
    PutPixel(x, y1, clr)
else if abs(yMax - yMin) < abs(xMax - xMin) then
  for x := xMin to xMax do
    begin
    y := y1 + (y2 - y1) * (x - x1) div (x2 - x1); 
    PutPixel(x, y, clr);
    end
else
  for y := yMin to yMax do
    begin
    x := x1 + (x2 - x1) * (y - y1) div (y2 - y1); 
    PutPixel(x, y, clr);
    end

end;




procedure Circle(x, y, r, clr: Integer);
var
  t, dt: Real;
  dx, dy: Integer;
begin
t := 0;  dt := 0.5 / r;

while t < Pi / 2 do
  begin
  dx := Round(r * cos(t));
  dy := Round(r * sin(t));

  PutPixel(x + dx, y + dy, clr);
  PutPixel(x - dx, y + dy, clr);
  PutPixel(x - dx, y - dy, clr);
  PutPixel(x + dx, y - dy, clr);

  t := t + dt;
  end;

end;




procedure OutCharXY(x, y, clr: Integer; ch: Char);
const
  CharSetOrigin = $F000 shl 16 + $FA6E;

type
  TCharBitmap = array [0..7] of ShortInt;
  PCharBitmap = ^TCharBitmap;
  
var
  CharBitmap: PCharBitmap;
  i, j: Integer;

begin
CharBitmap := PCharBitmap(CharSetOrigin + Integer(ch) shl 3);

for i := 0 to 7 do
  for j := 0 to 7 do
    if (CharBitmap^[i] and (1 shl j)) <> 0 then PutPixel(x + 7 - j, y + i, clr);
end;




procedure OutTextXY(x, y, clr: Integer; const s: string);
var
  i: Integer;
begin
i := 0;
while s[i] <> #0 do
  begin
  OutCharXY(x, y, clr, s[i]);
  x := x + 8;
  Inc(i);
  end;
end; 



