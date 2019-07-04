// The Game of Life


program Life;



const
  VideoBufOrigin = $A0000000;
  Width          = 320;
  Height         = 200;
  FieldSize      = 100;


type
  TVideoBuf = array [0..Height - 1, 0..Width - 1] of ShortInt;
  PVideoBuf = ^TVideoBuf;
  TField = array [1..FieldSize * FieldSize] of Boolean;


var
  VideoBuf: PVideoBuf;
  Fld: TField;



function ind(i, j: Integer): Integer;              // Linear index of a cell modulo field size
begin
while i > FIELDSIZE do i := i - FIELDSIZE;
while i < 1         do i := i + FIELDSIZE;
while j > FIELDSIZE do j := j - FIELDSIZE;
while j < 1         do j := j + FIELDSIZE;

Result := FIELDSIZE * (i - 1) + j;
end;




procedure Redraw;
const
  OriginX = Width div 2 - FieldSize div 2;
  OriginY = Height div 2 - FieldSize div 2;

var
  i, j: Integer;
  clr: ShortInt;

begin
for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    begin
    if Fld[ind(i, j)] then clr := 14 else clr := 1;
    VideoBuf^[OriginY + j, OriginX + i] := clr;
    end;

end; // Redraw




procedure Init;
var
  i, j: Integer;
begin
Randomize;

for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    Fld[ind(i, j)] := Random > 0.5;
end; // Init




procedure Regenerate;
var
  NextFld: TField;
  i, j, ni, nj, n: Integer;
begin

for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    begin
    // Count cell neighbors
    n := 0;
    for ni := i - 1 to i + 1 do
      for nj := j - 1 to j + 1 do
        if Fld[ind(ni, nj)] and not ((ni = i) and (nj = j)) then Inc(n);

    // Bear or kill the current cell in the next generation
    if Fld[ind(i, j)] then
      NextFld[ind(i, j)] := (n > 1) and (n < 4)  // Kill the cell or keep it alive
    else
      NextFld[ind(i, j)] := n = 3;               // Bear the cell or keep it dead
    end; // for j...

// Make new generation
for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    Fld[ind(i, j)] := NextFld[ind(i, j)];

end; // Regenerate




var
  Ch: Char;

begin
// Create initial population
Init;

// Set graphics mode
SetScreenMode($13);                       // 320 x 200, 256 colors
VideoBuf := PVideoBuf(VideoBufOrigin);

// Run simulation
repeat   
  Redraw;
  Regenerate;
until KeyPressed;

SetScreenMode($03);
end.

  

