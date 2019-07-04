// XD Pascal - a 32-bit compiler for MS-DOS (real CPU mode)
// Developed by Vasiliy Tereshkov, 2009-2010


{$APPTYPE CONSOLE}
{$I-}


program XDP;


// uses SysUtils;       // For debug purposes only


const
  VERSION               = '0.7.12';
  
  NUMDELIMITERS         = 22;
  NUMKEYWORDS           = 31;

  // Standard token codes

  OPARTOK               = 1;
  CPARTOK               = 2;
  MULTOK                = 3;
  PLUSTOK               = 4;
  COMMATOK              = 5;
  MINUSTOK              = 6;
  PERIODTOK             = 7;
  RANGETOK              = 8;
  DIVTOK                = 9;
  COLONTOK              = 10;
  ASSIGNTOK             = 11;
  SEMICOLONTOK          = 12;
  LTTOK                 = 13;
  LETOK                 = 14;
  NETOK                 = 15;
  EQTOK                 = 16;
  GTTOK                 = 17;
  GETOK                 = 18;
  ADDRESSTOK            = 19;
  OBRACKETTOK           = 20;
  CBRACKETTOK           = 21;
  DEREFERENCETOK        = 22;

  ANDTOK                = 23;
  ARRAYTOK              = 24;
  BEGINTOK              = 25;
  CASETOK               = 26;
  CONSTTOK              = 27;
  IDIVTOK               = 28;
  DOTOK                 = 29;
  DOWNTOTOK             = 30;
  ELSETOK               = 31;
  ENDTOK                = 32;
  FORTOK                = 33;
  FUNCTIONTOK           = 34;
  IFTOK                 = 35;
  MODTOK                = 36;
  NILTOK                = 37;
  NOTTOK                = 38;
  OFTOK                 = 39;
  ORTOK                 = 40;
  PROCEDURETOK          = 41;
  PROGRAMTOK            = 42;
  RECORDTOK             = 43;
  REPEATTOK             = 44;
  SHLTOK                = 45;
  SHRTOK                = 46;
  THENTOK               = 47;
  TOTOK                 = 48;
  TYPETOK               = 49;
  UNTILTOK              = 50;
  VARTOK                = 51;
  WHILETOK              = 52;
  XORTOK                = 53;

  // Non-standard token codes

  IDENTTOK              = 101;
  INTNUMBERTOK          = 102;
  FRACNUMBERTOK         = 103;
  CHARLITERALTOK        = 104;
  STRINGLITERALTOK      = 105;

  // Identifier kind codes

  CONSTANT              = 1;
  USERTYPE              = 2;
  VARIABLE              = 3;
  PROC                  = 4;
  FUNC                  = 5;

  // Type kinds

  ANYTYPE               = 1;
  INTEGERTYPE           = 2;
  SMALLINTTYPE          = 3;
  SHORTINTTYPE          = 4;
  CHARTYPE              = 5;
  BOOLEANTYPE           = 6;
  REALTYPE              = 7;
  POINTERTYPE           = 8;
  TEXTTYPE              = 9;
  ARRAYTYPE             = 10;
  RECORDTYPE            = 11;
  SUBRANGETYPE          = 12;
  FORWARDTYPE           = 101;

  IntegerTypes          = [INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE];
  OrdinalTypes          = IntegerTypes + [CHARTYPE, BOOLEANTYPE, SUBRANGETYPE];

  // Type indices

  ANYTYPEINDEX          = 1;      // Base type for untyped pointers
  INTEGERTYPEINDEX      = 2;
  SMALLINTTYPEINDEX     = 3;
  SHORTINTTYPEINDEX     = 4;
  CHARTYPEINDEX         = 5;
  BOOLEANTYPEINDEX      = 6;
  REALTYPEINDEX         = 7;
  POINTERTYPEINDEX      = 8;      // Untyped pointer, compatible with any other
  TEXTTYPEINDEX         = 9;      // Universal file type
  STRINGTYPEINDEX       = 10;

  // Predefined routine codes

  INCPROC               = 1;
  DECPROC               = 2;
  READPROC              = 3;
  WRITEPROC             = 4;
  READLNPROC            = 5;
  WRITELNPROC           = 6;
  INPPROC               = 7;      // Read from a port
  OUTPPROC              = 8;      // Write to a port
  NEWPROC               = 9;
  DISPOSEPROC           = 10;
  HALTPROC              = 11;
  INTRPROC              = 12;

  SIZEOFFUNC            = 15;
  ORDFUNC               = 16;
  CHRFUNC               = 17;
  PREDFUNC              = 18;
  SUCCFUNC              = 19;
  ROUNDFUNC             = 20;
  TRUNCFUNC             = 21;
  ABSFUNC               = 22;
  SQRFUNC               = 23;
  SINFUNC               = 24;
  COSFUNC               = 25;
  ARCTANFUNC            = 26;
  EXPFUNC               = 27;
  LNFUNC                = 28;
  SQRTFUNC              = 29;

  // Compiler parameters

  MAXSTRLENGTH          = 80;
  MAXSTDTOKENLENGTH     = 9;
  MAXNAMELENGTH         = 32;
  MAXIDENTS             = 1000;
  MAXTYPES              = 1000;
  MAXBLOCKS             = 128;    // Must be a multiple of 8
  MAXNESTING            = 10;
  MAXPARAMS             = 20;
  MAXUNITNESTING        = 5;
  MAXFIELDS             = 100;

  PSPSIZE               = $100;
  SEGMENTSIZE           = $10000;
  MAXSTATICSTRDATASIZE  = $4000;

  // Compilation pass codes

  CALLDETERMPASS        = 1;
  CODEGENERATIONPASS    = 2;

  // Scope levels

  GLOBAL                = 1;
  LOCAL                 = 2;

  // Parameter passing

  VALPASSING            = 1;
  CONSTPASSING          = 2;
  VARPASSING            = 3;



type
  TString  = string [MAXSTRLENGTH];
  TKeyName = string [MAXSTDTOKENLENGTH];
  TName    = string [MAXNAMELENGTH];

  TUnit = record
    FileName: TString;
    Pos, Line: Integer;
  end;  
  
  TParam = record
    Name: TName;
    DataType: Byte;
    PassMethod: Byte;
  end;

  PParam = ^TParam;

  TField = record
    Name: TName;
    DataType: Byte;
    Offset: Integer;
  end;

  TType = record
    Block: Byte;
    case TypeKind: Byte of
      SUBRANGETYPE:
        (HostType: Byte;
         Low, High: Integer);
      POINTERTYPE, ARRAYTYPE:
        (BaseType, IndexType: Byte);
      RECORDTYPE:
        (NumFields: Integer;
         Field: array [1..MAXFIELDS] of ^TField);
      FORWARDTYPE:
        (TypeIdentName: TName);   
  end;

  TConst = record
    case Kind: Byte of
      INTNUMBERTOK:
        (Value: LongInt);
      FRACNUMBERTOK:
        (FracValue: Single);
  end;

  TToken = record
    Kind: Byte;
    Name: TName;
    Value: LongInt;
    FracValue: Single;
    StrAddress: Integer;
    StrLength: Integer;
  end;

  TIdentifier = record
    Kind: Byte;
    Name: TName;
    Value: LongInt;                    // Value for a constant, address for a variable, procedure or function
    FracValue: Single;
    Block: Byte;                       // Index of a block in which the identifier is defined
    NestingLevel: Byte;
    DataType: Byte;
    RecType: Byte;                     // Parent record type code for a field
    Scope: Byte;
    PassMethod: Byte;                  // Value, CONST or VAR parameter status
    NumParams: Integer;
    Param: array [1..MAXPARAMS] of PParam;
    ProcAsBlock: Byte;
    PredefIndex: Byte;
    IsUnresolvedForward: Boolean;
  end;



const
  Keyword: array [1..NUMKEYWORDS] of TKeyName =
    (
    'AND',
    'ARRAY',
    'BEGIN',
    'CASE',
    'CONST',
    'DIV',
    'DO',
    'DOWNTO',
    'ELSE',
    'END',
    'FOR',
    'FUNCTION',
    'IF',
    'MOD',
    'NIL',
    'NOT',
    'OF',
    'OR',
    'PROCEDURE',
    'PROGRAM',
    'RECORD',
    'REPEAT',
    'SHL',
    'SHR',
    'THEN',
    'TO',
    'TYPE',
    'UNTIL',
    'VAR',
    'WHILE',
    'XOR'
    );



var
  Ident: array [1..MAXIDENTS] of TIdentifier;
  Types: array [1..MAXTYPES] of TType;
  UnitStack: array [1..MAXUNITNESTING] of TUnit;
  StaticStringData: array [0..MAXSTATICSTRDATASIZE - 1] of Char;
  CodePosStack: array [0..1023] of Integer;
  BlockStack: array [1..MAXNESTING] of Byte;
  CallGraph: array [0..MAXBLOCKS - 1, 0..MAXBLOCKS div 8 - 1] of Byte;    // Rows are callers, columns are callees
  BlockIsNotDead: array [1..MAXBLOCKS] of Boolean;

  Tok: TToken;

  NumIdent, NumTypes, NumStaticStrChars, VarDataOrigin, NumBlocks, BlockStackTop,
  CodeSize, CodePosStackTop, GlobalDataSize,
  Pass, UnitStackTop, Line: Integer;

  ProgramName, ExeName: TString;

  InFile: file of Char;
  OutFile: file of Byte;

  EndOfProgram: Boolean;

  ch, ch2: Char;



  
// ----- GENERAL ROUTINES -----




procedure DisposeAll;
var
  i, j: Integer;
begin
// Dispose dynamically allocated parameter data
for i := 1 to NumIdent do
  if Ident[i].Kind in [PROC, FUNC] then
    for j := 1 to Ident[i].NumParams do
      Dispose(Ident[i].Param[j]);

// Dispose dynamically allocated field data
for i := 1 to NumTypes do
  if Types[i].TypeKind = RECORDTYPE then
    for j := 1 to Types[i].NumFields do
      Dispose(Types[i].Field[j]);
end;




  
procedure Error(const Msg: string);
begin
WriteLn('Error ', UnitStack[UnitStackTop].FileName, ' ', Line, ': ', Msg);
WriteLn;
DisposeAll;
Close(InFile);
Close(OutFile);
Halt(1);
end;





function GetKeyword(const S: TKeyName): Integer;
var
  Max, Mid, Min: Integer;
  Found: Boolean;
begin
Result := 0;

// Binary search
Min := 1;
Max := NUMKEYWORDS;

repeat
  Mid := (Min + Max) div 2;
  if S > Keyword[Mid] then
    Min := Mid + 1
  else
    Max := Mid - 1;
  Found := S = Keyword[Mid];
until Found or (Min > Max);

if Found then Result := NUMDELIMITERS + Mid;
end;





function GetIdentUnsafe(const S: TName): Integer;
var
  IdentIndex, BlockStackIndex: Integer;
begin
Result := 0;

BlockStackIndex := BlockStackTop;
while (BlockStackIndex > 0) and (Result = 0) do
  begin

  IdentIndex := NumIdent;
  while (IdentIndex > 0) and (Result = 0) do
    begin
    if (Ident[IdentIndex].Name = S) and (Ident[IdentIndex].Block = BlockStack[BlockStackIndex]) then Result := IdentIndex;
    Dec(IdentIndex);
    end;// while

  Dec(BlockStackIndex);
  end;// while
end;




function GetIdent(const S: TName): Integer;
begin
Result := GetIdentUnsafe(S);
if Result = 0 then
  Error('Unknown identifier: ' + S);
end;




function GetField(RecType: Byte; const S: TName): Integer;
var
  FieldIndex: Integer;
begin
Result := 0;

FieldIndex := 1;
while (FieldIndex <= Types[RecType].NumFields) and (Result = 0) do
  begin
  if Types[RecType].Field[FieldIndex]^.Name = S then Result := FieldIndex;
  Inc(FieldIndex);
  end;// while

if Result = 0 then
  Error('Unknown field: ' + S);
end;      




function GetSpelling(var Tok: TToken): TString;
begin
if Tok.Kind = 0 then
  Result := 'no token'
else if Tok.Kind <= NUMDELIMITERS then
  case Tok.Kind of
    OPARTOK:        Result := '(';
    CPARTOK:        Result := ')';
    MULTOK:         Result := '*';
    PLUSTOK:        Result := '+';
    COMMATOK:       Result := ',';
    MINUSTOK:       Result := '-';
    PERIODTOK:      Result := '.';
    RANGETOK:       Result := '..';
    DIVTOK:         Result := '/';
    COLONTOK:       Result := ':';
    ASSIGNTOK:      Result := ':=';
    SEMICOLONTOK:   Result := ';';
    LTTOK:          Result := '<';
    LETOK:          Result := '<=';
    NETOK:          Result := '<>';
    EQTOK:          Result := '=';
    GTTOK:          Result := '>';
    GETOK:          Result := '>=';
    ADDRESSTOK:     Result := '@';
    OBRACKETTOK:    Result := '[';
    CBRACKETTOK:    Result := ']';
    DEREFERENCETOK: Result := '^';
  end // case
else if Tok.Kind <= NUMDELIMITERS + NUMKEYWORDS then
  Result := Keyword[Tok.Kind - NUMDELIMITERS]
else if Tok.Kind = IDENTTOK then
  Result := 'identifier'
else if (Tok.Kind = INTNUMBERTOK) or (Tok.Kind = FRACNUMBERTOK) then
  Result := 'number'
else if (Tok.Kind = CHARLITERALTOK) or (Tok.Kind = STRINGLITERALTOK) then
  Result := 'literal'
else
  Result := 'unknown token';
end;





procedure DefineStaticString(var Tok: TToken; const StrValue: TString);
var
  i: Integer;
begin
Tok.StrAddress := NumStaticStrChars;
Tok.StrLength := Length(StrValue);

for i := 1 to Length(StrValue) do
  begin
  StaticStringData[NumStaticStrChars] := StrValue[i];
  Inc(NumStaticStrChars);
  if NumStaticStrChars > MAXSTATICSTRDATASIZE - 1 then
    Error('Maximum string data size exceeded');
  end;

// Add string termination character
StaticStringData[NumStaticStrChars] := #0;
Inc(NumStaticStrChars);
end;





function LowBound(DataType: Byte): Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := Low(Integer);
  SMALLINTTYPE: Result := Low(SmallInt);
  SHORTINTTYPE: Result := Low(ShortInt);
  CHARTYPE:     Result := 0;
  BOOLEANTYPE:  Result := -1;
  SUBRANGETYPE: Result := Types[DataType].Low;
else
  Error('Ordinal type expected');
end;// case
end;

                        



function HighBound(DataType: Byte): Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := High(Integer);
  SMALLINTTYPE: Result := High(SmallInt);
  SHORTINTTYPE: Result := High(ShortInt);
  CHARTYPE:     Result := 255;
  BOOLEANTYPE:  Result := 0;
  SUBRANGETYPE: Result := Types[DataType].High;
else
  Error('Ordinal type expected');
end;// case
end;





function TypeSize(DataType: Byte): Integer;
var
  i: Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := SizeOf(Integer);
  SMALLINTTYPE: Result := SizeOf(SmallInt);
  SHORTINTTYPE: Result := SizeOf(ShortInt);
  CHARTYPE:     Result := SizeOf(Char);
  BOOLEANTYPE:  Result := SizeOf(Boolean);
  REALTYPE:     Result := SizeOf(Single);
  POINTERTYPE:  Result := SizeOf(Pointer);
  TEXTTYPE:     Result := SizeOf(Integer);
  ARRAYTYPE:    Result := (HighBound(Types[DataType].IndexType) - LowBound(Types[DataType].IndexType) + 1) * TypeSize(Types[DataType].BaseType);
  RECORDTYPE:   begin
                Result := 0;
                for i := 1 to Types[DataType].NumFields do
                  Result := Result + TypeSize(Types[DataType].Field[i]^.DataType);
                end;
  SUBRANGETYPE: Result := SizeOf(Integer);               
else
  Error('Illegal type');
end;// case
end;    




function GetCompatibleType(LeftType, RightType: Byte): Byte;
begin
Result := 0;

if LeftType = RightType then                 // General rule
  Result := LeftType
else                                         // Special cases
  begin
  // Untyped pointers compatible with any other pointers
  if (Types[LeftType].TypeKind = POINTERTYPE) and (Types[RightType].TypeKind = POINTERTYPE) and
     ((Types[LeftType].BaseType = ANYTYPE) or (Types[RightType].BaseType = ANYTYPE)) then
    Result := LeftType;

  // Subranges compatible with their host types
  if Types[LeftType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(Types[LeftType].HostType, RightType);
  if Types[RightType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(LeftType, Types[RightType].HostType);

  // Integers
  if (Types[LeftType].TypeKind in IntegerTypes) and
     (Types[RightType].TypeKind in IntegerTypes) then
    Result := LeftType;

  // Booleans
  if (Types[LeftType].TypeKind = BOOLEANTYPE) and
     (Types[RightType].TypeKind = BOOLEANTYPE) then
    Result := LeftType;

  // Characters
  if (Types[LeftType].TypeKind = CHARTYPE) and
     (Types[RightType].TypeKind = CHARTYPE) then
    Result := LeftType;
  end;// if

if Result = 0 then
  Error('Incompatible types');  
end;




function ConversionIsPossible(SrcType, DestType: Byte): Boolean;
begin
// Implicit type conversion is possible if DestType is real and SrcType is integer or a subrange of integer
Result := (Types[DestType].TypeKind = REALTYPE) and
          ((Types[SrcType].TypeKind in IntegerTypes) or
           ((Types[SrcType].TypeKind = SUBRANGETYPE) and (Types[Types[SrcType].HostType].TypeKind in IntegerTypes)));
end;





procedure AssertIdent;
begin
if Tok.Kind <> IDENTTOK then
  Error('Identifier expected but ' + GetSpelling(Tok) + ' found');
end;




procedure CheckOperator(op: Byte; DataType: Byte);
begin
if Types[DataType].TypeKind = SUBRANGETYPE then
  CheckOperator(op, Types[DataType].HostType)
else if (not (Types[DataType].TypeKind in (OrdinalTypes + [REALTYPE, POINTERTYPE]))) or
   ((Types[DataType].TypeKind = REALTYPE) and
       not (op in [MULTOK, DIVTOK, PLUSTOK, MINUSTOK, GTTOK, GETOK, EQTOK, NETOK, LETOK, LTTOK])) or
   ((Types[DataType].TypeKind in IntegerTypes) and
       not (op in [MULTOK, IDIVTOK, MODTOK, SHLTOK, SHRTOK, ANDTOK, PLUSTOK, MINUSTOK, ORTOK, XORTOK, NOTTOK, GTTOK, GETOK, EQTOK, NETOK, LETOK, LTTOK])) or
   ((Types[DataType].TypeKind = CHARTYPE) and
       not (op in [GTTOK, GETOK, EQTOK, NETOK, LETOK, LTTOK])) or
   ((Types[DataType].TypeKind = BOOLEANTYPE) and
       not (op in [ANDTOK, ORTOK, XORTOK, NOTTOK, GTTOK, GETOK, EQTOK, NETOK, LETOK, LTTOK])) or
   ((Types[DataType].TypeKind = POINTERTYPE) and
       not (op in [GTTOK, GETOK, EQTOK, NETOK, LETOK, LTTOK]))
then
  Error('Operator is not applicable');
end;  




procedure AddCallGraphChild(ParentBlock, ChildBlock: Integer);
begin
// Set bit at ParentBlock row, ChildBlock column
CallGraph[ParentBlock, ChildBlock div 8] := CallGraph[ParentBlock, ChildBlock div 8] or (1 shl (ChildBlock mod 8));
end;






// ----- SCANNER -----



procedure InitScanner;
begin
EndOfProgram := FALSE;
UnitStackTop := 1;
UnitStack[UnitStackTop].FileName := ProgramName;
Assign(InFile, ProgramName);
Reset(InFile);

if IOResult <> 0 then
  Error('Unable to open source file ' + ProgramName);

Line := 1;

ch  := ' ';
ch2 := ' ';
end;




procedure EnterIncludedFile(const Name: TString);
begin
UnitStack[UnitStackTop].Pos := FilePos(InFile);
UnitStack[UnitStackTop].Line := Line;

Close(InFile);
Assign(InFile, Name);
Reset(InFile);

if IOResult <> 0 then
  Error('Unable to open source file ' + Name);

Inc(UnitStackTop);
UnitStack[UnitStackTop].FileName := Name;

Line := 1;
end;




procedure LeaveIncludedFile(var ch: Char);
begin
if UnitStackTop > 1 then 
  begin
  Dec(UnitStackTop);
  Assign(InFile, UnitStack[UnitStackTop].FileName);
  Reset(InFile);
  Seek(InFile, UnitStack[UnitStackTop].Pos);
  Line := UnitStack[UnitStackTop].Line;
  Read(InFile, ch);
  end
else
  begin
  EndOfProgram := TRUE;
  ch := #0;
  end;
end;




procedure ReadChar(var ch: Char);
begin
if EndOfProgram then
  ch := #0
else
  if EOF(InFile) then
    begin
    Close(InFile);
    LeaveIncludedFile(ch);
    end
  else
    Read(InFile, ch);
if ch = #10 then Inc(Line);                            // End of line found
end;




procedure ReadValidChar(var ch: Char);
begin
ReadChar(ch);
ch := UpCase(ch);
end;




procedure ReadLiteralChar(var ch: Char);
begin
ReadChar(ch);
if (ch = #0) or (ch = #10) then
  Error('Unterminated string');
end;





procedure ReadSingleLineComment;
begin
while (ch <> #10) and not EndOfProgram do
  ReadChar(ch);
end;




procedure ReadMultiLineComment;
begin
while (ch <> '}') and not EndOfProgram do
  ReadChar(ch);
end;




procedure ReadDirective;
var
  Text: TString;
begin
ReadChar(ch);
if UpCase(ch) = 'I' then                              // Include directive found
  begin
  Text := '';
  ReadChar(ch);
  while (ch <> '}') and not EndOfProgram do
    begin
    if not (ch in [#1..#31, ' ']) then Text := Text + ch;
    ReadChar(ch);
    end;
  EnterIncludedFile(Text);
  end
else
  Error('Unknown compiler directive');
end;




procedure ReadHexadecimalNumber;
var
  Num: Integer;
  NumFound: Boolean;
begin
Num := 0;

NumFound := FALSE;
while ch in ['0'..'9', 'A'..'F'] do
  begin
  if ch in ['0'..'9'] then
    Num := 16 * Num + Ord(ch) - Ord('0')
  else
    Num := 16 * Num + Ord(ch) - Ord('A') + 10;
  NumFound := TRUE;
  ReadValidChar(ch);
  end;

if not NumFound then
  Error('Hexadecimal constant is not found');

Tok.Kind := INTNUMBERTOK;
Tok.Value := Num;
end;




procedure ReadDecimalNumber;
var
  Num, Expon: Integer;
  Frac, FracWeight: Single;
  NegExpon, RangeFound, ExponFound: Boolean;
begin
Num := 0;
Frac := 0;
Expon := 0;
NegExpon := FALSE;

while ch in ['0'..'9'] do
  begin
  Num := 10 * Num + Ord(ch) - Ord('0');
  ReadValidChar(ch);
  end;

if (ch <> '.') and (ch <> 'E') then                                   // Integer number
  begin
  Tok.Kind := INTNUMBERTOK;
  Tok.Value := Num;
  end
else
  begin

  // Check for '..' token
  RangeFound := FALSE;
  if ch = '.' then
    begin
    ReadValidChar(ch2);
    if ch2 = '.' then                                                 // Integer number followed by '..' token
      begin
      Tok.Kind := INTNUMBERTOK;
      Tok.Value := Num;
      RangeFound := TRUE;
      end;
    if not EndOfProgram then Seek(InFile, FilePos(InFile) - 1);
    end; // if ch = '.'
    
  if not RangeFound then                                              // Fractional number
    begin

    // Check for fractional part
    if ch = '.' then
      begin
      FracWeight := 0.1;
      ReadValidChar(ch);

      while ch in ['0'..'9'] do
        begin
        Frac := Frac + FracWeight * (Ord(ch) - Ord('0'));
        FracWeight := FracWeight / 10;
        ReadValidChar(ch);
        end;
      end; // if ch = '.'

    // Check for exponent
    if ch = 'E' then
      begin
      ReadValidChar(ch);

      // Check for exponent sign
      if ch = '+' then
        ReadValidChar(ch)
      else if ch = '-' then
        begin
        NegExpon := TRUE;
        ReadValidChar(ch);
        end;

      ExponFound := FALSE;
      while ch in ['0'..'9'] do
        begin
        Expon := 10 * Expon + Ord(ch) - Ord('0');
        ReadValidChar(ch);
        ExponFound := TRUE;
        end;

      if not ExponFound then
        Error('Exponent is not found');

      if NegExpon then Expon := -Expon;
      end; // if ch = 'E'

    Tok.Kind := FRACNUMBERTOK;
    Tok.FracValue := (Num + Frac) * exp(Expon * ln(10));
    end; // if not RangeFound
  end; // else
end;




procedure ReadNumber;
begin
if ch = '$' then
  begin
  ReadValidChar(ch);
  ReadHexadecimalNumber;
  end
else
  ReadDecimalNumber;
end;    




procedure ReadCharCode;
begin
ReadValidChar(ch);

if not (ch in ['0'..'9', '$']) then
  Error('Character code is not found');

ReadNumber;

if Tok.Kind = FRACNUMBERTOK then
  Error('Integer character code expected');

Tok.Kind := CHARLITERALTOK;
end;




procedure ReadKeywordOrIdentifier;
var
  Text: TString;
  CurToken: Integer;
begin
Text := '';
repeat
  Text := Text + ch;
  ReadValidChar(ch);
until not (ch in ['A'..'Z', '_', '0'..'9']);

CurToken := GetKeyword(Text);
if CurToken <> 0 then               // Keyword found
  Tok.Kind := CurToken
else
  begin                             // Identifier found
  Tok.Kind := IDENTTOK;
  Tok.Name := Text;
  end;
end;




procedure ReadCharOrStringLiteral;
var
  Text: TString;
  EndOfLiteral: Boolean;
begin
Text := '';
EndOfLiteral := FALSE;

repeat
  ReadLiteralChar(ch);
  if ch <> '''' then
    Text := Text + ch
  else
    begin
    Read(InFile, ch2);
    if ch2 = '''' then                                                      // Apostrophe character found
      Text := Text + ch
    else
      begin
      if not EndOfProgram then Seek(InFile, FilePos(InFile) - 1);           // Discard ch2
      EndOfLiteral := TRUE;
      end;
    end;
until EndOfLiteral;

if Length(Text) = 1 then
  begin
  Tok.Kind := CHARLITERALTOK;
  Tok.Value := Ord(Text[1]);
  end
else
  Tok.Kind := STRINGLITERALTOK;

DefineStaticString(Tok, Text);    // A character literal is also copied to a single-character static string so this string can be passed to subroutines

ReadValidChar(ch);
end;




procedure NextTok;
var
  DivFound: Boolean;
begin
Tok.Kind := 0;

// Skip spaces, comments, directives
DivFound := FALSE;
while (ch in [#1..#31, ' ', '{']) or ((ch = '/') and not DivFound) do
  begin
  if ch = '{' then                                                      // Multi-line comment or directive
    begin
    ReadValidChar(ch);
    if ch = '$' then ReadDirective else ReadMultiLineComment;
    end
  else if ch = '/' then
    begin
    ReadValidChar(ch2);
    if ch2 = '/' then
      ReadSingleLineComment                                             // Single-line comment
    else
      begin
      if not EndOfProgram then Seek(InFile, FilePos(InFile) - 2);       // Discard ch and ch2
      DivFound := TRUE;
      end;
    end;
  ReadValidChar(ch);
  end;

// Read token
case ch of
  '0'..'9', '$':
    ReadNumber;
  '#':
    ReadCharCode;
  'A'..'Z', '_':
    ReadKeywordOrIdentifier;
  '''':
    ReadCharOrStringLiteral;
  ':':                              // Single- or double-character tokens
    begin
    Tok.Kind := COLONTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := ASSIGNTOK;
      ReadValidChar(ch);
      end;
    end;
  '>':
    begin
    Tok.Kind := GTTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := GETOK;
      ReadValidChar(ch);
      end;
    end;
  '<':
    begin
    Tok.Kind := LTTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := LETOK;
      ReadValidChar(ch);
      end
    else if ch = '>' then
      begin
      Tok.Kind := NETOK;
      ReadValidChar(ch);
      end;
    end;
  '.':
    begin
    Tok.Kind := PERIODTOK;
    ReadValidChar(ch);
    if ch = '.' then
      begin
      Tok.Kind := RANGETOK;
      ReadValidChar(ch);
      end;
    end;
else                                // Single-character tokens
  case ch of
    '=': Tok.Kind := EQTOK;
    ',': Tok.Kind := COMMATOK;
    ';': Tok.Kind := SEMICOLONTOK;
    '(': Tok.Kind := OPARTOK;
    ')': Tok.Kind := CPARTOK;
    '*': Tok.Kind := MULTOK;
    '/': Tok.Kind := DIVTOK;
    '+': Tok.Kind := PLUSTOK;
    '-': Tok.Kind := MINUSTOK;
    '^': Tok.Kind := DEREFERENCETOK;
    '@': Tok.Kind := ADDRESSTOK;
    '[': Tok.Kind := OBRACKETTOK;
    ']': Tok.Kind := CBRACKETTOK;
  else
    Error('Unexpected end of program');
  end; // case

  ReadValidChar(ch);
end; // case

end; // NextTok





procedure CheckTok(ExpectedTokKind: Byte);
var
  ExpectedTok: TToken;
begin
if Tok.Kind <> ExpectedTokKind then
  begin
  ExpectedTok.Kind := ExpectedTokKind;
  Error(GetSpelling(ExpectedTok) + ' expected but ' + GetSpelling(Tok) + ' found');
  end;
end;




procedure EatTok(ExpectedTokKind: Byte);
begin
CheckTok(ExpectedTokKind);
NextTok;
end;






// ----- CODE GENERATOR -----



procedure Gen(b: Byte);
begin
if (Pass = CALLDETERMPASS) or BlockIsNotDead[BlockStack[BlockStackTop]] then
  begin
  Inc(CodeSize);
  if Pass = CODEGENERATIONPASS then
    begin
    if CodeSize > SEGMENTSIZE - PSPSIZE then
      Error('Maximum code size exceeded');
    Write(OutFile, b);
    end;
  end;
end;



procedure GenAt(Pos: LongInt; b: Byte);
var
  PrevPos: LongInt;
begin
if (Pass = CODEGENERATIONPASS) and BlockIsNotDead[BlockStack[BlockStackTop]] then
  begin
  PrevPos := FilePos(OutFile);
  Seek(OutFile, Pos);
  Write(OutFile, b);
  Seek(OutFile, PrevPos);
  end;
end;




procedure GenWord(w: Integer);
begin
Gen(Lo(w)); Gen(Hi(w));
end;




procedure GenWordAt(Pos: LongInt; w: Integer);
begin
GenAt(Pos, Lo(w)); GenAt(Pos + 1, Hi(w));
end;




procedure GenDWord(dw: LongInt);
begin
Gen(Lo(dw)); Gen(Hi(dw));
dw := dw shr 16;
Gen(Lo(dw)); Gen(Hi(dw));
end;




procedure PushConst(Value: LongInt);
begin
Gen($66); Gen($68); GenDWord(Value);                            // push Value
end;




procedure PushVarPtr(Addr: Integer; Scope: Byte; DeltaNesting: Byte);
const
  StaticLinkAddr = 2 * SizeOf(LongInt);
var
  i: Integer;  
begin
case Scope of
  GLOBAL:                                     // Global variable
    begin
    Gen($1E);                                                   // push ds
    Gen($68); GenWord(Addr);                                    // push Addr
    end;
  LOCAL:
    begin
    Gen($16);                                                   // push ss
    if DeltaNesting = 0 then                  // Strictly local variable
      begin
      Gen($8D); Gen($86); GenWord(Addr);                        // lea ax, [bp + Addr]
      end
    else                                      // Intermediate level variable
      begin
      Gen($8B); Gen($76); Gen(StaticLinkAddr);                  // mov si, [bp + StaticLinkAddr]
      for i := 1 to DeltaNesting - 1 do
        begin
        Gen($36); Gen($8B); Gen($74); Gen(StaticLinkAddr);      // mov si, ss:[si + StaticLinkAddr]
        end;
      Gen($8D); Gen($84); GenWord(Addr);                        // lea ax, [si + Addr]
      end;
    Gen($50);                                                   // push ax
    end;// if
end;// case
end;




procedure DerefPtr(DataType: Byte);
begin
Gen($5E);                                                       // pop si
Gen($07);                                                       // pop es

case TypeSize(DataType) of
  1: begin
     Gen($26); Gen($8A); Gen($04);                              // mov al, es:[si]
     Gen($98);                                                  // cbw
     Gen($66); Gen($98);                                        // cwde
     end;
  2: begin
     Gen($26); Gen($8B); Gen($04);                              // mov ax, es:[si]
     Gen($66); Gen($98);                                        // cwde
     end;
  4: begin
     Gen($66); Gen($26); Gen($8B); Gen($04);                    // mov eax, es:[si]
     end;
else
  Error('Internal fault: Illegal designator size');
end;

Gen($66); Gen($50);                                             // push eax
end;




procedure GetArrayElementPtr(ArrType: Byte);

  function Log2(x: LongWord): ShortInt;
  var
    i: Integer;
  begin
  Result := -1;
  i := 0;
  while (i <= 31) and (Result = -1) do
    begin
    if x = 1 shl i then Result := i;
    Inc(i);
    end;
  end;

var
  BaseTypeSize, IndexLowBound: Integer;
  Log2BaseTypeSize: ShortInt;

begin
Gen($66); Gen($58);                                             // pop eax           ; Array index

IndexLowBound := LowBound(Types[ArrType].IndexType);
if IndexLowBound = 1 then
  Gen($48)                                                      // dec ax
else if IndexLowBound <> 0 then
  begin
  Gen($2D); GenWord(IndexLowBound);                             // sub ax, IndexLowBound
  end;

BaseTypeSize := TypeSize(Types[ArrType].BaseType);
Log2BaseTypeSize := Log2(BaseTypeSize);

if Log2BaseTypeSize > 0 then
  begin
  Gen($C1); Gen($E0); Gen(Log2BaseTypeSize);                    // shl ax, Log2BaseTypeSize
  end
else if Log2BaseTypeSize < 0 then
  begin
  Gen($B9); GenWord(BaseTypeSize);                              // mov cx, BaseTypeSize
  Gen($F7); Gen($E1);                                           // mul cx
  end;

Gen($5B);                                                       // pop bx            ; Array base offset
Gen($03); Gen($D8);                                             // add bx, ax
Gen($53);                                                       // push bx
end;




procedure GetFieldPtr(RecType: Byte; FieldIndex: Integer);
var
  Offset: Integer;
begin
Offset := Types[RecType].Field[FieldIndex]^.Offset;
if Offset > 0 then
  begin
  Gen($58);                                                       // pop ax
  Gen($05); GenWord(Offset);                                      // add ax, Offset
  Gen($50);                                                       // push ax
  end;
end;




procedure SaveStackTop;
begin
Gen($66); Gen($5A);                                                   // pop edx
end;




procedure RestoreStackTop;
begin
Gen($66); Gen($52);                                                   // push edx
end;



procedure SaveFileHandle;
begin
Gen($66); Gen($5F);                                                   // pop edi
Gen($66); Gen($58);                                                   // pop eax  ; pop and discard unnecessary stream handle, i.e. 0
Gen($66); Gen($58);                                                   // pop eax  ; pop and discard unnecessary console handle, i.e. 0
end;




procedure RestoreFileHandle;
begin
Gen($66); Gen($57);                                                   // push edi
end;




procedure GenerateIncDec(proc, Size: Byte);
begin
Gen($5E);                                                             // pop si
Gen($07);                                                             // pop es

case Size of
  1: begin
     Gen($26); Gen($FE);                                              // ... byte ptr es: ...
     end;
  2: begin
     Gen($26); Gen($FF);                                              // ... word ptr es: ...
     end;
  4: begin
     Gen($66); Gen($26); Gen($FF);                                    // ... dword ptr es: ...
     end;
  end;

case proc of
  INCPROC: Gen($04);                                                  // inc ... [si]
  DECPROC: Gen($0C);                                                  // dec ... [si]
  end;
end;




procedure GenerateInpOutp(proc: Byte);
begin
case proc of
  INPPROC:
    begin
    Gen($5E);                                                           // pop si
    Gen($07);                                                           // pop es
    Gen($66); Gen($5A);                                                 // pop edx
    Gen($EC);                                                           // in al, dx
    Gen($26); Gen($88); Gen($04);                                       // mov es:[si], al
    end;
  OUTPPROC:
    begin
    Gen($66); Gen($5B);                                                 // pop ebx
    Gen($66); Gen($5A);                                                 // pop edx
    Gen($8A); Gen($C3);                                                 // mov al, bl    
    Gen($EE);                                                           // out dx, al
    end;
end;// case
end;




procedure GenerateNewDispose(proc: Byte; Size: Integer);
begin
Gen($5E);                                                               // pop si
Gen($07);                                                               // pop es
case proc of
  NEWPROC:
    begin
    Gen($B4); Gen($48);                                                 // mov ah, 48h
    Gen($BB); GenWord((Size - 1) div 16 + 1);                           // mov bx, (Size - 1) div 16 + 1  ; paragraphs to allocate
    Gen($CD); Gen($21);                                                 // int 21h
    Gen($66); Gen($C1); Gen($E0); Gen(16);                              // shl eax, 16                    ; get total address from segment address
    Gen($66); Gen($26); Gen($89); Gen($04);                             // mov es:[si], eax
    end;
  DISPOSEPROC:
    begin
    Gen($B4); Gen($49);                                                 // mov ah, 49h
    Gen($26); Gen($C4); Gen($34);                                       // les si, es:[si]
    Gen($CD); Gen($21);                                                 // int 21h
    end;
end;// case
end;// GenerateNewDispose    




procedure GenerateHalt(err: Byte);
begin
Gen($B4); Gen($4C);                                                     // mov ah, 4Ch
Gen($B0); Gen(err);                                                     // mov al, err
Gen($CD); Gen($21);                                                     // int 21h
end;// GenerateHalt




procedure GenerateInterrupt(InterruptNumber: Byte);
begin
Gen($5E);                                                               // pop si
Gen($07);                                                               // pop es
Gen($26); Gen($8B); Gen($44); Gen($10);                                 // mov ax, es:[si + 16]   ; ds
Gen($1E);                                                               // push ds
Gen($8E); Gen($D8);                                                     // mov ds, ax
Gen($26); Gen($8B); Gen($04);                                           // mov ax, es:[si]
Gen($26); Gen($8B); Gen($5C); Gen($04);                                 // mov bx, es:[si + 4]
Gen($26); Gen($8B); Gen($4C); Gen($08);                                 // mov cx, es:[si + 8]
Gen($26); Gen($8B); Gen($54); Gen($0C);                                 // mov dx, es:[si + 12]
Gen($CD); Gen(InterruptNumber);                                         // int InterruptNumber
Gen($9C);                                                               // pushf
Gen($26); Gen($89); Gen($04);                                           // mov es:[si], ax
Gen($26); Gen($89); Gen($5C); Gen($04);                                 // mov es:[si + 4], bx
Gen($26); Gen($89); Gen($4C); Gen($08);                                 // mov es:[si + 8], cx
Gen($26); Gen($89); Gen($54); Gen($0C);                                 // mov es:[si + 12], dx
Gen($26); Gen($8F); Gen($44); Gen($14);                                 // pop es:[si + 20]       ; flags
Gen($1F);                                                               // pop ds
end;// GenerateInterrupt




procedure GenerateRound(TruncMode: Boolean);
begin
Gen($8B); Gen($DC);                                                   // mov bx, sp
Gen($36); Gen($D9); Gen($07);                                         // fld ss:[bx]   ;  st := operand
if TruncMode then
  begin
  Gen($36); Gen($C7); Gen($87); GenWord(-4); GenWord($0F7F);          // mov ss:[bx - 4], 0F7Fh
  Gen($36); Gen($D9); Gen($AF); GenWord(-4);                          // fldcw ss:[bx - 4]
  end;
Gen($36); Gen($DB); Gen($1F);                                         // fistp ss:[bx] ;  ss:[bx] := round(result);  pop
if TruncMode then
  begin
  Gen($36); Gen($C7); Gen($87); GenWord(-4); GenWord($037F);          // mov ss:[bx - 4], 037Fh
  Gen($36); Gen($D9); Gen($AF); GenWord(-4);                          // fldcw ss:[bx - 4]
  end;
end;// GenerateRound




procedure GenerateFloat(Depth: Byte);
begin
Gen($8B); Gen($DC);                                                   // mov bx, sp

if Depth > 0 then
  begin
  Gen($83); Gen($C3); Gen(Depth);                                     // add bx, Depth
  end;

Gen($36); Gen($DB); Gen($07);                                         // fild ss:[bx]  ;  st := float(operand)
Gen($36); Gen($D9); Gen($1F);                                         // fstp ss:[bx]  ;  ss:[bx] := result;  pop
end;// GenerateFloat




procedure GenerateMathFunction(func, ResultType: Byte);
begin
if Types[ResultType].TypeKind = REALTYPE then       // Real type
  begin
  Gen($8B); Gen($DC);                                                 // mov bx, sp
  Gen($36); Gen($D9); Gen($07);                                       // fld ss:[bx]  ;  st := x
  case func of
    ABSFUNC:
      begin
      Gen($D9); Gen($E1);                                             // fabs
      end;
    SQRFUNC:
      begin
      Gen($DC); Gen($C8);                                             // fmul st, st
      end;
    SINFUNC:
      begin
      Gen($D9); Gen($FE);                                             // fsin
      end;
    COSFUNC:
      begin
      Gen($D9); Gen($FF);                                             // fcos
      end;
    ARCTANFUNC:
      begin
      Gen($D9); Gen($E8);                                             // fld1
      Gen($D9); Gen($F3);                                             // fpatan    ; st := arctan(x / 1.0)
      end;
    EXPFUNC:
      begin
      Gen($D9); Gen($EA);                                             // fldl2e
      Gen($DE); Gen($C9);                                             // fmul
      Gen($D9); Gen($C0);                                             // fld st
      Gen($D9); Gen($FC);                                             // frndint
      Gen($DD); Gen($D2);                                             // fst st(2) ; st(2) := round(x * log2(e))
      Gen($DE); Gen($E9);                                             // fsub
      Gen($D9); Gen($F0);                                             // f2xm1     ; st := 2 ^ frac(x * log2(e)) - 1
      Gen($D9); Gen($E8);                                             // fld1
      Gen($DE); Gen($C1);                                             // fadd
      Gen($D9); Gen($FD);                                             // fscale    ; st := 2 ^ frac(x * log2(e)) * 2 ^ round(x * log2(e)) = exp(x)
      end;
    LNFUNC:
      begin
      Gen($D9); Gen($ED);                                             // fldln2
      Gen($D9); Gen($C9);                                             // fxch
      Gen($D9); Gen($F1);                                             // fyl2x     ; st := ln(2) * log2(x) = ln(x)
      end;
    SQRTFUNC:
      begin
      Gen($D9); Gen($FA);                                             // fsqrt
      end;

  end;// case

  Gen($36); Gen($D9); Gen($1F);                                       // fstp ss:[bx] ;  ss:[bx] := result;  pop
  end
else                                // Ordinal types
  case func of
    ABSFUNC:
      begin
      Gen($66); Gen($58);                                             // pop eax
      Gen($66); Gen($83); Gen($F8); Gen($00);                         // cmp eax, 0
      Gen($7D); Gen($03);                                             // jge +3
      Gen($66); Gen($F7); Gen($D8);                                   // neg eax
      Gen($66); Gen($50);                                             // push eax
      end;
    SQRFUNC:
      begin
      Gen($66); Gen($58);                                             // pop eax
      Gen($66); Gen($F7); Gen($E8);                                   // imul eax
      Gen($66); Gen($50);                                             // push eax
      end;
  end;// case
end;// GenerateMathFunction





procedure GenerateUnaryOperator(op: Byte; ResultType: Byte);
begin
if Types[ResultType].TypeKind = REALTYPE then     // Real type
  begin
  if op = MINUSTOK then
    begin
    Gen($8B); Gen($DC);                                                 // mov bx, sp
    Gen($36); Gen($D9); Gen($07);                                       // fld ss:[bx]  ;  st := operand
    Gen($D9); Gen($E0);                                                 // fchs
    Gen($36); Gen($D9); Gen($1F);                                       // fstp ss:[bx] ;  ss:[bx] := result;  pop
    end;
  end
else                                              // Ordinal types
  begin
  Gen($66); Gen($58);                                                   // pop eax
  case op of
    MINUSTOK:
      begin
      Gen($66); Gen($F7); Gen($D8);                                     // neg eax
      end;
    NOTTOK:
      begin
      Gen($66); Gen($F7); Gen($D0);                                     // not eax
      end;
  end;// case
  Gen($66); Gen($50);                                                   // push eax
  end;// else
  
end;




procedure GenerateBinaryOperator(op: Byte; ResultType: Byte);
begin
if Types[ResultType].TypeKind = REALTYPE then     // Real type
  begin
  Gen($8B); Gen($DC);                                                   // mov bx, sp
  Gen($36); Gen($D9); Gen($07);                                         // fld ss:[bx]  ;  st := operand2
  Gen($66); Gen($58);                                                   // pop eax
  Gen($8B); Gen($DC);                                                   // mov bx, sp
  Gen($36); Gen($D9); Gen($07);                                         // fld ss:[bx]  ;  st(1) := operand2;  st := operand1
  Gen($D9); Gen($C9);                                                   // fxch         ;  st := operand2;  st(1) := operand1

  case op of
    PLUSTOK:
      begin
      Gen($DE); Gen($C1);                                               // fadd  ;  st(1) := st(1) + st;  pop
      end;
    MINUSTOK:
      begin
      Gen($DE); Gen($E9);                                               // fsub  ;  st(1) := st(1) - st;  pop
      end;
    MULTOK:
      begin
      Gen($DE); Gen($C9);                                               // fmul  ;  st(1) := st(1) * st;  pop
      end;
    DIVTOK:
      begin
      Gen($DE); Gen($F9);                                               // fdiv  ;  st(1) := st(1) / st;  pop
      end;
  end;// case

  Gen($36); Gen($D9); Gen($1F);                                         // fstp ss:[bx]  ;  ss:[bx] := result;  pop

  end // if
else                              // Ordinal types
  begin
  Gen($66); Gen($59);                                                   // pop ecx
  Gen($66); Gen($58);                                                   // pop eax

  case op of
    PLUSTOK:
      begin
      Gen($66); Gen($03); Gen($C1);                                     // add eax, ecx
      end;
    MINUSTOK:
      begin
      Gen($66); Gen($2B); Gen($C1);                                     // sub eax, ecx
      end;
    MULTOK:
      begin
      Gen($66); Gen($F7); Gen($E9);                                     // imul ecx
      end;
    IDIVTOK, MODTOK:
      begin
      Gen($66); Gen($99);                                               // cdq
      Gen($66); Gen($F7); Gen($F9);                                     // idiv ecx
      if op = MODTOK then
        begin
        Gen($66); Gen($8B); Gen($C2);                                   // mov eax, edx         ; save remainder
        end;
      end;
    SHLTOK:
      begin
      Gen($66); Gen($D3); Gen($E0);                                     // shl eax, cl
      end;
    SHRTOK:
      begin
      Gen($66); Gen($D3); Gen($E8);                                     // shr eax, cl
      end;
    ANDTOK:
      begin
      Gen($66); Gen($23); Gen($C1);                                     // and eax, ecx
      end;
    ORTOK:
      begin
      Gen($66); Gen($0B); Gen($C1);                                     // or eax, ecx
      end;
    XORTOK:
      begin
      Gen($66); Gen($33); Gen($C1);                                     // xor eax, ecx
      end;

  end;// case

  Gen($66); Gen($50);                                                   // push eax
  end;// else
end;




procedure GenerateRelation(rel: Byte; ValType: Byte);
begin
if Types[ValType].TypeKind = REALTYPE then        // Real type
  begin
  Gen($8B); Gen($DC);                                                   // mov bx, sp
  Gen($36); Gen($D9); Gen($07);                                         // fld ss:[bx]    ;  st := operand2
  Gen($66); Gen($58);                                                   // pop eax
  Gen($8B); Gen($DC);                                                   // mov bx, sp
  Gen($36); Gen($D9); Gen($07);                                         // fld ss:[bx]    ;  st(1) := operand2;  st := operand1
  Gen($66); Gen($58);                                                   // pop eax
  Gen($8B); Gen($DC);                                                   // mov bx, sp
  Gen($DE); Gen($D9);                                                   // fcompp         ;  test st - st(1)
  Gen($DF); Gen($E0);                                                   // fstsw ax
  Gen($66); Gen($68); GenDWord(-1);                                     // push FFFFFFFFh ;  TRUE
  Gen($9E);                                                             // sahf
  case rel of
    EQTOK: Gen($74);                                                    // je  ...
    NETOK: Gen($75);                                                    // jne ...
    GTTOK: Gen($77);                                                    // ja  ...
    GETOK: Gen($73);                                                    // jae ...
    LTTOK: Gen($72);                                                    // jb  ...
    LETOK: Gen($76);                                                    // jbe ...
  end;// case
  end
else                              // Ordinal types
  begin
  Gen($66); Gen($59);                                                   // pop ecx
  Gen($66); Gen($58);                                                   // pop eax
  Gen($66); Gen($68); GenDWord(-1);                                     // push FFFFFFFFh ;  TRUE
  Gen($66); Gen($3B); Gen($C1);                                         // cmp eax, ecx
  case rel of
    EQTOK: Gen($74);                                                    // je  ...
    NETOK: Gen($75);                                                    // jne ...
    GTTOK: Gen($7F);                                                    // jg  ...
    GETOK: Gen($7D);                                                    // jge ...
    LTTOK: Gen($7C);                                                    // jl  ...
    LETOK: Gen($7E);                                                    // jle ...
  end;// case
  end;// else

Gen($08);                                                               // ... +8
Gen($66); Gen($59);                                                     // pop ecx
Gen($66); Gen($68); GenDWord(0);                                        // push 00000000h ;  FALSE
end;





procedure GenerateAssignment(DesignatorType: Byte);
begin
// EDX should be preserved

// Source value
Gen($66); Gen($58);                                                     // pop eax
// Destination address
Gen($5E);                                                               // pop si
Gen($07);                                                               // pop es

case TypeSize(DesignatorType) of
  1: begin
     Gen($26); Gen($88); Gen($04);                                      // mov es:[si], al
     end;
  2: begin
     Gen($26); Gen($89); Gen($04);                                      // mov es:[si], ax
     end;
  4: begin
     Gen($66); Gen($26); Gen($89); Gen($04);                            // mov es:[si], eax
     end;
else
  Error('Internal fault: Illegal designator size');
end;

end;




procedure GenerateStructuredAssignment(DesignatorType: Byte);
begin
Gen($8C); Gen($D8);                                                     // mov ax, ds
Gen($8B); Gen($DF);                                                     // mov bx, di   ; edi is used in Write, Read, etc. and should be preserved

// Source address
Gen($5E);                                                               // pop si
Gen($1F);                                                               // pop ds
// Destination address
Gen($5F);                                                               // pop di
Gen($07);                                                               // pop es

// Copy source to destination
Gen($B9); GenWord(TypeSize(DesignatorType));                            // mov cx, TypeSize(DesignatorType)
Gen($FC);                                                               // cld          ; increment si, di after each step
Gen($F3); Gen($A4);                                                     // rep movsb

Gen($8E); Gen($D8);                                                     // mov ds, ax
Gen($8B); Gen($FB);                                                     // mov di, bx
end;






procedure GenerateCall(EntryPoint: LongInt; DeltaNesting: Byte);
const
  StaticLinkAddr = 2 * SizeOf(LongInt);
var
  CodePos: Integer;
  i: Integer;
begin
// Push routine static link as the last hidden parameter (needed for nested routines)
if DeltaNesting = 0 then                       // The caller and the callee's enclosing routine are at the same nesting level
  begin
  Gen($66); Gen($55);                                                   // push ebp
  end
else                                           // The caller is deeper
  begin
  Gen($8B); Gen($76); Gen(StaticLinkAddr);                              // mov si, [bp + StaticLinkAddr]
  for i := 1 to DeltaNesting - 1 do
    begin
    Gen($36); Gen($8B); Gen($74); Gen(StaticLinkAddr);                  // mov si, ss:[si + StaticLinkAddr]
    end;
  Gen($66); Gen($56);                                                   // push esi
  end;

// Call the routine  
Gen($50);                                                               // push ax     ; align stack data on 32-bit bound
CodePos := CodeSize;
Gen($E8); GenWord(EntryPoint - (CodePos + 3));                          // call EntryPoint
Gen($58);                                                               // pop ax      ; align stack data on 32-bit bound
end;




procedure GenerateReturn(TotalParamsSize: Integer);
begin
Gen($C2); GenWord(TotalParamsSize + SizeOf(LongInt));                   // ret TotalParamsSize + 4   ; + 4 is for static link
end;




procedure GenerateIfCondition;
begin
Gen($66); Gen($58);                                         // pop eax
Gen($66); Gen($83); Gen($F8); Gen($00);                     // cmp eax, 0
Gen($75); Gen($03);                                         // jne +3
end;




procedure GenerateWhileCondition;
begin
GenerateIfCondition;
end;



procedure GenerateRepeatCondition;
begin
GenerateIfCondition;
end;




procedure GenerateForCondition(CounterAddress: Integer; Scope, CounterSize: Byte; Down: Boolean);
begin
Gen($66); Gen($59);                                         // pop ecx
Gen($66); Gen($51);                                         // push ecx             ; The final value of the counter will be removed from stack by GenerateForEpilog  
case Scope of
  GLOBAL:
    case CounterSize of
      1: begin
         Gen($A0);                                          // mov al, [...]
         end;
      2: begin
         Gen($A1);                                          // mov ax, [...]
         end;
      4: begin
         Gen($66); Gen($A1);                                // mov eax, [...]
         end;
      end;
  LOCAL:
    case CounterSize of
      1: begin
         Gen($8A); Gen($86);                                // mov al, [bp + ...]
         end;
      2: begin
         Gen($8B); Gen($86);                                // mov ax, [bp + ...]
         end;
      4: begin
         Gen($66); Gen($8B); Gen($86);                      // mov eax, [bp + ...]
         end;
      end;
    end;
    
GenWord(CounterAddress);                                    // ... CounterAddress ...

if CounterSize < 2 then
  Gen($98);                                                 // cbw
if CounterSize < 4 then
  begin
  Gen($66); Gen($98);                                       // cwde
  end;

Gen($66); Gen($3B); Gen($C1);                               // cmp eax, ecx
if Down then
  begin
  Gen($7D); Gen($03);                                       // jge +3
  end
else
  begin
  Gen($7E); Gen($03);                                       // jle +3
  end;
end;




procedure GenerateIfProlog;
begin
Inc(CodePosStackTop);
CodePosStack[CodePosStackTop] := CodeSize;

Gen($90);                                                   // nop   ; jump to the IF block end will be inserted here
Gen($90);                                                   // nop
Gen($90);                                                   // nop
end;




procedure GenerateElseProlog;
var
  CodePos: Integer;
begin
CodePos := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);

GenAt(CodePos, $E9); GenWordAt(CodePos + 1, CodeSize - (CodePos + 3) + 3);  // jmp (IF..THEN block end)

GenerateIfProlog;
end;




procedure GenerateIfElseEpilog;
var
  CodePos: Integer;
begin
CodePos := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);

GenAt(CodePos, $E9); GenWordAt(CodePos + 1, CodeSize - (CodePos + 3));      // jmp (IF..THEN block end)
end;




procedure GenerateCaseProlog;
begin
Gen($66); Gen($59);                                         // pop ecx           ; CASE switch value
Gen($B0); Gen($00);                                         // mov al, 00h       ; initial flag mask
end;




procedure GenerateCaseEpilog(NumCaseStatements: Integer);
var
  i: Integer;
begin
for i := 1 to NumCaseStatements do
  GenerateIfElseEpilog;
end;




procedure GenerateCaseEqualityCheck(Value: LongInt);
begin
Gen($66); Gen($81); Gen($F9); GenDWord(Value);              // cmp ecx, Value
Gen($9F);                                                   // lahf
Gen($0A); Gen($C4);                                         // or al, ah
end;




procedure GenerateCaseRangeCheck(Value1, Value2: LongInt);
begin
Gen($66); Gen($81); Gen($F9); GenDWord(Value1);             // cmp ecx, Value1
Gen($7C); Gen($0B);                                         // jl +11
Gen($66); Gen($81); Gen($F9); GenDWord(Value2);             // cmp ecx, Value2
Gen($7F); Gen($02);                                         // jg +2
Gen($0C); Gen($40);                                         // or al, 40h     ; set zero flag on success
end;




procedure GenerateCaseStatementProlog;
begin
Gen($24); Gen($40);                                         // and al, 40h    ; test zero flag
Gen($75); Gen($03);                                         // jnz +3         ; if set, jump to the case statement
GenerateIfProlog;
end;




procedure GenerateCaseStatementEpilog;
var
  StoredCodeSize: LongInt;
begin
StoredCodeSize := CodeSize;

Gen($90);                                                   // nop   ; jump to the CASE block end will be inserted here
Gen($90);                                                   // nop
Gen($90);                                                   // nop

GenerateIfElseEpilog;

Inc(CodePosStackTop);
CodePosStack[CodePosStackTop] := StoredCodeSize;
end;




procedure GenerateWhileEpilog;
var
  CodePos, CurPos, ReturnPos: Integer;
begin
CodePos := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);

GenAt(CodePos, $E9); GenWordAt(CodePos + 1, CodeSize - (CodePos + 3) + 3);  // jmp (WHILE..DO block end)

ReturnPos := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);

CurPos := CodeSize;

Gen($E9); GenWord(ReturnPos - (CurPos + 3));                                // jmp ReturnPos
end;




procedure GenerateRepeatProlog;
begin
Inc(CodePosStackTop);
CodePosStack[CodePosStackTop] := CodeSize;
end;




procedure GenerateRepeatEpilog;
var
  CurPos, ReturnPos: Integer;
begin
ReturnPos := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);

CurPos := CodeSize;

Gen($E9); GenWord(ReturnPos - (CurPos + 3));                // jmp ReturnPos
end;






procedure GenerateForEpilog(CounterAddress: Integer; Scope, CounterSize: Byte; Down: Boolean);
begin
PushVarPtr(CounterAddress, Scope, 0);
if Down then
  GenerateIncDec(DECPROC, CounterSize)
else
  GenerateIncDec(INCPROC, CounterSize);
GenerateWhileEpilog;

Gen($66); Gen($59);                                         // pop ecx    ; Remove the final value of the counter from the stack
end;




procedure GenerateDeclarationProlog;
begin
GenerateIfProlog;
end;




procedure GenerateDeclarationEpilog;
begin
GenerateIfElseEpilog;
end;




procedure GenerateProgramProlog;
begin
// Initialize segment registers
Gen($8C); Gen($C8);                                         // mov ax, cs
Gen($8E); Gen($C0);                                         // mov es, ax         ; Extra data segment base
Gen($90);                                                   // nop
Gen($90);                                                   // nop
Gen($90);                                                   // nop                ; Reserved space for add ax, CodeSegmentSize
Gen($8E); Gen($D8);                                         // mov ds, ax         ; Data segment base
Gen($05); GenWord($1000);                                   // add ax, 1000h      ; Allocate 64 Kbytes for static data
Gen($8E); Gen($D0);                                         // mov ss, ax         ; Stack segment base

// Initialize FPU
Gen($DB); Gen($E3);                                         // finit

// Shrink allocated memory block to the maximum total size of code, data and stack segments (needed by New and Dispose)
Gen($B4); Gen($4A);                                         // mov ah, 4Ah
Gen($BB); GenWord($3000);                                   // mov bx, 3000h      ; New block size in paragraphs
Gen($CD); Gen($21);                                         // int 21h            ; Block segment address is in ES

end;




procedure GenerateProgramEpilog;
var
  i, StoredCodeSize, CodeSegmentSize: Integer;
begin
GenerateHalt(0);
// End of pure code

StoredCodeSize := CodeSize;

// Complete program prolog
CodeSegmentSize := (PSPSIZE + StoredCodeSize) div 16 + 1;                       // in paragraphs

GenAt(4, $05); GenAt(5, Lo(CodeSegmentSize)); GenAt(6, Hi(CodeSegmentSize));    // add ax, CodeSegmentSize

// Align code segment
for i := 1 to CodeSegmentSize * 16 - (PSPSIZE + StoredCodeSize) do
  Gen($90);                                                                     // nop

// Build static string data table at the end of the executable file (i.e. at the beginning of data segment)
StoredCodeSize := CodeSize;
for i := 0 to NumStaticStrChars - 1 do
  Gen(Ord(StaticStringData[i]));                                                // db StaticStringData[i]
CodeSize := StoredCodeSize;
end;




procedure GenerateStackFrameProlog(TotalLocalVarsSize: Integer);
begin
Gen($66); Gen($55);                                           // push ebp
Gen($66); Gen($8B); Gen($EC);                                 // mov ebp, esp
Gen($66); Gen($81); Gen($EC); GenDWord(TotalLocalVarsSize);   // sub esp, TotalLocalVarsSize
end;





procedure GenerateStackFrameEpilog;
begin
Gen($66); Gen($8B); Gen($E5);                                 // mov esp, ebp
Gen($66); Gen($5D);                                           // pop ebp
end;




procedure GenerateForwardReference;
begin
Gen($90);                                                     // nop   ; jump to the procedure entry point will be inserted here
Gen($90);                                                     // nop
Gen($90);                                                     // nop
end;




procedure GenerateForwardResolution(IdentIndex: Integer);
var
  CodePos: Integer;
begin
CodePos := Ident[IdentIndex].Value;
GenAt(CodePos, $E9); GenWordAt(CodePos + 1, CodeSize - (CodePos + 3));      // jmp Ident[IdentIndex].Value
end;






// ----- PARSER -----



procedure CompileConstExpression(var ConstVal: TConst; var ConstValType: Byte); forward;
procedure CompileDesignator(var ValType: Byte); forward;
procedure CompileExpression(var ValType: Byte); forward;
procedure CompileStatement; forward;
procedure CompileType(var DataType: Byte); forward;




procedure CompileConstFactor(var ConstVal: TConst; var ConstValType: Byte);
var
  IdentIndex: Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind <> CONSTANT then
      Error('Constant expected but ' + Ident[IdentIndex].Name + ' found')
    else
      begin
      ConstValType := Ident[IdentIndex].DataType;
      if Types[ConstValType].TypeKind = REALTYPE then
        ConstVal.FracValue := Ident[IdentIndex].FracValue
      else
        ConstVal.Value := Ident[IdentIndex].Value;
      NextTok;
      end;
    end;


  INTNUMBERTOK:
    begin
    ConstVal.Value := Tok.Value;
    ConstValType := INTEGERTYPEINDEX;
    NextTok;
    end;


  FRACNUMBERTOK:
    begin
    ConstVal.FracValue := Tok.FracValue;
    ConstValType := REALTYPEINDEX;
    NextTok;
    end;


  CHARLITERALTOK:
    begin
    ConstVal.Value := Tok.Value;
    ConstValType := CHARTYPEINDEX;
    NextTok;
    end;


  OPARTOK:       // Expression in parentheses expected
    begin
    NextTok;
    CompileConstExpression(ConstVal, ConstValType);
    EatTok(CPARTOK);
    end;


  NOTTOK:
    begin
    CompileConstFactor(ConstVal, ConstValType);
    ConstVal.Value := not ConstVal.Value;
    end; 

else
  Error('Expression expected but ' + GetSpelling(Tok) + ' found');
end;// case

end;// CompileConstFactor




procedure CompileConstTerm(var ConstVal: TConst; var ConstValType: Byte);
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Byte;

begin
CompileConstFactor(ConstVal, ConstValType);

while Tok.Kind in [MULTOK, DIVTOK, IDIVTOK, MODTOK, SHLTOK, SHRTOK, ANDTOK] do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstFactor(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionIsPossible(ConstValType, REALTYPEINDEX) and ConversionIsPossible(RightConstValType, REALTYPEINDEX) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    RightConstVal.FracValue := RightConstVal.Value;
    ConstValType := REALTYPEINDEX;
    RightConstValType := REALTYPEINDEX;
    end;

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok.Kind, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then        // Real constants
    case OpTok.Kind of
      MULTOK:  ConstVal.FracValue := ConstVal.FracValue * RightConstVal.FracValue;
      DIVTOK:  if RightConstVal.FracValue <> 0 then
                 ConstVal.FracValue := ConstVal.FracValue / RightConstVal.FracValue
               else
                 Error('Constant division by zero');
    end
  else                                                    // Integer constants
    case OpTok.Kind of             
      MULTOK:  ConstVal.Value := ConstVal.Value  *  RightConstVal.Value;
      IDIVTOK: if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value div RightConstVal.Value
               else
                 Error('Constant division by zero');  
      MODTOK:  if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value mod RightConstVal.Value
               else
                 Error('Constant division by zero');
      SHLTOK:  ConstVal.Value := ConstVal.Value shl RightConstVal.Value;
      SHRTOK:  ConstVal.Value := ConstVal.Value shr RightConstVal.Value;
      ANDTOK:  ConstVal.Value := ConstVal.Value and RightConstVal.Value;
    end;

  end;// while

end;// CompileConstTerm



procedure CompileSimpleConstExpression(var ConstVal: TConst; var ConstValType: Byte);
var
  UnaryOpTok, OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Byte;

begin
UnaryOpTok := Tok;
if UnaryOpTok.Kind in [PLUSTOK, MINUSTOK] then
  NextTok;

CompileConstTerm(ConstVal, ConstValType);

if UnaryOpTok.Kind in [PLUSTOK, MINUSTOK] then
  CheckOperator(UnaryOpTok.Kind, ConstValType);

if UnaryOpTok.Kind = MINUSTOK then      // Unary minus
  if Types[ConstValType].TypeKind = REALTYPE then
    ConstVal.FracValue := -ConstVal.FracValue
  else
    ConstVal.Value := -ConstVal.Value;

while Tok.Kind in [PLUSTOK, MINUSTOK, ORTOK, XORTOK] do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstTerm(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;  

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok.Kind, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then       // Real constants
    case OpTok.Kind of
      PLUSTOK:  ConstVal.FracValue := ConstVal.FracValue  +  RightConstVal.FracValue;
      MINUSTOK: ConstVal.FracValue := ConstVal.FracValue  -  RightConstVal.FracValue;
    end
  else                                                  // Integer constants
    case OpTok.Kind of
      PLUSTOK:  ConstVal.Value := ConstVal.Value  +  RightConstVal.Value;
      MINUSTOK: ConstVal.Value := ConstVal.Value  -  RightConstVal.Value;
      ORTOK:    ConstVal.Value := ConstVal.Value  or RightConstVal.Value;
      XORTOK:   ConstVal.Value := ConstVal.Value xor RightConstVal.Value;
    end;

  end;// while

end;// CompileSimpleConstExpression



procedure CompileConstExpression(var ConstVal: TConst; var ConstValType: Byte);
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Byte;
  Yes: Boolean;

begin
Yes := FALSE;
CompileSimpleConstExpression(ConstVal, ConstValType);

if Tok.Kind in [EQTOK, NETOK, LTTOK, LETOK, GTTOK, GETOK] then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleConstExpression(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok.Kind, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then
    case OpTok.Kind of
      EQTOK: Yes := ConstVal.FracValue =  RightConstVal.FracValue;
      NETOK: Yes := ConstVal.FracValue <> RightConstVal.FracValue;
      LTTOK: Yes := ConstVal.FracValue <  RightConstVal.FracValue;
      LETOK: Yes := ConstVal.FracValue <= RightConstVal.FracValue;
      GTTOK: Yes := ConstVal.FracValue >  RightConstVal.FracValue;
      GETOK: Yes := ConstVal.FracValue >= RightConstVal.FracValue;
    end
  else
    case OpTok.Kind of
      EQTOK: Yes := ConstVal.Value =  RightConstVal.Value;
      NETOK: Yes := ConstVal.Value <> RightConstVal.Value;
      LTTOK: Yes := ConstVal.Value <  RightConstVal.Value;
      LETOK: Yes := ConstVal.Value <= RightConstVal.Value;
      GTTOK: Yes := ConstVal.Value >  RightConstVal.Value;
      GETOK: Yes := ConstVal.Value >= RightConstVal.Value;
    end;

  if Yes then ConstVal.Value := -1 else ConstVal.Value := 0;
  
  ConstValType := BOOLEANTYPEINDEX;
  end;

end;// CompileConstExpression




procedure CompilePredefinedProc(proc: Byte);
var
  DesignatorType, ExpressionType, ActualParamType: Byte;
  InterruptNumber, ErrorCode: TConst;
  ExitLoop: Boolean;
  LibProcIdentIndex: Integer;
  IsFirstParam, FileSpecified: Boolean;
begin
NextTok;

case proc of
  INCPROC, DECPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType);
    GetCompatibleType(DesignatorType, INTEGERTYPEINDEX);
    GenerateIncDec(proc, TypeSize(DesignatorType));
    EatTok(CPARTOK);
    end;

  READPROC, READLNPROC:
    begin
    FileSpecified := FALSE;              // By default, use standard output device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          RestoreFileHandle
        else
          PushConst(0);                  // Console handle

        // 2nd argument - string stream handle
        PushConst(0);

        // 3rd argument - designator
        CompileDesignator(DesignatorType);

        if Types[DesignatorType].TypeKind = TEXTTYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types');
          FileSpecified := TRUE;
          DerefPtr(DesignatorType);
          SaveFileHandle;
          end
        else                                                        // Any output expression
          begin
          LibProcIdentIndex := 0;

          if (Types[DesignatorType].TypeKind in IntegerTypes) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and
             (Types[Types[DesignatorType].HostType].TypeKind in IntegerTypes)) then
                LibProcIdentIndex := GetIdent('READINT')                 // Integer or boolean argument
          else if (Types[DesignatorType].TypeKind = CHARTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and
             (Types[Types[DesignatorType].HostType].TypeKind = CHARTYPE)) then
                LibProcIdentIndex := GetIdent('READCH')                  // Character argument
          else if Types[DesignatorType].TypeKind = REALTYPE then
                LibProcIdentIndex := GetIdent('READREAL')                // Real argument
          else if (Types[DesignatorType].TypeKind = ARRAYTYPE) and (Types[DesignatorType].BaseType = CHARTYPEINDEX) then
                LibProcIdentIndex := GetIdent('READSTRING')              // String argument
          else
            Error('Incompatible types');

          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; var Designator
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;
      EatTok(CPARTOK);
      end; // if OPARTOR
      
    // Add CR+LF, if necessary
    if proc = READLNPROC then
      begin
      LibProcIdentIndex := GetIdent('READNEWLINE');
      
      // 1st argument - file handle
      if FileSpecified then
        RestoreFileHandle
      else
        PushConst(0);   // Console handle

      // 2nd argument - string stream handle
      PushConst(0);

      if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
      end;

    end;// READPROC, READLNPROC


  WRITEPROC, WRITELNPROC:
    begin
    FileSpecified := FALSE;              // By default, use standard output device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          RestoreFileHandle
        else
          PushConst(0);                  // Console handle

        // 2nd argument - string stream handle
        PushConst(0);

        // 3rd argument - expression
        CompileExpression(ExpressionType);

        if Types[ExpressionType].TypeKind = TEXTTYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types');
          FileSpecified := TRUE;
          SaveFileHandle;
          end
        else                                                        // Any output expression
          begin
          LibProcIdentIndex := 0;
          
          if (Types[ExpressionType].TypeKind in IntegerTypes) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and
             (Types[Types[ExpressionType].HostType].TypeKind in IntegerTypes)) then
                LibProcIdentIndex := GetIdent('WRITEINT')                 // Integer or boolean argument
          else if (Types[ExpressionType].TypeKind = BOOLEANTYPE) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and
             (Types[Types[ExpressionType].HostType].TypeKind = BOOLEANTYPE)) then
                LibProcIdentIndex := GetIdent('WRITEBOOLEAN')             // Boolean argument
          else if (Types[ExpressionType].TypeKind = CHARTYPE) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and
             (Types[Types[ExpressionType].HostType].TypeKind = CHARTYPE)) then
                LibProcIdentIndex := GetIdent('WRITECH')                  // Character argument
          else if Types[ExpressionType].TypeKind = REALTYPE then
                LibProcIdentIndex := GetIdent('WRITEREAL')                // Real argument
          else if Types[ExpressionType].TypeKind = POINTERTYPE then
                LibProcIdentIndex := GetIdent('WRITEPOINTER')             // Pointer argument
          else if (Types[ExpressionType].TypeKind = ARRAYTYPE) and (Types[ExpressionType].BaseType = CHARTYPEINDEX) then
                LibProcIdentIndex := GetIdent('WRITESTRING')              // String argument
          else
            Error('Incompatible types');

          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; Expression
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;
      EatTok(CPARTOK);
      end; // if OPARTOR
      
    // Add CR+LF, if necessary
    if proc = WRITELNPROC then
      begin
      LibProcIdentIndex := GetIdent('WRITENEWLINE');
      
      // 1st argument - file handle
      if FileSpecified then
        RestoreFileHandle
      else
        PushConst(0);   // Console handle

      // 2nd argument - string stream handle
      PushConst(0);

      if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
      end;

    end;// WRITEPROC, WRITELNPROC
    

  INPPROC, OUTPPROC:
    begin
    EatTok(OPARTOK);
    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, INTEGERTYPEINDEX);
    EatTok(COMMATOK);
    if proc = INPPROC then
      CompileDesignator(ExpressionType)
    else
      CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, SHORTINTTYPEINDEX);
    GenerateInpOutp(proc);
    EatTok(CPARTOK);
    end;

  NEWPROC, DISPOSEPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType);
    GetCompatibleType(DesignatorType, POINTERTYPEINDEX);
    GenerateNewDispose(proc, TypeSize(Types[DesignatorType].BaseType));
    EatTok(CPARTOK);
    end;

  HALTPROC:
    begin
    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      CompileConstExpression(ErrorCode, ExpressionType);
      GetCompatibleType(ExpressionType, INTEGERTYPEINDEX);
      EatTok(CPARTOK);
      end
    else
      ErrorCode.Value := 0;
    GenerateHalt(ErrorCode.Value);
    end;

  INTRPROC:
    begin
    EatTok(OPARTOK);
    CompileConstExpression(InterruptNumber, ActualParamType);
    GetCompatibleType(ActualParamType, INTEGERTYPEINDEX);
    EatTok(COMMATOK);
    CompileExpression(ActualParamType);
    GetCompatibleType(ActualParamType, POINTERTYPEINDEX);
    GenerateInterrupt(InterruptNumber.Value);
    EatTok(CPARTOK);
    end;
end;// case

end;// CompilePredefinedProc




procedure CompilePredefinedFunc(func: Byte; var ValType: Byte);
var
  IdentIndex: Integer;
begin
NextTok;
EatTok(OPARTOK);

case func of
  SIZEOFFUNC:
    begin
    AssertIdent;
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = USERTYPE then
      begin
      NextTok;
      PushConst(TypeSize(Ident[IdentIndex].DataType));
      end
    else
      begin
      CompileDesignator(ValType);
      SaveStackTop;                        // Save result to EDX
      PushConst(TypeSize(ValType));
      end;
    ValType := INTEGERTYPEINDEX;
    end;

  ROUNDFUNC, TRUNCFUNC:
    begin
    CompileExpression(ValType);

    // Try to convert integer to real
    if ConversionIsPossible(ValType, REALTYPEINDEX) then
      begin
      GenerateFloat(0);
      ValType := REALTYPEINDEX;
      end;

    GetCompatibleType(ValType, REALTYPEINDEX);
    GenerateRound(func = TRUNCFUNC);
    ValType := INTEGERTYPEINDEX;
    end;

  ORDFUNC:
    begin
    CompileExpression(ValType);
    if not (Types[ValType].TypeKind in OrdinalTypes) then
      Error('Ordinal type expected');
    ValType := INTEGERTYPEINDEX;
    end;

  CHRFUNC:
    begin
    CompileExpression(ValType);
    GetCompatibleType(ValType, INTEGERTYPEINDEX);
    ValType := CHARTYPEINDEX;
    end;

  PREDFUNC, SUCCFUNC:
    begin
    CompileExpression(ValType);
    if not (Types[ValType].TypeKind in OrdinalTypes) then
      Error('Ordinal type expected');
    if func = SUCCFUNC then
      PushConst(1)
    else
      PushConst(-1);
    GenerateBinaryOperator(PLUSTOK, INTEGERTYPEINDEX);
    end;

  ABSFUNC, SQRFUNC, SINFUNC, COSFUNC, ARCTANFUNC, EXPFUNC, LNFUNC, SQRTFUNC:
    begin
    CompileExpression(ValType);
    if func in [ABSFUNC, SQRFUNC] then                          // Abs and Sqr accept real or integer parameters
      begin
      if not ((Types[ValType].TypeKind in (IntegerTypes + [REALTYPE])) or
             ((Types[ValType].TypeKind = SUBRANGETYPE) and
              (Types[Types[ValType].HostType].TypeKind in IntegerTypes))) then
        Error('Numeric type expected')
      end
    else
      begin
      
      // Try to convert integer to real
      if ConversionIsPossible(ValType, REALTYPEINDEX) then
        begin
        GenerateFloat(0);
        ValType := REALTYPEINDEX;
        end;

      GetCompatibleType(ValType, REALTYPEINDEX);
      end;

    GenerateMathFunction(func, ValType);
    end;
end;// case

EatTok(CPARTOK);
end;// CompilePredefinedFunc





procedure CompileDesignator(var ValType: Byte);
var
  IdentIndex, FieldIndex: Integer;
  ArrayIndexType: Byte;
  IsRefParam: Boolean;
begin
AssertIdent;

IdentIndex := GetIdent(Tok.Name);

if Ident[IdentIndex].Kind <> VARIABLE then
  Error('Variable expected but ' + Tok.Name + ' found');

PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, BlockStackTop - Ident[IdentIndex].NestingLevel);
ValType := Ident[IdentIndex].DataType;


if Types[Ident[IdentIndex].DataType].TypeKind in [ARRAYTYPE, RECORDTYPE] then
  IsRefParam := Ident[IdentIndex].PassMethod in [CONSTPASSING, VARPASSING]    // For structured parameters, CONST is equivalent to VAR
else
  IsRefParam := Ident[IdentIndex].PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

if IsRefParam then DerefPtr(POINTERTYPEINDEX);                                // Parameter is passed by reference


NextTok;

while Tok.Kind in [DEREFERENCETOK, OBRACKETTOK, PERIODTOK] do
  if Tok.Kind = DEREFERENCETOK then                           // Pointer dereferencing
    begin
    if (Types[ValType].TypeKind <> POINTERTYPE) or (Types[ValType].BaseType = ANYTYPEINDEX) then
      Error('Typed pointer expected');
    DerefPtr(ValType);
    ValType := Types[ValType].BaseType;
    NextTok;
    end
  else if Tok.Kind = OBRACKETTOK then                         // Array element access
    begin
    repeat
      if Types[ValType].TypeKind <> ARRAYTYPE then
        Error('Array expected');
      NextTok;
      CompileExpression(ArrayIndexType);                        // Array index
      GetCompatibleType(ArrayIndexType, Types[ValType].IndexType);
      GetArrayElementPtr(ValType);
      ValType := Types[ValType].BaseType;
    until Tok.Kind <> COMMATOK;
    EatTok(CBRACKETTOK);
    end
  else if Tok.Kind = PERIODTOK then                           // Record field access
    begin
    if Types[ValType].TypeKind <> RECORDTYPE then
      Error('Record expected');
    NextTok;
    AssertIdent;
    FieldIndex := GetField(ValType, Tok.Name);
    GetFieldPtr(ValType, FieldIndex);
    ValType := Types[ValType].Field[FieldIndex]^.DataType;
    NextTok;   
    end;
end; // CompileDesignator




procedure CompileActualParameters(IdentIndex: Integer);
var
  NumActualParams: Integer;
  ActualParamType: Byte;
  IsRefParam, TreatCharAsString: Boolean;
  CurParam: PParam;
begin
NumActualParams := 0;

if Tok.Kind = OPARTOK then                            // Actual parameter list found
  begin
  repeat
    NextTok;

    if NumActualParams + 1 > Ident[IdentIndex].NumParams then
      Error('Too many actual parameters');

    CurParam := Ident[IdentIndex].Param[NumActualParams + 1];

    // Evaluate actual parameters and push them onto the stack

    TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (CurParam^.DataType = STRINGTYPEINDEX);

    if (Tok.Kind = STRINGLITERALTOK) or TreatCharAsString then
      begin
      if CurParam^.PassMethod <> CONSTPASSING then
        Error('String literals can be passed as CONST only');
      IsRefParam := FALSE;
      end
    else
      if Types[CurParam^.DataType].TypeKind in [ARRAYTYPE, RECORDTYPE] then
        IsRefParam := CurParam^.PassMethod in [CONSTPASSING, VARPASSING]    // For structured parameters, CONST is equivalent to VAR
      else
        IsRefParam := CurParam^.PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

    if TreatCharAsString then
      begin                                     // Special case
      PushVarPtr(Tok.StrAddress, GLOBAL, 0);
      ActualParamType := STRINGTYPEINDEX;
      NextTok;
      end
    else
      if IsRefParam then                        // General rule
        CompileDesignator(ActualParamType)
      else
        CompileExpression(ActualParamType);

    Inc(NumActualParams);

    // Try to convert integer to real
    if ConversionIsPossible(ActualParamType, CurParam^.DataType) and not IsRefParam then
      begin
      GenerateFloat(0);
      ActualParamType := REALTYPEINDEX;
      end;

    GetCompatibleType(CurParam^.DataType, ActualParamType);
  until Tok.Kind <> COMMATOK;

  EatTok(CPARTOK);
  end;// if Tok.Kind = OPARTOR

if NumActualParams < Ident[IdentIndex].NumParams then
  Error('Too few actual parameters');
  
end;// CompileActualParameters  

        



procedure CompileFactor(var ValType: Byte);
var
  IdentIndex: Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = PROC then
      Error('Expression expected but procedure ' + Ident[IdentIndex].Name + ' found')
    else if Ident[IdentIndex].Kind = FUNC then                                      // Function call
      if Ident[IdentIndex].PredefIndex <> 0 then                                    // Predefined function call
        CompilePredefinedFunc(Ident[IdentIndex].PredefIndex, ValType)
      else                                                                          // User-defined function call
        begin
        NextTok;
        CompileActualParameters(IdentIndex);
        if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[IdentIndex].ProcAsBlock);
        GenerateCall(Ident[IdentIndex].Value, BlockStackTop - Ident[IdentIndex].NestingLevel);
        RestoreStackTop;
        ValType := Ident[IdentIndex].DataType;
        end // FUNC
    else if Ident[IdentIndex].Kind = VARIABLE then            // Designator
      begin
      CompileDesignator(ValType);
      if not (Types[ValType].TypeKind in [ARRAYTYPE, RECORDTYPE]) then  // Factors of type 'array' or 'record' should contain a pointer to them
        DerefPtr(ValType);
      end
    else if Ident[IdentIndex].Kind = CONSTANT then            // Constant
      begin
      ValType := Ident[IdentIndex].DataType;
      if ValType = REALTYPE then
        PushConst(Integer((Pointer(@Ident[IdentIndex].FracValue))^))
      else
        PushConst(Ident[IdentIndex].Value);
      NextTok;
      end
    else                                                      // Type cast
      begin
      NextTok;
      EatTok(OPARTOK);
      CompileExpression(ValType);
      EatTok(CPARTOK);

      if not ((Types[Ident[IdentIndex].DataType].TypeKind in OrdinalTypes + [TEXTTYPE, POINTERTYPE]) and
              (Types[ValType].TypeKind in OrdinalTypes + [TEXTTYPE, POINTERTYPE])) then
        Error('Invalid typecast');

      ValType := Ident[IdentIndex].DataType;
      end;
    end;


  ADDRESSTOK:
    begin
    NextTok;
    CompileDesignator(ValType);
    ValType := POINTERTYPEINDEX;
    end;


  INTNUMBERTOK:
    begin
    PushConst(Tok.Value);
    ValType := INTEGERTYPEINDEX;
    NextTok;
    end;


  FRACNUMBERTOK:
    begin
    PushConst(Integer((Pointer(@Tok.FracValue))^));
    ValType := REALTYPEINDEX;
    NextTok;
    end;


  CHARLITERALTOK:
    begin
    PushConst(Tok.Value);
    ValType := CHARTYPEINDEX;
    NextTok;
    end;


  STRINGLITERALTOK:
    begin
    PushVarPtr(Tok.StrAddress, GLOBAL, 0);
    ValType := STRINGTYPEINDEX;
    NextTok;
    end;


  OPARTOK:       // Expression in parentheses expected
    begin
    NextTok;
    CompileExpression(ValType);
    EatTok(CPARTOK);
    end;


  NOTTOK:
    begin
    NextTok;
    CompileFactor(ValType);
    CheckOperator(NOTTOK, ValType);
    GenerateUnaryOperator(NOTTOK, ValType);
    end;


  NILTOK:
    begin
    PushConst(0);
    ValType := POINTERTYPEINDEX;
    NextTok;
    end;

else
  Error('Expression expected but ' + GetSpelling(Tok) + ' found');
end;// case

end;// CompileFactor




procedure CompileTerm(var ValType: Byte);
var
  OpTok: TToken;
  RightValType: Byte;
begin
CompileFactor(ValType);

while Tok.Kind in [MULTOK, DIVTOK, IDIVTOK, MODTOK, SHLTOK, SHRTOK, ANDTOK] do
  begin
  OpTok := Tok;
  NextTok;
  CompileFactor(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionIsPossible(ValType, REALTYPEINDEX) and ConversionIsPossible(RightValType, REALTYPEINDEX) then
    begin
    GenerateFloat(SizeOf(Single));
    GenerateFloat(0);
    ValType := REALTYPEINDEX;
    RightValType := REALTYPEINDEX;
    end;


  ValType := GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok.Kind, ValType);
  GenerateBinaryOperator(OpTok.Kind, ValType);
  end;// while

end;// CompileTerm




procedure CompileSimpleExpression(var ValType: Byte);
var
  UnaryOpTok, OpTok: TToken;
  RightValType: Byte;
begin
UnaryOpTok := Tok;
if UnaryOpTok.Kind in [PLUSTOK, MINUSTOK] then
  NextTok;

CompileTerm(ValType);

if UnaryOpTok.Kind in [PLUSTOK, MINUSTOK] then
  CheckOperator(UnaryOpTok.Kind, ValType);

if UnaryOpTok.Kind = MINUSTOK then GenerateUnaryOperator(MINUSTOK, ValType);     // Unary minus

while Tok.Kind in [PLUSTOK, MINUSTOK, ORTOK, XORTOK] do
  begin
  OpTok := Tok;
  NextTok;
  CompileTerm(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  ValType := GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok.Kind, ValType);
  GenerateBinaryOperator(OpTok.Kind, ValType);
  end;// while

end;// CompileSimpleExpression




procedure CompileExpression(var ValType: Byte);
var
  OpTok: TToken;
  RightValType: Byte;
begin
CompileSimpleExpression(ValType);

if Tok.Kind in [EQTOK, NETOK, LTTOK, LETOK, GTTOK, GETOK] then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleExpression(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok.Kind, ValType);
  ValType := BOOLEANTYPEINDEX;
  GenerateRelation(OpTok.Kind, RightValType);
  end;// while

end;// CompileExpression




procedure CompileStatementList;
begin
CompileStatement;
while Tok.Kind = SEMICOLONTOK do
  begin
  NextTok;
  CompileStatement;
  end;
end; // CompileStatementList 




procedure CompileCompoundStatement;
begin
EatTok(BEGINTOK);
CompileStatementList;
EatTok(ENDTOK);
end; // CompileCompoundStatement




procedure CompileStatement;
var
  IdentIndex, ResultIdentIndex, NumCaseStatements: Integer;
  ConstVal, ConstVal2: TConst;
  ExpressionType, DesignatorType, ConstValType, SelectorType: Byte;
  Down, ExitLoop, TreatCharAsString: Boolean;
begin

case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    case Ident[IdentIndex].Kind of

      VARIABLE, FUNC:                                                 // Variable or function result assignment
        begin
        if Ident[IdentIndex].Kind = VARIABLE then
          CompileDesignator(DesignatorType)
        else
          begin
          if Ident[IdentIndex].ProcAsBlock <> BlockStack[BlockStackTop] then
            Error('Current function name expected but ' + Ident[IdentIndex].Name + ' found');

          ResultIdentIndex := GetIdent('RESULT');
          PushVarPtr(Ident[ResultIdentIndex].Value, LOCAL, 0);
          DesignatorType := Ident[ResultIdentIndex].DataType;

          NextTok;
          end;    

        EatTok(ASSIGNTOK);

        TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (DesignatorType = STRINGTYPEINDEX);
        if TreatCharAsString then
          begin                                                       // Special case
          PushVarPtr(Tok.StrAddress, GLOBAL, 0);
          ExpressionType := STRINGTYPEINDEX;
          NextTok;
          end
        else                                                          
          CompileExpression(ExpressionType);                          // General rule - right-hand side expression

        // Try to convert integer to real
        if ConversionIsPossible(ExpressionType, DesignatorType) then
          begin
          GenerateFloat(0);
          ExpressionType := REALTYPEINDEX;
          end;

        GetCompatibleType(DesignatorType, ExpressionType);

        if Types[DesignatorType].TypeKind in [ARRAYTYPE, RECORDTYPE] then
          GenerateStructuredAssignment(DesignatorType)
        else
          GenerateAssignment(DesignatorType);
        end;// VARIABLE

      PROC:                                                           // Procedure call
        if Ident[IdentIndex].PredefIndex <> 0 then                    // Predefined procedure call
          CompilePredefinedProc(Ident[IdentIndex].PredefIndex)
        else                                                          // User-defined procedure call
          begin
          NextTok;
          CompileActualParameters(IdentIndex);
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[IdentIndex].ProcAsBlock);
          GenerateCall(Ident[IdentIndex].Value, BlockStackTop - Ident[IdentIndex].NestingLevel);
          end;// PROC
    else
      Error('Statement expected but ' + Ident[IdentIndex].Name + ' found');
    end// case Ident[IdentIndex].Kind
    end;

  BEGINTOK:
    CompileCompoundStatement;

  IFTOK:
    begin
    NextTok;
    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    EatTok(THENTOK);

    GenerateIfCondition;              // Satisfied if expression is not zero
    GenerateIfProlog;
    CompileStatement;

    if Tok.Kind = ELSETOK then
      begin
      NextTok;
      GenerateElseProlog;                 
      CompileStatement;
      end;

    GenerateIfElseEpilog;
    end;

  CASETOK:
    begin
    NextTok;
    CompileExpression(SelectorType);
    if not (Types[SelectorType].TypeKind in OrdinalTypes) then
      Error('Ordinal variable expected as CASE selector');
    EatTok(OFTOK);

    GenerateCaseProlog;  

    NumCaseStatements := 0;

    repeat       // Loop over all cases

      repeat     // Loop over all constants for the current case
        CompileConstExpression(ConstVal, ConstValType);
        GetCompatibleType(ConstValType, SelectorType);

        if Tok.Kind = RANGETOK then                                      // Range check
          begin
          NextTok;
          CompileConstExpression(ConstVal2, ConstValType);
          GetCompatibleType(ConstValType, SelectorType);
          GenerateCaseRangeCheck(ConstVal.Value, ConstVal2.Value);
          end
        else
          GenerateCaseEqualityCheck(ConstVal.Value);                     // Equality check

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;

      EatTok(COLONTOK);

      GenerateCaseStatementProlog;
      CompileStatement;
      GenerateCaseStatementEpilog;

      Inc(NumCaseStatements);

      ExitLoop := FALSE;
      if Tok.Kind <> SEMICOLONTOK then
        begin
        if Tok.Kind = ELSETOK then              // Default statements
          begin
          NextTok;
          CompileStatementList;
          end;
        ExitLoop := TRUE;
        end
      else
        begin
        NextTok;
        if Tok.Kind = ENDTOK then ExitLoop := TRUE;
        end
    until ExitLoop;

    EatTok(ENDTOK);

    GenerateCaseEpilog(NumCaseStatements);
    end;  

  WHILETOK:
    begin
    Inc(CodePosStackTop);
    CodePosStack[CodePosStackTop] := CodeSize;      // Save return address used by GenerateWhileEpilog

    NextTok;
    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    EatTok(DOTOK);

    GenerateWhileCondition;                         // Satisfied if expression is not zero
    GenerateIfProlog;
    CompileStatement;
    GenerateWhileEpilog;
    end;

  REPEATTOK:
    begin
    GenerateRepeatProlog;

    NextTok;
    CompileStatementList;

    EatTok(UNTILTOK);

    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    GenerateRepeatCondition;
    GenerateRepeatEpilog;
    end;

  FORTOK:
    begin
    NextTok;
    AssertIdent;
    IdentIndex := GetIdent(Tok.Name);

    if (Ident[IdentIndex].Kind <> VARIABLE) or
      ((Ident[IdentIndex].NestingLevel <> 1) and (Ident[IdentIndex].NestingLevel <> BlockStackTop)) or
       (Ident[IdentIndex].PassMethod <> VALPASSING) then
      Error('Simple local variable expected as FOR loop counter');

    if not (Types[Ident[IdentIndex].DataType].TypeKind in OrdinalTypes) then
      Error('Ordinal variable expected as FOR loop counter');

    PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, 0);

    NextTok;
    EatTok(ASSIGNTOK);
    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, Ident[IdentIndex].DataType);

    if not (Tok.Kind in [TOTOK, DOWNTOTOK]) then
      Error('TO or DOWNTO expected but ' + GetSpelling(Tok) + ' found');

    Down := Tok.Kind = DOWNTOTOK;

    NextTok;
    CompileExpression(ExpressionType);
    GetCompatibleType(ExpressionType, Ident[IdentIndex].DataType);

    SaveStackTop;                                             // Save final value
    GenerateAssignment(Ident[IdentIndex].DataType);           // Assign initial value to the counter
    RestoreStackTop;                                          // Restore final value

    Inc(CodePosStackTop);
    CodePosStack[CodePosStackTop] := CodeSize;                // Save return address used by GenerateForEpilog

    GenerateForCondition(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);  // Satisfied if counter does not reach the second expression value

    EatTok(DOTOK);

    GenerateIfProlog;
    CompileStatement;
    GenerateForEpilog(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);
    end;

end;// case

end;// CompileStatement




procedure CompileType(var DataType: Byte);
var
  FieldInListName: array [1..MAXFIELDS] of TName;
  IdentIndex, NumFieldsInList, FieldInListIndex: LongInt;
  NestedDataType, LowBoundType, HighBoundType, ArrType, IndexType, FieldType: Byte;
  ConstVal: TConst;
  TypeNameGiven, ExitLoop: Boolean;

  procedure DeclareField(const Name: TName; RecType, FieldType: Byte);
  var
    i: Integer;
  begin
  for i := 1 to Types[RecType].NumFields do
    if Types[RecType].Field[i]^.Name = Name then
      Error('Duplicate field');

  // Add new field
  Inc(Types[RecType].NumFields);
  New(Types[RecType].Field[Types[RecType].NumFields]);
  
  Types[RecType].Field[Types[RecType].NumFields]^.Name     := Name;
  Types[RecType].Field[Types[RecType].NumFields]^.DataType := FieldType;
  Types[RecType].Field[Types[RecType].NumFields]^.Offset   := TypeSize(RecType) - TypeSize(FieldType);
  end;


begin
if Tok.Kind = DEREFERENCETOK then                                 // Typed pointer
  begin
  // Add new anonymous type
  Inc(NumTypes);
  Types[NumTypes].TypeKind := POINTERTYPE;
  DataType := NumTypes;

  // Compile pointer base type
  NextTok;
  AssertIdent;
  IdentIndex := GetIdentUnsafe(Tok.Name);                         
  
  if IdentIndex = 0 then                                          // Check for a forward-referenced base type
    begin
    // Add new forward-referenced type
    Inc(NumTypes);
    Types[NumTypes].TypeKind := FORWARDTYPE;
    Types[NumTypes].TypeIdentName := Tok.Name;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    NestedDataType := NumTypes;
    end
  else
    begin
    if Ident[IdentIndex].Kind <> USERTYPE then
      Error('Type name expected');
    NestedDataType := Ident[IdentIndex].DataType;                 // Usual base type
    end;

  Types[DataType].BaseType := NestedDataType;
  Types[DataType].Block := BlockStack[BlockStackTop];

  NextTok;
  end// if DEREFERENCETOK
else if Tok.Kind = ARRAYTOK then                                  // Array
  begin
  NextTok;
  EatTok(OBRACKETTOK);

  DataType := NumTypes + 1;

  repeat
    // Add new anonymous type
    Inc(NumTypes);
    Types[NumTypes].TypeKind := ARRAYTYPE;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    ArrType := NumTypes;

    CompileType(IndexType);
    if not (Types[IndexType].TypeKind in OrdinalTypes) then
      Error('Ordinal type expected');
    Types[ArrType].IndexType := IndexType;

    ExitLoop := FALSE;
    if Tok.Kind = COMMATOK then
      begin
      Types[ArrType].BaseType := NumTypes + 1;
      NextTok;
      end
    else
      ExitLoop := TRUE;
  until ExitLoop;

  EatTok(CBRACKETTOK);
  EatTok(OFTOK);

  CompileType(NestedDataType);
  Types[ArrType].BaseType := NestedDataType;
  end // if ARRAYTOK
else if Tok.Kind = RECORDTOK then                                 // Record
  begin
  // Add new anonymous type
  Inc(NumTypes);
  Types[NumTypes].TypeKind := RECORDTYPE;
  DataType := NumTypes;

  NextTok;

  Types[DataType].NumFields := 0;
  repeat
    NumFieldsInList := 0;
    repeat
      AssertIdent;

      Inc(NumFieldsInList);
      FieldInListName[NumFieldsInList] := Tok.Name;

      NextTok;

      ExitLoop := FALSE;
      if Tok.Kind = COMMATOK then
        NextTok
      else
        ExitLoop := TRUE;
    until ExitLoop;

    EatTok(COLONTOK);

    CompileType(FieldType);

    for FieldInListIndex := 1 to NumFieldsInList do
      DeclareField(FieldInListName[FieldInListIndex], DataType, FieldType);

    ExitLoop := FALSE;
    if Tok.Kind <> SEMICOLONTOK then
      ExitLoop := TRUE
    else
      begin
      NextTok;
      if Tok.Kind = ENDTOK then ExitLoop := TRUE;
      end
  until ExitLoop;

  EatTok(ENDTOK);

  Types[DataType].Block := BlockStack[BlockStackTop];
  end// if RECORDTOK
else                                                                              // Subrange or type name
  begin
  TypeNameGiven := FALSE;
  IdentIndex := 0;
  if Tok.Kind = IDENTTOK then      
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = USERTYPE then TypeNameGiven := TRUE;
    end;

  if TypeNameGiven then                                                           // Type identifier
    begin
    DataType := Ident[IdentIndex].DataType;
    NextTok;
    end
  else                                                                            // Subrange
    begin
    // Add new anonymous type
    Inc(NumTypes);
    Types[NumTypes].TypeKind := SUBRANGETYPE;
    DataType := NumTypes;

    CompileConstExpression(ConstVal, LowBoundType);                               // Subrange lower bound
    if not (Types[LowBoundType].TypeKind in (OrdinalTypes - [SUBRANGETYPE])) then
      Error('Ordinal type expected');
    Types[DataType].Low := ConstVal.Value;

    EatTok(RANGETOK);

    CompileConstExpression(ConstVal, HighBoundType);                              // Subrange upper bound
    if not (Types[HighBoundType].TypeKind in (OrdinalTypes - [SUBRANGETYPE])) then
      Error('Ordinal type expected');
    Types[DataType].High := ConstVal.Value;

    GetCompatibleType(LowBoundType, HighBoundType);

    if Types[DataType].High < Types[DataType].Low then
      Error('Illegal subrange bounds');

    Types[DataType].HostType := LowBoundType;
    Types[DataType].Block := BlockStack[BlockStackTop];
    end;// else
  end;// else  

end;// CompileType




procedure CompileBlock(BlockIdentIndex: Integer);
var
  NameTok, ProcFuncTok: TToken;
  IdentInListName: array [1..MAXPARAMS] of TName;
  LocalDataSize, ParamDataSize: Integer;
  NumIdentInList, IdentInListIndex, ForwardIdentIndex, IdentIndex, ParamIndex, FieldIndex, TypeIndex: Integer;
  ConstVal: TConst;
  ExitLoop: Boolean;
  ListPassMethod: Byte;
  VarType, ConstValType: Byte;


  procedure DeclareId(const Name: TName; Kind: Byte; TotalNumParams: Integer; DataType: Byte; PassMethod: Byte; ConstValue: LongInt; FracConstValue: Single; PredefIndex: Byte);
  var
    i: Integer;
    Scope: Byte;
  begin
  if BlockStack[BlockStackTop] = 1 then Scope := GLOBAL else Scope := LOCAL;

  i := GetIdentUnsafe(Name);

  if (i > 0) and (Ident[i].Block = BlockStack[BlockStackTop]) then
    Error('Duplicate identifier: ' + Name);

  Inc(NumIdent);
  Ident[NumIdent].Name := Name;
  Ident[NumIdent].Kind := Kind;
  Ident[NumIdent].Scope := Scope;
  Ident[NumIdent].DataType := DataType;
  Ident[NumIdent].Block := BlockStack[BlockStackTop];
  Ident[NumIdent].NestingLevel := BlockStackTop;
  Ident[NumIdent].NumParams := 0;
  Ident[NumIdent].PassMethod := PassMethod;
  Ident[NumIdent].IsUnresolvedForward := FALSE;

  case Kind of
    PROC, FUNC:
      if PredefIndex = 0 then
        Ident[NumIdent].Value := CodeSize                                // Routine entry point address
      else
        Ident[NumIdent].PredefIndex := PredefIndex;                      // Predefined routine index

    VARIABLE:
      if (Pass = CALLDETERMPASS) or BlockIsNotDead[BlockStack[BlockStackTop]] then
        case Scope of
          GLOBAL:
            begin
            Ident[NumIdent].Value := VarDataOrigin + GlobalDataSize;     // Variable address
            GlobalDataSize := GlobalDataSize + TypeSize(DataType);
            end;// else

          LOCAL:
            if TotalNumParams > 0 then
              begin
              ParamDataSize := ParamDataSize + SizeOf(LongInt);                                 // Parameters always occupy 4 bytes each
              Ident[NumIdent].Value := (3 + TotalNumParams) * SizeOf(LongInt) - ParamDataSize;  // Parameter offset from bp (>0); the last (hidden) parameter is the static link
              end
            else
              begin
              Ident[NumIdent].Value := -LocalDataSize - TypeSize(DataType);                     // Local variable offset from bp (<0)
              LocalDataSize := LocalDataSize + TypeSize(DataType);
              end;
        end// case
      else
        Ident[NumIdent].Value := 0;

    CONSTANT:
      if Types[DataType].TypeKind = REALTYPE then
        Ident[NumIdent].FracValue := FracConstValue                     // Real constant value
      else
        Ident[NumIdent].Value := ConstValue;                            // Ordinal constant value

  end;// case

  if VarDataOrigin + GlobalDataSize > SEGMENTSIZE then
    Error('Maximum global data size exceeded');

  if LocalDataSize > SEGMENTSIZE then
    Error('Maximum local data size exceeded');

  if ParamDataSize > SEGMENTSIZE then
    Error('Maximum parameter data size exceeded');

  end;// DeclareId



  procedure DeclarePredefinedIdents;
  begin
  // Constants
  DeclareId('TRUE',  CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING, -1, 0.0, 0);
  DeclareId('FALSE', CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING,  0, 0.0, 0);

  // Types
  DeclareId('INTEGER',  USERTYPE, 0, INTEGERTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('SMALLINT', USERTYPE, 0, SMALLINTTYPEINDEX, VALPASSING, 0, 0.0, 0);
  DeclareId('SHORTINT', USERTYPE, 0, SHORTINTTYPEINDEX, VALPASSING, 0, 0.0, 0);
  DeclareId('CHAR',     USERTYPE, 0, CHARTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('BOOLEAN',  USERTYPE, 0, BOOLEANTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('REAL',     USERTYPE, 0, REALTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('POINTER',  USERTYPE, 0, POINTERTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('TEXT',     USERTYPE, 0, TEXTTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('STRING',   USERTYPE, 0, STRINGTYPEINDEX,   VALPASSING, 0, 0.0, 0);

  // Procedures
  DeclareId('INC',      PROC, 0, 0, VALPASSING, 0, 0.0, INCPROC);
  DeclareId('DEC',      PROC, 0, 0, VALPASSING, 0, 0.0, DECPROC);
  DeclareId('READ',     PROC, 0, 0, VALPASSING, 0, 0.0, READPROC);
  DeclareId('WRITE',    PROC, 0, 0, VALPASSING, 0, 0.0, WRITEPROC);
  DeclareId('READLN',   PROC, 0, 0, VALPASSING, 0, 0.0, READLNPROC);
  DeclareId('WRITELN',  PROC, 0, 0, VALPASSING, 0, 0.0, WRITELNPROC);
  DeclareId('INP',      PROC, 0, 0, VALPASSING, 0, 0.0, INPPROC);
  DeclareId('OUTP',     PROC, 0, 0, VALPASSING, 0, 0.0, OUTPPROC);
  DeclareId('NEW',      PROC, 0, 0, VALPASSING, 0, 0.0, NEWPROC);
  DeclareId('DISPOSE',  PROC, 0, 0, VALPASSING, 0, 0.0, DISPOSEPROC);
  DeclareId('HALT',     PROC, 0, 0, VALPASSING, 0, 0.0, HALTPROC);
  DeclareId('INTR',     PROC, 0, 0, VALPASSING, 0, 0.0, INTRPROC);

  // Functions
  DeclareId('SIZEOF', FUNC, 0, 0, VALPASSING, 0, 0.0, SIZEOFFUNC);
  DeclareId('ORD',    FUNC, 0, 0, VALPASSING, 0, 0.0, ORDFUNC);
  DeclareId('CHR',    FUNC, 0, 0, VALPASSING, 0, 0.0, CHRFUNC);
  DeclareId('PRED',   FUNC, 0, 0, VALPASSING, 0, 0.0, PREDFUNC);
  DeclareId('SUCC',   FUNC, 0, 0, VALPASSING, 0, 0.0, SUCCFUNC);
  DeclareId('ROUND',  FUNC, 0, 0, VALPASSING, 0, 0.0, ROUNDFUNC);
  DeclareId('TRUNC',  FUNC, 0, 0, VALPASSING, 0, 0.0, TRUNCFUNC);
  DeclareId('ABS',    FUNC, 0, 0, VALPASSING, 0, 0.0, ABSFUNC);
  DeclareId('SQR',    FUNC, 0, 0, VALPASSING, 0, 0.0, SQRFUNC);
  DeclareId('SIN',    FUNC, 0, 0, VALPASSING, 0, 0.0, SINFUNC);
  DeclareId('COS',    FUNC, 0, 0, VALPASSING, 0, 0.0, COSFUNC);
  DeclareId('ARCTAN', FUNC, 0, 0, VALPASSING, 0, 0.0, ARCTANFUNC);
  DeclareId('EXP',    FUNC, 0, 0, VALPASSING, 0, 0.0, EXPFUNC);
  DeclareId('LN',     FUNC, 0, 0, VALPASSING, 0, 0.0, LNFUNC);
  DeclareId('SQRT',   FUNC, 0, 0, VALPASSING, 0, 0.0, SQRTFUNC);
  end;// DeclarePredefinedIdents



  procedure DeclarePredefinedTypes;
  begin
  NumTypes := STRINGTYPEINDEX;

  Types[ANYTYPEINDEX].TypeKind      := ANYTYPE;
  Types[INTEGERTYPEINDEX].TypeKind  := INTEGERTYPE;
  Types[SMALLINTTYPEINDEX].TypeKind := SMALLINTTYPE;
  Types[SHORTINTTYPEINDEX].TypeKind := SHORTINTTYPE;
  Types[CHARTYPEINDEX].TypeKind     := CHARTYPE;
  Types[BOOLEANTYPEINDEX].TypeKind  := BOOLEANTYPE;
  Types[REALTYPEINDEX].TypeKind     := REALTYPE;
  Types[POINTERTYPEINDEX].TypeKind  := POINTERTYPE;
  Types[TEXTTYPEINDEX].TypeKind     := TEXTTYPE;
  Types[STRINGTYPEINDEX].TypeKind   := ARRAYTYPE;

  Types[POINTERTYPEINDEX].BaseType  := ANYTYPEINDEX;

  // Add new anonymous type: 0..MAXSTRLENGTH
  Inc(NumTypes);
  Types[NumTypes].TypeKind := SUBRANGETYPE;
  Types[NumTypes].HostType := INTEGERTYPEINDEX;
  Types[NumTypes].Low      := 0;
  Types[NumTypes].High     := MAXSTRLENGTH;
  Types[NumTypes].Block    := BlockStack[BlockStackTop];

  Types[STRINGTYPEINDEX].BaseType  := CHARTYPEINDEX;
  Types[STRINGTYPEINDEX].IndexType := NumTypes;
  end;// DeclarePredefinedTypes



  procedure CheckForwardResolutions;
  var
    TypeIndex: Integer;
  begin
  // Search for unresolved forward references
  for TypeIndex := 1 to NumTypes do
    if (Types[TypeIndex].TypeKind = FORWARDTYPE) and
       (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
      Error('Unresolved forward reference to type ' + Types[TypeIndex].TypeIdentName);
  end; // CheckForwardResolutions


// DeclareId
begin
Inc(BlockStackTop);

if BlockIdentIndex = 0 then
  BlockStack[BlockStackTop] := 1
else
  BlockStack[BlockStackTop] := Ident[BlockIdentIndex].ProcAsBlock;

ParamDataSize := 0;
LocalDataSize := 0;

if BlockStack[BlockStackTop] = 1 then             // Main program
  begin
  DeclarePredefinedTypes;
  DeclarePredefinedIdents;
  GenerateProgramProlog;
  end
else
  begin
  // DeclareId parameters like local variables
  for ParamIndex := 1 to Ident[BlockIdentIndex].NumParams do
    DeclareId(Ident[BlockIdentIndex].Param[ParamIndex]^.Name,
            VARIABLE,
            Ident[BlockIdentIndex].NumParams,
            Ident[BlockIdentIndex].Param[ParamIndex]^.DataType,
            Ident[BlockIdentIndex].Param[ParamIndex]^.PassMethod,
            0,
            0.0,
            0);

  // Allocate Result variable if the current block is a function
  if Ident[BlockIdentIndex].Kind = FUNC then DeclareId('RESULT', VARIABLE, 0, Ident[BlockIdentIndex].DataType, VALPASSING, 0, 0.0, 0);
  end;// else

GenerateDeclarationProlog;


while Tok.Kind in [CONSTTOK, TYPETOK, VARTOK, PROCEDURETOK, FUNCTIONTOK] do
  begin
  if Tok.Kind = CONSTTOK then
    begin
    NextTok;
    repeat
      AssertIdent;

      NameTok := Tok;
      NextTok;
      EatTok(EQTOK);

      CompileConstExpression(ConstVal, ConstValType);
      DeclareId(NameTok.Name, CONSTANT, 0, ConstValType, VALPASSING, ConstVal.Value, ConstVal.FracValue, 0);

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    end;// if CONSTTOK


  if Tok.Kind = TYPETOK then
    begin
    NextTok;
    repeat
      AssertIdent;

      NameTok := Tok;
      NextTok;
      EatTok(EQTOK);

      CompileType(VarType);
      DeclareId(NameTok.Name, USERTYPE, 0, VarType, VALPASSING, 0, 0.0, 0);

      // Check if this type was forward-referenced
      for TypeIndex := 1 to NumTypes do
        if (Types[TypeIndex].TypeKind = FORWARDTYPE) and
           (Types[TypeIndex].TypeIdentName = NameTok.Name) and
           (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
          begin
          // Forward type reference resolution
          Types[TypeIndex] := Types[VarType];
          if Types[VarType].TypeKind = RECORDTYPE then
            for FieldIndex := 1 to Types[VarType].NumFields do
              begin
              New(Types[TypeIndex].Field[FieldIndex]);
              Types[TypeIndex].Field[FieldIndex]^ := Types[VarType].Field[FieldIndex]^;
              end;
          end;// if    

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    CheckForwardResolutions;

    end;// if TYPETOK


  if Tok.Kind = VARTOK then
    begin
    NextTok;
    repeat
      NumIdentInList := 0;
      repeat
        AssertIdent;

        Inc(NumIdentInList);
        IdentInListName[NumIdentInList] := Tok.Name;

        NextTok;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;

      EatTok(COLONTOK);

      CompileType(VarType);

      for IdentInListIndex := 1 to NumIdentInList do
        DeclareId(IdentInListName[IdentInListIndex], VARIABLE, 0, VarType, VALPASSING, 0, 0.0, 0);

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    CheckForwardResolutions;

    end;// if VARTOK


  if Tok.Kind in [PROCEDURETOK, FUNCTIONTOK] then
    begin
    ProcFuncTok := Tok;
    NextTok;

    AssertIdent;

    // Check for forward declaration resolution
    ForwardIdentIndex := GetIdentUnsafe(Tok.Name);
    if ForwardIdentIndex <> 0 then
      if not Ident[ForwardIdentIndex].IsUnresolvedForward or
         (Ident[ForwardIdentIndex].Block <> BlockStack[BlockStackTop]) or
         ((ProcFuncTok.Kind = PROCEDURETOK) and (Ident[ForwardIdentIndex].Kind <> PROC)) or
         ((ProcFuncTok.Kind = FUNCTIONTOK) and (Ident[ForwardIdentIndex].Kind <> FUNC)) then
       ForwardIdentIndex := 0;                                      // Found an identifier of another kind or scope, or it is already resolved

    if ForwardIdentIndex = 0 then
      begin

      if ProcFuncTok.Kind = PROCEDURETOK then
        DeclareId(Tok.Name, PROC, 0, 0, VALPASSING, 0, 0.0, 0)
      else
        DeclareId(Tok.Name, FUNC, 0, 0, VALPASSING, 0, 0.0, 0);

      NextTok;

      if Tok.Kind = OPARTOK then                                    // Formal parameter list found
        begin
        NextTok;
        repeat
          NumIdentInList := 0;
          ListPassMethod := VALPASSING;

          if Tok.Kind = CONSTTOK then
            begin
            ListPassMethod := CONSTPASSING;
            NextTok;
            end
          else if Tok.Kind = VARTOK then
            begin
            ListPassMethod := VARPASSING;
            NextTok;
            end;

          repeat
            AssertIdent;

            Inc(NumIdentInList);
            IdentInListName[NumIdentInList] := Tok.Name;

            NextTok;

            ExitLoop := FALSE;
            if Tok.Kind = COMMATOK then
              NextTok
            else
              ExitLoop := TRUE;
          until ExitLoop;

          EatTok(COLONTOK);

          // Only type names are allowed for formal parameters
          AssertIdent;
          IdentIndex := GetIdent(Tok.Name);
          if Ident[IdentIndex].Kind = USERTYPE then
            VarType := Ident[IdentIndex].DataType
          else
            Error('Type name expected');
          NextTok;


          if (ListPassMethod = VALPASSING) and (Types[VarType].TypeKind in [ARRAYTYPE, RECORDTYPE]) then
            Error('Structured parameters cannot be passed by value');

          for IdentInListIndex := 1 to NumIdentInList do
            begin
            Inc(Ident[NumIdent].NumParams);

            if Ident[NumIdent].NumParams > MAXPARAMS then
              Error('Too many formal parameters in ' + Ident[NumIdent].Name);

            New(Ident[NumIdent].Param[Ident[NumIdent].NumParams]);

            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.DataType   := VarType;
            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.PassMethod := ListPassMethod;
            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.Name       := IdentInListName[IdentInListIndex];
            end;// for

          ExitLoop := FALSE;
          if Tok.Kind = SEMICOLONTOK then
            NextTok
          else
            ExitLoop := TRUE;
        until ExitLoop;

        EatTok(CPARTOK);
        end;// if Tok.Kind = OPARTOR

      Ident[NumIdent].DataType := 0;

      if ProcFuncTok.Kind = FUNCTIONTOK then
        begin
        EatTok(COLONTOK);

        // Only type names are allowed for function results
        AssertIdent;
        IdentIndex := GetIdent(Tok.Name);
        if Ident[IdentIndex].Kind = USERTYPE then
          VarType := Ident[IdentIndex].DataType
        else
          Error('Type name expected');
        NextTok;

        if Types[VarType].TypeKind in [ARRAYTYPE, RECORDTYPE] then
          Error('Structured result is not allowed');

        Ident[NumIdent].DataType := VarType;
        end;// if IsNestedFunction

      end// if ForwardIdentIndex = 0
    else
      NextTok;

    EatTok(SEMICOLONTOK);

    // Check for a FORWARD directive (it is not a reserved word)
    if (ForwardIdentIndex = 0) and (Tok.Kind = IDENTTOK) and (Tok.Name = 'FORWARD') then  // Forward declaration
      begin
      Inc(NumBlocks);
      Ident[NumIdent].ProcAsBlock := NumBlocks;
      Ident[NumIdent].IsUnresolvedForward := TRUE;
      GenerateForwardReference;
      NextTok;
      end
    else
      begin

      if ForwardIdentIndex = 0 then                                                       // New declaration
        begin
        Inc(NumBlocks);
        Ident[NumIdent].ProcAsBlock := NumBlocks;
        CompileBlock(NumIdent);
        end
      else                                                                                // Forward declaration resolution
        begin
        GenerateForwardResolution(ForwardIdentIndex);
        CompileBlock(ForwardIdentIndex);
        Ident[ForwardIdentIndex].IsUnresolvedForward := FALSE;
        end;

      end;

    EatTok(SEMICOLONTOK);
    end;// if Tok.Kind in [PROCEDURETOK, FUNCTIONTOK]

  end;// while

GenerateDeclarationEpilog;  // Make jump to block entry point

if BlockStack[BlockStackTop] <> 1 then
  GenerateStackFrameProlog(LocalDataSize);

CompileCompoundStatement;

// If function, return Result value via the EDX register
if (BlockStack[BlockStackTop] <> 1) and (Ident[BlockIdentIndex].Kind = FUNC) then
  begin
  PushVarPtr(Ident[GetIdent('RESULT')].Value, LOCAL, 0);
  DerefPtr(Ident[BlockIdentIndex].DataType);
  SaveStackTop;
  end;

if BlockStack[BlockStackTop] = 1 then          // Main program
  GenerateProgramEpilog
else
  begin
  GenerateStackFrameEpilog;
  GenerateReturn(Ident[BlockIdentIndex].NumParams * SizeOf(LongInt));
  end;

// Delete local identifiers and types from the tables to save space
while (NumIdent > 0) and (Ident[NumIdent].Block = BlockStack[BlockStackTop]) do
  begin
  // If procedure or function, delete parameters first
  if Ident[NumIdent].Kind in [PROC, FUNC] then
    begin
    if Ident[NumIdent].IsUnresolvedForward then
      Error('Unresolved forward declaration of ' + Ident[NumIdent].Name);

    for ParamIndex := 1 to Ident[NumIdent].NumParams do
      Dispose(Ident[NumIdent].Param[ParamIndex]);
    end;  

  // Delete identifier itself
  Dec(NumIdent);
  end;
  
while (NumTypes > 0) and (Types[NumTypes].Block = BlockStack[BlockStackTop]) do
  begin
  // If record, delete fields first
  if Types[NumTypes].TypeKind = RECORDTYPE then
    for FieldIndex := 1 to Types[NumTypes].NumFields do
      Dispose(Types[NumTypes].Field[FieldIndex]);

  // Delete type itself
  Dec(NumTypes);
  end;    

Dec(BlockStackTop);
end;// CompileBlock




procedure CompileProgram;
begin
NextTok;
EatTok(PROGRAMTOK);
AssertIdent;
NextTok;
CheckTok(SEMICOLONTOK);

EnterIncludedFile('system.pas');
NextTok;

Inc(NumBlocks);
CompileBlock(0);

CheckTok(PERIODTOK);
end;// CompileProgram






// ----- OPTIMIZER -----


procedure MarkBlockNotDead(i: Integer);
var
  j: Integer;
begin
BlockIsNotDead[i] := TRUE;
for j := 1 to NumBlocks do
  if ((CallGraph[i, j div 8] and (1 shl (j mod 8))) <> 0) and not BlockIsNotDead[j] then
    MarkBlockNotDead(j);
end;






// ----- MAIN PROGRAM -----


procedure ZeroAll;
begin
NumIdent := 0; NumTypes := 0; NumBlocks := 0; BlockStackTop := 0; CodeSize := 0; CodePosStackTop := 0;
NumStaticStrChars := 0;
GlobalDataSize := 0;
end;





procedure ChangeExt(const InStr, Ext: TString; var OutStr: TString);
var
  i, DotPos: Integer;
begin
i := Length(InStr);
DotPos := 0;

while (i > 0) and (DotPos = 0) do
  begin
  if InStr[i] = '.' then DotPos := i;
  Dec(i);
  end;

if DotPos > 0 then
  OutStr := Copy(InStr, 1, DotPos) + Ext
else
  OutStr := InStr + Ext;
end;  




var
  BlockIndex: Integer;
  OptimizationDisabled: Boolean;




begin
WriteLn;
WriteLn('XD Pascal compiler v. ', VERSION, '. Developed by Vasiliy Tereshkov, 2009-2010');
WriteLn;

if ParamCount < 1 then
  begin
  WriteLn('Usage: xdp <file.pas> [/n]');
  WriteLn;
  WriteLn('       /n - disable optimization');
  WriteLn;
  Halt(1);
  end;

if ParamCount > 1 then
  OptimizationDisabled := (ParamStr(2) = '/n') or (ParamStr(2) = '/N')
else
  OptimizationDisabled := FALSE;


ProgramName := ParamStr(1);

ChangeExt(ProgramName, 'com', ExeName);
Assign(OutFile, ExeName);

if not OptimizationDisabled then             // Default mode
  begin
  for BlockIndex := 1 to MAXBLOCKS do
    BlockIsNotDead[BlockIndex] := FALSE;

  // Preliminary pass: compile the program and build the call graph
  VarDataOrigin := 0;
  ZeroAll;

  Pass := CALLDETERMPASS;

  InitScanner;
  CompileProgram;

  // Visit the call graph nodes and mark all procedures that are called as not dead
  MarkBlockNotDead(1);

  VarDataOrigin := NumStaticStrChars;
  end
else
  begin
  for BlockIndex := 1 to MAXBLOCKS do
    BlockIsNotDead[BlockIndex] := TRUE;

  VarDataOrigin := MAXSTATICSTRDATASIZE;
  end;

// Final pass: compile the program and generate output (BlockIsNotDead array is preserved)

ZeroAll;

Pass := CODEGENERATIONPASS;

Rewrite(OutFile);

if IOResult <> 0 then
  Error('Unable to open output file ' + ExeName);

InitScanner;
CompileProgram;

Close(OutFile);

WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', VarDataOrigin + GlobalDataSize, ' bytes.');
WriteLn;
end.

