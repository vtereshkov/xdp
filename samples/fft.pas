// Fast Fourier Transform demo program


program FFT;



const
  DataLength = 512; 



type
  Complex = record
    Re, Im: Real;
  end;
  
  TData = array [0..DataLength - 1] of Real;
  TComplexData = array [0..DataLength - 1] of Complex;
  
  PData = ^TData;



var
  x, S: TData;
  Twiddle: TComplexData;
  



procedure CAdd(var a, b, c: Complex);
begin
c.Re := a.Re + b.Re;
c.Im := a.Im + b.Im;
end;




procedure CSub(var a, b, c: Complex);
begin
c.Re := a.Re - b.Re;
c.Im := a.Im - b.Im;
end;




procedure CMul(var a, b, c: Complex);
begin
c.Re := a.Re * b.Re - a.Im * b.Im;
c.Im := a.Re * b.Im + a.Im * b.Re;
end;




function CAbs(var a: Complex): Real;
begin
CAbs := sqrt(a.Re * a.Re + a.Im * a.Im);
end;




procedure GetFFT(var x: TData; var FFT: TComplexData; Depth: Integer);
var
  k, HalfLen, Step: Integer;
  FFTEven, FFTOdd: TComplexData;
  FFTOddTwiddled: Complex;
  xShiftedPtr: PData;

begin
HalfLen := DataLength shr (Depth + 1);
Step := 1 shl Depth;

if HalfLen = 0 then
  begin
  FFT[0].Re := x[0];
  FFT[0].Im := 0;
  end
else
  begin
  xShiftedPtr := @x[Step];
  
  GetFFT(x,            FFTEven, Depth + 1);
  GetFFT(xShiftedPtr^, FFTOdd,  Depth + 1);

  for k := 0 to HalfLen - 1 do
    begin
    CMul(FFTOdd[k], Twiddle[k * Step], FFTOddTwiddled);

    CAdd(FFTEven[k], FFTOddTwiddled, FFT[k]);
    CSub(FFTEven[k], FFTOddTwiddled, FFT[k + HalfLen]);
    end; // for
  end; // else

end;




procedure Spectrum(var x, S: TData);
var
  FFT: TComplexData;
  i: Integer;
begin
for i := 0 to DataLength - 1 do
  begin
  Twiddle[i].Re :=  cos(2 * Pi * i / DataLength);
  Twiddle[i].Im := -sin(2 * Pi * i / DataLength);
  end;

GetFFT(x, FFT, 0);

for i := 0 to DataLength - 1 do
  S[i] := CAbs(FFT[i]);

end;




const
  x01 =  50;  y01 = 100;  scale = 4.0;
  x02 =  50;  y02 = 300;



var
  Amp, Period: array [0..4] of Real;
  Phase: Real;
  i, j: Integer;
  Ch: Char;



begin
Randomize;

repeat
  SetScreenMode($10);                             // 640 x 350 pixels, 16 colors
  
  Line(x01, y01, x01 + 540, y01, 12);             // Time axis
  OutTextXY(x01 + 550, y01, 15, 'Time');
  
  Line(x01, y01 - 50, x01, y01 + 50, 12);         // Signal axis
  OutTextXY(x01, y01 - 60, 15, 'Signal');

  Line(x02, y02, x02 + 270, y02, 12);             // Frequency axis
  OutTextXY(x02 + 280, y02, 15, 'Frequency');
  
  Line(x02, y02 - 100, x02, y02, 12);             // Amplitude axis
  OutTextXY(x02, y02 - 110, 15, 'Magnitude');

  for j := 0 to 4 do
    begin
    Amp[j]    := (Random - 0.5) * 40;
    Period[j] := 2 + abs(Random - 0.5) * 40;
    end;

  for i := 0 to DataLength - 1 do
    begin
    Phase := 2 * Pi * i;

    x[i] := Amp[0] / 2;

    for j := 1 to 4 do
      x[i] := x[i] + Amp[j] * sin(Phase / Period[j]);

    if i > 0 then Line(x01 + i - 1, y01 - Round(x[i - 1]), x01 + i, y01 - Round(x[i]), 10);
    end; // for
  
  Spectrum(x, S);

  for i := 0 to DataLength shr 1 - 1 do  
    Line(x02 + i, y02, x02 + i, y02 - Round(scale * S[i] * 2 / DataLength), 9);

  Line(x02 - 2, y02 - Round(scale * abs(Amp[0])), 
       x02 + 2, y02 - Round(scale * abs(Amp[0])), 14);
 
  for j := 1 to 4 do
    Line(x02 + Round(DataLength / Period[j]) - 2, y02 - Round(scale * abs(Amp[j])), 
         x02 + Round(DataLength / Period[j]) + 2, y02 - Round(scale * abs(Amp[j])), 14);
 
  Read(Ch);
until Ch = #27;

SetScreenMode($03);
end.


  