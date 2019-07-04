// Inertial navigation system error estimator demo


program INSErr;


{$I samples\kalman.inc}



const
  g     = 9.81;
  Re    = 6378e3;
  dt    = 0.1;
  A     = 100.0;
  beta  = 1e-4;
  deg   = pi / 180;
  hr    = 3600;
  tstop = 2 * hr;



type
  TModel = record
    dV, Phi, omegaDr, z: Real;
  end;  
  



function GaussRnd(m, sigma: Real): Real;
var
  s: Real;
  i: SmallInt;
begin
s := 0;

for i := 1 to 12 do
  s := s + Random;

Result := m + sigma * (s - 6);
end;




procedure InitModel(var M: TModel);
begin
M.dV      := 0;
M.Phi     := GaussRnd(0, 0.1 * deg);
M.omegaDr := GaussRnd(0, 0.5 * deg / hr);
end;



procedure ExecuteModel(var M: TModel);
var
  dVdot, Phidot, omegaDrdot: Real;
begin
dVdot      := -g * M.Phi;
Phidot     :=  M.dV / Re + M.omegaDr;
omegaDrdot := -beta * M.omegaDr + A * sqrt(2 * beta) * GaussRnd(0, 0.0000001);

M.dV      := M.dV + dVdot * dt;
M.Phi     := M.Phi + Phidot * dt;
M.omegaDr := M.omegaDr + omegaDrdot * dt;

M.z       := M.dV + GaussRnd(0, 3.0);
end;



procedure InitSchulerKF(var KF: TKalmanFilter; Q, R: Real);
begin
{The following error model is used:

 dV'  := -g F;
 F'   := dV / R + wdr;
 wdr' := -b wdr + A sqrt(2b) w;

 z    := dV + v.}

KF.n := 3;  KF.m := 1;  KF.s := 1;

KF.Phi[1, 1] := 1;       KF.Phi[1, 2] := -g * dt;  KF.Phi[1, 3] := 0;
KF.Phi[2, 1] := dt / Re; KF.Phi[2, 2] := 1;        KF.Phi[2, 3] := dt;
KF.Phi[3, 1] := 0;       KF.Phi[3, 2] := 0;        KF.Phi[3, 3] := 1 - beta * dt;

KF.H[1, 1] := 1;         KF.H[1, 2] := 0;          KF.H[1, 3] := 0;

KF.G[1, 1] := 0;
KF.G[2, 1] := 0;
KF.G[3, 1] := A * sqrt(2 * beta) * dt;

KF.Q[1, 1] := Q;

KF.R[1, 1] := R;

KF.x[1, 1] := 0;         KF.x[2, 1] := 0;          KF.x[3, 1] := 0;
 
KF.P[1, 1] := 1;         KF.P[1, 2] := 0;          KF.P[1, 3] := 0;
KF.P[2, 1] := 0;         KF.P[2, 2] := 1;          KF.P[2, 3] := 0;
KF.P[3, 1] := 0;         KF.P[3, 2] := 0;          KF.P[3, 3] := 1;
end;




const
  x0  =  50;  scalex  = 540 / (tstop / dt); 
  y01 =  60;  scaley1 = 1;
  y02 = 170;  scaley2 = 100;
  y03 = 280;  scaley3 = 10;



var
  Model: TModel;
  Filter: TKalmanFilter;

  i, screenx: Integer;
  Ch: Char;
  rand: Real;



begin
Randomize;

repeat
  SetScreenMode($10);                           // 640 x 350 pixels, 16 colors
 
  Line(x0, y01, x0 + 540, y01, 12);             // t axis
  OutTextXY(x0 + 550, y01, 15, 'Time');
  
  Line(x0, y01 - 40, x0, y01 + 40, 12);         // dV axis
  OutTextXY(x0, y01 - 50, 15, 'Velocity Error');

  Line(x0, y02, x0 + 540, y02, 12);             // t axis
  OutTextXY(x0 + 550, y02, 15, 'Time');
  
  Line(x0, y02 - 40, x0, y02 + 40, 12);         // Phi axis
  OutTextXY(x0, y02 - 50, 15, 'Angle Error');

  Line(x0, y03, x0 + 540, y03, 12);             // t axis
  OutTextXY(x0 + 550, y03, 15, 'Time');
  
  Line(x0, y03 - 40, x0, y03 + 40, 12);         // omegaDr axis
  OutTextXY(x0, y03 - 50, 15, 'Gyro Drift');

  InitModel(Model);
  InitSchulerKF(Filter, 1e-10, 1e6);

  for i := 0 to Round(tstop / dt) do
    begin
    ExecuteModel(Model);
    Filter.z[1, 1] := Model.z;
    ExecuteFilter(Filter);

    screenx := x0 + Round(scalex * i);

    PutPixel(screenx, y01 - Round(scaley1 * Model.z                   ), 5);
    
    PutPixel(screenx, y01 - Round(scaley1 * Model.dV                  ), 9);
    PutPixel(screenx, y02 - Round(scaley2 * Model.Phi / deg           ), 9);
    PutPixel(screenx, y03 - Round(scaley3 * Model.omegaDr / (deg / hr)), 9);

    if i * dt > 0.01 * tstop then
      begin
      PutPixel(screenx, y01 - Round(scaley1 * Filter.x[1, 1]             ), 14);
      PutPixel(screenx, y02 - Round(scaley2 * Filter.x[2, 1] / deg       ), 14);
      PutPixel(screenx, y03 - Round(scaley3 * Filter.x[3, 1] / (deg / hr)), 14);
      end;

    end; // for
 
  Read(Ch);
until Ch = #27;

SetScreenMode($03);
end.
