// Cannabola plot program


program Cannabis;



const
  dt0 = 0.003;
  scale = 120;


var
  r, rold, rdot, t, dt, x, y: Real;


begin
SetScreenMode($10);   // 640 x 350 pixels, 16 colors

t    := 0;
dt   := dt0;
rold := 0;

while t <= 2 * pi do
  begin
  r := (1 + sin(t)) * (1 + 0.9 * cos(8 * t)) * (1 + 0.1 * cos(24 * t)) * (0.5 + 0.05 * cos(200 * t));

  x := r * cos(t);
  y := r * sin(t);

  rdot := abs(r - rold) / dt;

  dt := dt0 / (1 + rdot);

  PutPixel(320 + Round(scale * x), 290 - Round(scale * y), 10);
  
  t := t + dt;
  rold := r;
  end;

repeat until KeyPressed;
SetScreenMode($03);
end.