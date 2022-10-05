program emul6502;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, EmuUnit, BaseUnit
  { you can add units after this };

begin
  writeln('emul6502');
  writeln;
  //testary := (0,1, 2, 3, 4, 5, 6, 7);

  stack.setStackBase();   { in hex $100}
  regs.InitEmul;
  regs.DebugOn := true;
  mem.doPatch($200, testary);
  mem.dump($200,8);
  regs.ExecEmul($200);
  readln;
end.

