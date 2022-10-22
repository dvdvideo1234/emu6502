program emul6502;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, EmuUnit, BaseUnit
  { you can add units after this }
  ,crt
  ;

begin
  for counter :=0 to 300 do
      cpu.bytes[counter] := counter;
  cpu.Debug := true;
  //cpu.doPatch($200, testary);
  cpu.dump(0, 330);
  writeln;
  //cpu.ExecEmul($200);
  readln;
end.

