program emul6502;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, BaseUnit, EmuUnit
  { you can add units after this }
  ,crt
  ;

begin
  //writeln(sizeof(tRegisters));
  clrscr;
  cpu.ShowTable;
  crt.readkey;
  clrscr;
  cpu.Debug := true;
  cpu.doPatch($800, testary);
  cpu.ExecEmul($800);
  writeln;

  {
  for counter :=0 to 300 do
      cpu.bytes[counter] := counter;
  //cpu.dump(0, 330);
  for counter :=0 to 255 do begin
    cpu.disasm(counter, 1);
    if (succ(counter) and 15) = 0 then crt.readkey;
  end;
  }

  //
  readln;
end.

