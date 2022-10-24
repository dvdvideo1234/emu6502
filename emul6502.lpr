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
  //clrscr;
  cpu.ShowTable;
  cpu.Debug := true;
  cpu.doPatch($200, testary);
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

  cpu.ExecEmul($200);
  readln;
end.

