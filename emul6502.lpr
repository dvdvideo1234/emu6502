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
//  tt := 98;
//  writeln(divmod(tt,10),' ',tt);
//  writeln;
  readln;
  //testary := (0,1, 2, 3, 4, 5, 6, 7);

  //stack.setStackBase();   { in hex $100}
  cpu.Debug := true;
  cpu.doPatch($200, testary);
  cpu.dump($200,8);
  cpu.ExecEmul($200);
  readln;
end.

