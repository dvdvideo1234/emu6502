program emul6502;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, EmuUnit
  { you can add units after this };

begin
  writeln('emul6502');
  //stack;
  readln;
end.

