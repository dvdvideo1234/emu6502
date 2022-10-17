unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type
  aType = (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX, aZY, aRa);
  TypeSet = packed set of aType;
  TProcType = procedure() of object; // Method type
  FuncType = function(): bytep of object;
  Tprocary = array of TProcType;
  //ProcType = procedure ;
  oper{ation} = record
    p{roc}: TProcType;
    n{ame}: str3;
    o{pcode start}: byte;
    a{ddressation}: typeSet;
  end;
  operType = array of oper;


const
  aOnly = [aIX];
  aAll = [aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX];
  aShift = [aZ, aA, aZX, aAX];
  aMem = [aZ, aA];

type
  tCPU6502 = class(tRegisters)
    fDebug: boolean;
    Running: boolean;
    fopc: byte;
    fadrs: aType;
    fAdressation: array[aType] of FuncType;
    fopers: opertype;
    fopcodes: array[byte] of tProctype;
    fOpInds: byteAry;
    fmemory: wordAdressable;

  public
    procedure TablesMaker;
    procedure precodX;
    function NextByte: byte;
    function NextWord: word;

    function aINDX(): bytep;
    function aZP(): bytep;
    function aIMM(): bytep;
    function aABS(): bytep;
    function aINDY(): bytep;
    function aZPX(): bytep;
    function aABSY(): bytep;
    function aABSX(): bytep;
    function aZPY(): bytep;
    // ara
    function ptrValue: bytep;

    procedure doPatch(adr: word; Patch: byteAry);
    procedure dump(adr, cnt: word);

    function pop: byte;
    procedure push(b: byte);
    function popAdr: word;
    procedure pushAdr(w: word);

    {page1}
    procedure ORA;
    procedure ANDm;
    procedure EOR;
    procedure ADC;
    procedure STA;
    procedure LDA;
    procedure CMP;
    procedure SBC;

    {page2}
    procedure incm;
    procedure decm;
    procedure LDX;
    procedure STX;
    procedure ASL;
    procedure LSR;
    procedure ROL;
    procedure ROR;
    procedure nop;

    {page0}
    procedure CPY;
    procedure CPX;
    procedure STY;
    procedure LDY;
    procedure JPA;
    procedure JPI;
    procedure JSR;
    procedure RTS;
    procedure rti;
    procedure CSF;
    procedure BRK;
    procedure Rel;
    procedure PLA;
    procedure PHa;
    procedure PHp;
    procedure PLp;
    procedure BIT;

    constructor Create(StackBase: byte = 1);
    destructor Done;
    procedure ExecEmul(newPc: word);
    procedure Execute;
    procedure ShowTable;

    procedure Store(address: word; Value: byte);
    function Fetch(address: word): byte;
    function PageAdr(adr: word): word;

    property Debug: boolean read fDebug write fDebug;
    property bytes: wordAdressable read fMemory write fMemory;
  end;

var
  cpu: tCPU6502;


implementation

{memory}
function tCPU6502.Fetch(address: word): byte;
begin
  Result := fMemory[address];
end;

procedure tCPU6502.Store(address: word; Value: byte);
begin
  fMemory[address] := Value;
end;

function tCPU6502.PageAdr(adr: word): word;
begin
  wordRec(Result).lo := Fetch(adr);
  Inc(wordRec(adr).Lo);
  wordRec(Result).hi := Fetch(adr);
end;

procedure tCPU6502.doPatch(adr: word; Patch: byteAry);
var
  i: word;
begin
  for i := 0 to high(Patch) do
  begin
    bytes[adr] := patch[i];
    Inc(adr);
  end;
end;


procedure tCPU6502.dump(adr, cnt: word);
var
  i, ind: word;

  procedure NewLine;
  begin
    ind := 0;
    writeln;
    Write(word2hex(adr), '  ');
  end;

begin
  NewLine;
  for i := cnt downto 1 do
  begin
    Write(byte2hex(bytes[adr]), ' ');
    Inc(adr);
    Inc(ind);
    if ind = 20 then
      NewLine;
  end;
  writeln;
end;

{ptrValue of addressing memory }

function tCPU6502.ptrValue: bytep;
begin
  bytep(Result) := fAdressation[fadrs]();
end;

procedure tCPU6502.Execute;
const precod: array[aIX..aIm] of aTYpe = (aim, az, ara);
begin
  fopc := bytes[pc];
  fadrs := aType((fopc shr 2) and 7);
  if ((fopC and 1) = 0) and (fadrs in [aim, aIX])
     then  fadrs := precod[fadrs];
  fopcodes[fopc];
end;

procedure tCPU6502.nop;
begin
end;

function tCPU6502.aINDX(): bytep;
begin                       { ORA INDX }
  Result := @bytes[byte(PageAdr(NextByte + X))];
end;

function tCPU6502.aZP(): bytep;
begin                       { ORA ZP }
  Result := @bytes[NextByte];
end;

function tCPU6502.aIMM(): bytep;
begin
  PC := PC + 1;
  Result := @bytes[PC];
end;

function tCPU6502.aABS(): bytep;         { ORA ABS }
begin
  Result := @bytes[NextWord];
end;

function tCPU6502.aINDY(): bytep;
begin                      { ORA INDY }
  Result := @bytes[PageAdr(NextByte) + Y];
end;

function tCPU6502.aZPX(): bytep;
begin                      { ORA ZPX }
  Result := @bytes[byte(NextByte + X)];
end;

function tCPU6502.aABSY(): bytep;
begin                       { ORA ABSY }
  Result := @bytes[NextWord + Y];
end;

function tCPU6502.aABSX(): bytep;
begin                       { ORA ABSX }
  Result := @bytes[NextWord + X];
end;

function tCPU6502.aZPY(): bytep;
begin                      { STX ZPY }
  Result := @bytes[byte(NextByte + Y)];
end;


{tRegisters6502}

function tCPU6502.NextByte: byte;
begin
  pc := pc + 1;
  Result := bytes[pc];
end;

function tCPU6502.NextWord: word;
begin
  wordRec(Result).lo := NextByte;
  wordRec(Result).hi := NextByte;
end;



procedure tCPU6502.ORA;  begin A := A or ptrValue^; end;

procedure tCPU6502.ANDm; begin A := A and ptrValue^; end;

procedure tCPU6502.EOR;  begin A := A xor ptrValue^; end;

procedure tCPU6502.ADC;  begin AD(ptrValue^); end;

procedure tCPU6502.STA;  begin ptrValue^ := a; end;

procedure tCPU6502.LDA;  begin A := ptrValue^; end;

procedure tCPU6502.CMP;  begin cp(A, ptrValue^); end;

procedure tCPU6502.SBC;  begin SB(ptrValue^); end;

procedure tCPU6502.incm; begin incr(ptrValue^); end;

procedure tCPU6502.decm; begin decr(ptrValue^); end;

//aType = (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX, aZY, aRa);

const precod: array[azx..aax] of aTYpe = (azy, aAY, aay);
procedure tCPU6502.precodX; begin
  if fadrs in [azx,aax] then fadrs := precod[fadrs];
end;

procedure tCPU6502.STX;  begin  precodX; ptrValue^ := X; end;

procedure tCPU6502.LDX;  begin  precodX; X := ptrValue^; end;

procedure tCPU6502.ASL;  begin  ASLbase(ptrValue^); end;

procedure tCPU6502.LSR;  begin LSRbase(ptrValue^); end;

procedure tCPU6502.ROL;  begin ROLbase(ptrValue^); end;

procedure tCPU6502.ROR;  begin RORbase(ptrValue^); end;


procedure tCPU6502.CPY;  begin cp(Y, ptrValue^);  end;

procedure tCPU6502.CPX;  begin cp(X, ptrValue^);  end;

procedure tCPU6502.STY;  begin ptrValue^ := Y;    end;

procedure tCPU6502.LDY;  begin y := ptrValue^;    end;

procedure tCPU6502.JPA;  begin PC := NextWord; Execute; end;

procedure tCPU6502.JPI;  begin PC := PageAdr(NextWord); Execute; end;

procedure tCPU6502.JSR;  begin PushAdr(PC + 2); JPA; end;

procedure tCPU6502.RTS;  begin PC := Popadr; end;

procedure tCPU6502.PHa;  begin Push(a); end;

procedure tCPU6502.PLA;  begin A := Pop; end;

procedure tCPU6502.PHp;  begin Push(p or $40); end;

procedure tCPU6502.PLp;  begin p := Pop; end;

procedure tCPU6502.rti;  begin plp; RTS; end;

procedure tCPU6502.CSF;    {clear set flag}
var
  opIndex: byte;
const
  AfLAGS: array[0..3] of flags = (fc, fi, fv, fd);
begin
  opIndex := fopc shr 5;
  SetFlag(AfLAGS[(opIndex) shr 1], odd((opIndex)));
end;

procedure tCPU6502.BRK;
begin
  Running := False;
end;

procedure tCPU6502.Rel;
const
  AfLAGS: array[0..3] of flags = (fn, fv, fc, fz);
var
  offset: byte;
begin
  offset := nextbyte;
  if odd(fopc shr 5) and (AfLAGS[fopc shr 6] in state)
     then PC := pc + fromTwosCom(offset, $80);
end;

procedure tCPU6502.BIT;
var
  b: byte;
begin
  b := ptrValue^;
  setFlag(fz, (A and b) = 0);
  setFlags([fv, fn], b);
end;

procedure tCPU6502.ExecEmul(newPc: word);
begin
  Running := True;
  pc := newPc;
  while (Running) do
  begin
    Execute; {getOpCode and do housekeeping}
    pc := pc + 1;
  end;
end;

function tCPU6502.pop: byte;
begin
  Inc(wordRec(fsp).Lo);
  Result := bytes[fsp];
end;

procedure tCPU6502.push(b: byte);
begin
  bytes[fsp] := b;
  Dec(wordRec(fsp).Lo);
end;

function tCPU6502.popAdr: word;
begin
  wordRec(Result).lo := pop;
  wordRec(Result).Hi := pop;
end;

procedure tCPU6502.pushAdr(w: word);
begin
  push(wordRec(w).Hi);
  push(wordRec(w).lo);
end;

constructor tCPU6502.Create(StackBase: byte = 1);
begin
  regInit;
  setStackBase(StackBase);
  setLength(fOpers, 62);
  setLength(fOpInds, 256);
  TablesMaker;
end;

destructor tCPU6502.Done;
begin
  fopers := nil;
  fOpInds := nil;
end;

procedure tCPU6502.TablesMaker;
var
  i, cnt: integer;

  procedure expander(ind: integer);
  const
    step = 4;
  var
    j: aType;
    rec: oper;
  begin
    rec := fOpers[ind];
    if rec.a = aOnly then begin
      fopcodes[rec.o] := rec.p;
      fOpInds[rec.o] := ind;
      Inc(cnt);
    end else begin
      for j := aIX to aAX do begin
        if j in rec.a then  begin
          fopcodes[rec.o] := rec.p;
          fOpInds[rec.o] := ind;
          Inc(cnt);
        end;
        Inc(rec.o, step);
      end;
    end;
  end;

  procedure def(proc: TProcType; Name: str3;
  opcode {start}: byte; adrs{sation}: typeSet);
  var res: oper;
  begin
    Res.p := proc;
    Res.n := Name;
    Res.o := opcode;
    Res.a := adrs;
    fopers[i] := res;
    inc(i);
  end;

begin
  for i := 0 to high(fOpInds) do
    fOpInds[i] := $ff;

  fAdressation[aIX] := @aINDX;
  fAdressation[aZ]  := @aZP;
  fAdressation[aIM] := @aIMM;
  fAdressation[aA]  := @aABS;
  fAdressation[aIY] := @aINDY;
  fAdressation[aZX] := @aZPX;
  fAdressation[aAY] := @aABSY;
  fAdressation[aAX] := @aABSX;
  fAdressation[aZY] := @aZPY;
  fAdressation[aRa] := @Adra;

  i := 0;
  DEF(@ORA  { ORA  }, 'ORA', $01, aAll);
  DEF(@ANDm { ANDm }, 'AND', $21, aAll);
  DEF(@EOR  { EOR  }, 'EOR', $41, aAll);
  DEF(@ADC  { ADC  }, 'ADC', $61, aAll);
  DEF(@STA  { STA  }, 'STA', $81, aAll - [aIM]);
  DEF(@LDA  { LDA  }, 'LDA', $a1, aAll);
  DEF(@CMP  { CMP  }, 'CMP', $c1, aAll);
  DEF(@SBC  { SBC  }, 'SBC', $e1, aAll);
  DEF(@incm { incm }, 'inc', $e2, aShift);       {page2}
  DEF(@decm { decm }, 'dec', $c2, aShift);
  DEF(@LDX  { LDX  }, 'LDX', $a2, aShift + [aIX]);
  DEF(@STX  { STX  }, 'STX', $82, aShift-[aax]);
  DEF(@ASL  { ASL  }, 'ASL', $02, aShift + [aIM]);
  DEF(@ROL  { ROL  }, 'ROL', $22, aShift + [aIM]);
  DEF(@LSR  { LSR  }, 'LSR', $42, aShift + [aIM]);
  DEF(@ROR  { ROR  }, 'ROR', $62, aShift + [aIM]);
  DEF(@TXS  { TXS  }, 'TXS', $9a, aOnly);
  DEF(@TSX  { TSX  }, 'TSX', $ba, aOnly);
  DEF(@TXA  { TXA  }, 'TXA', $aa, aOnly);
  DEF(@TAX  { TAX  }, 'TAX', $8a, aOnly);
  DEF(@DEX  { DEX  }, 'DEX', $ca, aOnly);
  DEF(@nop  { nop  }, 'nop', $ea, aOnly);
  DEF(@CPY  { CPY  }, 'CPY', $c0, aMem + [aIX]);
  DEF(@CPX  { CPX  }, 'CPX', $e0, aMem + [aIX]);
  DEF(@STY  { STY  }, 'STY', $80, aMem + [aZX]);
  DEF(@LDY  { LDY  }, 'LDY', $a0, aShift + [aIX]);
  DEF(@JPA  { JPA  }, 'JPA', $4c, aOnly);
  DEF(@JPI  { JPI  }, 'JPI', $6c, aOnly);
  DEF(@JSR  { JSR  }, 'JSR', $20, aOnly);
  DEF(@RTS  { RTS  }, 'RTS', $60, aOnly);
  DEF(@rti  { rti  }, 'rti', $40, aOnly);
  DEF(@CSF  { CSF  }, 'CLC', $18, aOnly);
  DEF(@CSF  { CSF  }, 'CLI', $58, aOnly);
  DEF(@CLV  { CLV  }, 'CLV', $b8, aOnly);
  DEF(@CSF  { CSF  }, 'CLD', $d8, aOnly);
  DEF(@CSF  { CSF  }, 'SEC', $38, aOnly);
  DEF(@CSF  { CSF  }, 'SEI', $78, aOnly);
  DEF(@CSF  { CSF  }, 'SED', $f8, aOnly);
  DEF(@BRK  { BRK  }, 'BRK', $00, aOnly);
  DEF(@rel  { rel  }, 'BPL', $10, aOnly);
  DEF(@rel  { rel  }, 'BMI', $30, aOnly);
  DEF(@rel  { rel  }, 'BVC', $50, aOnly);
  DEF(@rel  { rel  }, 'BVS', $70, aOnly);
  DEF(@rel  { rel  }, 'BCC', $90, aOnly);
  DEF(@rel  { rel  }, 'BCS', $b0, aOnly);
  DEF(@rel  { rel  }, 'BNE', $d0, aOnly);
  DEF(@rel  { rel  }, 'BEQ', $f0, aOnly);
  DEF(@BIT  { BIT  }, 'BIT', $20, aMem);
  DEF(@PHp  { PHp  }, 'PHp', $08, aOnly);
  DEF(@PLp  { PLp  }, 'PLp', $28, aOnly);
  DEF(@PHa  { PHa  }, 'PHa', $48, aOnly);
  DEF(@PLA  { PLA  }, 'PLA', $68, aOnly);
  DEF(@INY  { INY  }, 'INY', $c8, aOnly);
  DEF(@DEY  { DEY  }, 'DEY', $88, aOnly);
  DEF(@INX  { INX  }, 'INX', $e8, aOnly);
  DEF(@TAY  { TAY  }, 'TAY', $98, aOnly);
  DEF(@TYA  { TYA  }, 'TYA', $a8, aOnly);
  cnt := 0;
  for i := 0 to high(fOpers) do
    expander(i);
  writeln(cnt);
end;

procedure tCPU6502.ShowTable;
var
  i: word;
  j, v, h, r, c: byte;
begin
  for i := 0 to 255 do
  begin
    if (i and 15) = 0 then
      writeln;
    if (i and 127) = 0 then
      writeln;
    if ((i - 8) and 15) = 0 then
      Write('  ');
    c := i and 7;
    v := i shr 7;
    r := (i shr 4) and 7;
    h := (i shr 3) and 1;
    j := c shl 2 + v shl 1 + h + r shl 5;

    if fOpInds[j] <> 255 then
      Write(fOpers[fOpInds[j]].n)
    else
      Write('---');
    Write(' ');
  end;
  writeln;
end;

initialization

  cpu := tCPU6502.Create();
  cpu.ShowTable;

finalization

  cpu.Done;

end.
