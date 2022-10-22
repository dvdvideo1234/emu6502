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
var    opIndex: byte;
const  AfLAGS: array[0..3] of flags = (fc, fi, fv, fd);
begin  opIndex := fopc shr 5;
  SetFlag(AfLAGS[(opIndex) shr 1], odd((opIndex)));
end;

procedure tCPU6502.BRK;
begin
  Running := False;
end;

procedure tCPU6502.Rel;
const   AfLAGS: array[0..3] of flags = (fn, fv, fc, fz);
var     offset: byte;
begin   offset := nextbyte;
  if odd(fopc shr 5) and (AfLAGS[fopc shr 6] in state)
     then PC := pc + fromTwosCom(offset, $80);
end;

procedure tCPU6502.BIT;
var   b: byte;
begin b := ptrValue^;
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
    const  step = 4;
    var    j: aType;  rec: oper;
    procedure exp1; begin
      if fopcodes[rec.o] <> nil then writeln(fOpers[ind].n);
      fopcodes[rec.o] := rec.p;
      fOpInds[rec.o] := ind;
      Inc(cnt);
    end;
  begin   rec := fOpers[ind];
    if rec.a = aOnly then exp1
    else  for j := aIX to aAX do begin
      if j in rec.a then  exp1;
      Inc(rec.o, step);
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
  DEF(@ORA  { ORA  }, 'ORA', $01, aALL);
  DEF(@ANDM { ANDM }, 'AND', $21, aALL);
  DEF(@EOR  { EOR  }, 'EOR', $41, aALL);
  DEF(@ADC  { ADC  }, 'ADC', $61, aALL);
  DEF(@STA  { STA  }, 'STA', $81, aALL - [AIM]);
  DEF(@LDA  { LDA  }, 'LDA', $A1, aALL);
  DEF(@CMP  { CMP  }, 'CMP', $C1, aALL);
  DEF(@SBC  { SBC  }, 'SBC', $E1, aALL);
  DEF(@INCM { INCM }, 'INC', $E2, aSHIFT);       {PAGE2}
  DEF(@DECM { DECM }, 'DEC', $C2, aSHIFT);
  DEF(@LDX  { LDX  }, 'LDX', $A2, aSHIFT + [AIX]);
  DEF(@STX  { STX  }, 'STX', $82, aSHIFT - [AAX]);
  DEF(@ASL  { ASL  }, 'ASL', $02, aSHIFT + [AIM]);
  DEF(@ROL  { ROL  }, 'ROL', $22, aSHIFT + [AIM]);
  DEF(@LSR  { LSR  }, 'LSR', $42, aSHIFT + [AIM]);
  DEF(@ROR  { ROR  }, 'ROR', $62, aSHIFT + [AIM]);
  DEF(@TXS  { TXS  }, 'TXS', $9A, aONLY);
  DEF(@TSX  { TSX  }, 'TSX', $BA, aONLY);
  DEF(@TXA  { TXA  }, 'TXA', $AA, aONLY);
  DEF(@TAX  { TAX  }, 'TAX', $8A, aONLY);
  DEF(@DEX  { DEX  }, 'DEX', $CA, aONLY);
  DEF(@NOP  { NOP  }, 'NOP', $EA, aONLY);
  DEF(@CPY  { CPY  }, 'CPY', $C0, aMEM  +  [AIX]);
  DEF(@CPX  { CPX  }, 'CPX', $E0, aMEM  +  [AIX]);
  DEF(@STY  { STY  }, 'STY', $80, aMEM  +  [AZX]);
  DEF(@LDY  { LDY  }, 'LDY', $A0, aSHIFT + [AIX]);
  DEF(@JPA  { JPA  }, 'JPA', $4C, aONLY);
  DEF(@JPI  { JPI  }, 'JPI', $6C, aONLY);
  DEF(@JSR  { JSR  }, 'JSR', $20, aONLY);
  DEF(@RTS  { RTS  }, 'RTS', $60, aONLY);
  DEF(@RTI  { RTI  }, 'RTI', $40, aONLY);
  DEF(@CSF  { CSF  }, 'CLC', $18, aONLY);
  DEF(@CSF  { CSF  }, 'CLI', $58, aONLY);
  DEF(@CLV  { CLV  }, 'CLV', $B8, aONLY);
  DEF(@CSF  { CSF  }, 'CLD', $D8, aONLY);
  DEF(@CSF  { CSF  }, 'SEC', $38, aONLY);
  DEF(@CSF  { CSF  }, 'SEI', $78, aONLY);
  DEF(@CSF  { CSF  }, 'SED', $F8, aONLY);
  DEF(@BRK  { BRK  }, 'BRK', $00, aONLY);
  DEF(@REL  { REL  }, 'BPL', $10, aONLY);
  DEF(@REL  { REL  }, 'BMI', $30, aONLY);
  DEF(@REL  { REL  }, 'BVC', $50, aONLY);
  DEF(@REL  { REL  }, 'BVS', $70, aONLY);
  DEF(@REL  { REL  }, 'BCC', $90, aONLY);
  DEF(@REL  { REL  }, 'BCS', $B0, aONLY);
  DEF(@REL  { REL  }, 'BNE', $D0, aONLY);
  DEF(@REL  { REL  }, 'BEQ', $F0, aONLY);
  DEF(@BIT  { BIT  }, 'BIT', $20, aMEM);
  DEF(@PHP  { PHP  }, 'PHP', $08, aONLY);
  DEF(@PLP  { PLP  }, 'PLP', $28, aONLY);
  DEF(@PHA  { PHA  }, 'PHA', $48, aONLY);
  DEF(@PLA  { PLA  }, 'PLA', $68, aONLY);
  DEF(@INY  { INY  }, 'INY', $C8, aONLY);
  DEF(@DEY  { DEY  }, 'DEY', $88, aONLY);
  DEF(@INX  { INX  }, 'INX', $E8, aONLY);
  DEF(@TAY  { TAY  }, 'TAY', $98, aONLY);
  DEF(@TYA  { TYA  }, 'TYA', $A8, aONLY);
  CNT := 0;
  for i := 0 to high(fOpers) do
    expander(i);
  write(cnt);
end;

procedure tCPU6502.ShowTable;
var i, j: byte; //, v, h, r, c: byte;
  {c := i and 7;  v := i shr 7; r := (i shr 4) and 7;
  h := (i shr 3) and 1;  j := c shl 2 + v shl 1 + h + r shl 5;}

  procedure amiddle(i: byte);  begin
    if (i and 7) = 0 then   Write('  ');  end;

  procedure aRow; var n: integer; begin writeln; writeln; write('   ');
    for n := i to i+15 do begin   j := rold(rold(n,7),4);
      amiddle(n);
      write(byte2hex(j and $1f),'  ');
    end;
  end;

begin
  write(' aIX aZ  aIM aA  aIY aZX aAY aAX   aIX aZ  aIM aA  aIY aZX aAY aAX');
  for i := 0 to 255 do  begin
    if (i and 127) = 0 then  aRow;
    j := rold(rold(i,7),4); // and 255;
    if ((i) and 15)  = 0 then  write(#13#10,byte2hex(j and $f0));
    amiddle(i);
    if fOpInds[j] <> 255 then  Write(fOpers[fOpInds[j]].n)
                         else  Write('---');
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
