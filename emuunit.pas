unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type
  aType = (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX, aZY, aRa, aRl, aI);
  bType = aIx..aAx;
  TypeSet = packed set of bType;
  TProcType = procedure() of object; // Method type
  FuncType = function(): bytep of object;

  oper{ation} = record
    p{roc}: TProcType;
    n{ame}: str3;
    o{pcode start}: byte;
    a{ddressation}: typeSet;
  end;
  operType = array of oper;


const
  aLen: array[atype] of byte = (1,1,1,2,1,1,2,2,1,0,1,2);
  aOnly = [aIX];
  aAll = [aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX];
  aShift = [aZ, aA, aZX, aAX];
  aMem = [aZ, aA];

type
  tCPU6502 = class
    regs: tRegisters;

    fDebug: boolean;
    Running: boolean;

    VALptr: bytep;
    fopc: byte;
    fadrs: aType;
    fAdressation: array[aType] of FuncType;
    fopers: opertype;
    //fopcodes: array[byte] of tProctype;
    fprecode: array[byte] of aType;
    fOpInds: byteAry;
    fmemory: wordAdressable;

  public
    procedure Execute;          function  disasm(adr, cnt: word): word;
    procedure TablesMaker;      function  precodX(a: byte): atype;

    function  NextByte: byte;   function  Fetch(address: word): byte;
    function  NextWord: word;   procedure Store(address: word; Value: byte);
    function  PageAdr(adr: word): word;

    function  pop: byte;        procedure push(b: byte);
    function  popAdr: word;     procedure pushAdr(w: word);

    function  GetState: flagSet;procedure SetState(f: flagSet);
    function  GetPc: word;      procedure SetPc(w: word);
    procedure SetA(b: byte);    procedure SetY(b: byte);
    procedure SetX(b: byte);    function  GetSp: word;

    function aINDX(): bytep;    function aZP(): bytep;
    function aIMM(): bytep;     function aABS(): bytep;
    function aINDY(): bytep;    function aZPX(): bytep;
    function aABSY(): bytep;    function aABSX(): bytep;
    function aZPY(): bytep;     function aRacc():bytep;

    {page1}
    procedure ORA;    procedure ANDm;   procedure EOR;    procedure ADC;
    procedure STA;    procedure LDA;    procedure CMP;    procedure SBC;

    {page2}
    procedure incm;   procedure decm;   procedure LDX;    procedure STX;
    procedure ASL;    procedure LSR;    procedure ROL;    procedure ROR;
    procedure nop;    procedure TAX;    procedure TXA;    procedure TSX;
    procedure TXS;    procedure DEX;

    {page0}
    procedure BRK;    procedure CSF;    procedure Rel;    procedure CLV;
    procedure CPY;    procedure CPX;    procedure STY;    procedure LDY;
    procedure JPA;    procedure JPI;    procedure JSR;    procedure RTS;
    procedure PLA;    procedure PHa;    procedure PHp;    procedure PLp;
    procedure BIT;    procedure INY;    procedure DEY;    procedure INX;
    procedure TAY;    procedure TYA;    procedure rti;

    constructor Create(StackBase: byte = 1);
    destructor Done;
    procedure ExecEmul(newPc: word);
    procedure ShowTable;
    procedure doPatch(adr: word; Patch: byteAry);
    procedure dump(adr, cnt: word);

    property y:  byte read regs.YReg.fReg write SetY;
    property a:  byte read regs.aReg.fReg write SetA;
    property x:  byte read regs.xReg.fReg write SetX;
    property p:  byte read regs.PReg.fReg write regs.PReg.fReg;
    property pc: word read GetPc write SetPc;
    property SP: word read GetSP;
    property state: flagSet read GetState write SetState;

    property Debug: boolean read fDebug write fDebug;
    property bytes: wordAdressable read fMemory write fMemory;
  end;

var
  cpu: tCPU6502;
  counter: word;


implementation

uses crt;
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

  function tCPU6502.precodX(a: byte): atype;
  const  // (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX, aZY, aRa, aRl, aI);
    precod_0: array[aIX..aax] of aTYpe =
              (aim, az, ara, aA, aRl, azx, aAY, aax);
    precod_1: array[aIX..aax] of aTYpe =
              (aim, az, ara, aA, aRa, azx, aAY, aax);
    precod_2: array[aIX..aax] of aTYpe =
              (aim, az, ara, aA, aRa, azy, aAY, aay);
  var ind: byte;
  begin  ind := fOpInds[a];
    if (ind = 0) or (fOpers[ind].a = aONLY) then exit(aRa);
    result := bType((a shr 2) and 7);
    case (a and 3) of
      0: case a of
         $20: exit(aA);
         $6c: exit(aI);
         else result := precod_0[result];
         end;
      2: if a in [$96 ,$b6, $be] then exit( precod_2[result])
           else result := precod_1[result];
    end;
  end;

  procedure tCPU6502.Execute;
  begin
    fopc := bytes[Pc];
    fadrs := fprecode[fopc];
    If Debug
      Then begin
        Write(
           ' Y=',   byte2hex(Y),
           ' A=',   byte2hex(A),
           ' X=',   byte2hex(X),
           ' P=',   byte2hex(P), '   ');
        disasm(pc, 1);
        crt.readkey;
      end;
    valPtr := fAdressation[fadrs]();
    fopers[fOpInds[fopc]].p;
  end;

  procedure tCPU6502.nop;
  begin
  end;

  function tCPU6502.aINDX(): bytep;
  begin                       { ORA INDX }
    Result := @bytes[PageAdr(byte(NextByte + X))];
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

  function tCPU6502.aracc: bytep;
  begin
    result := @regs.aReg.fReg;
  end;


  {tRegisters6502}

  function tCPU6502.NextByte: byte;
  begin
    pc := pc + 1;
    Result := bytes[Pc];
  end;

  function tCPU6502.NextWord: word;
  begin
    wordRec(Result).lo := NextByte;
    wordRec(Result).hi := NextByte;
  end;

  procedure tCPU6502.CSF;    {clear set flag}
  var    opIndex: byte;
  const  AfLAGS: array[0..3] of tCPUflag = (fc, fi, fv, fd);
  begin  opIndex := fopc shr 5;
    regs.pReg.SetFlag(AfLAGS[(opIndex) shr 1], odd((opIndex)));
  end;

  procedure tCPU6502.Rel;
  const   AfLAGS: array[0..3] of tCPUflag = (fn, fv, fc, fz);
  begin
    if odd(fopc shr 5) and (AfLAGS[fopc shr 6] in state)
       then PC := pc + shortInt(valPtr^);
  end;

  procedure tCPU6502.BIT;
  begin
    regs.pReg.SetFlag(fz, (A and valPtr^) = 0);
    regs.pReg.setFlags([fv, fn], valPtr^);
  end;


  procedure tCPU6502.BRK;  begin    Running := False;  end;

  procedure tCPU6502.EOR;  begin a := regs.pReg.ZN(a  xor valPtr^); end;

  procedure tCPU6502.ORA;  begin a := a or valPtr^; end;

  procedure tCPU6502.ANDm; begin a := a  and valPtr^; end;

  procedure tCPU6502.LDA;  begin A := valPtr^; end;

  procedure tCPU6502.STA;  begin valPtr^ := a; end;

  procedure tCPU6502.ADC;  begin regs.AD(valPtr^); end;

  procedure tCPU6502.CMP;  begin regs.cp(A, valPtr^); end;

  procedure tCPU6502.SBC;  begin regs.SB(valPtr^); end;


  procedure tCPU6502.incm; begin regs.incr(valPtr^); end;

  procedure tCPU6502.decm; begin regs.decr(valPtr^); end;

  procedure tCPU6502.STX;  begin  valPtr^ := X; end;

  procedure tCPU6502.LDX;  begin  X := valPtr^; end;

  procedure tCPU6502.ASL;  begin  regs.RL(valPtr^); end;

  procedure tCPU6502.LSR;  begin regs.RR(valPtr^); end;

  procedure tCPU6502.ROL;  begin regs.RL(valPtr^,regs.pReg.GetC); end;

  procedure tCPU6502.ROR;  begin regs.rr(valPtr^,regs.pReg.GetC); end;

  procedure tCPU6502.CPY;  begin regs.cp(Y, valPtr^);  end;

  procedure tCPU6502.CPX;  begin regs.cp(X, valPtr^);  end;

  procedure tCPU6502.STY;  begin valPtr^ := Y;    end;

  procedure tCPU6502.LDY;  begin y := valPtr^;    end;

  procedure tCPU6502.JPA;  begin PC := NextWord; end;

  procedure tCPU6502.JPI;  begin PC := PageAdr(NextWord); end;

  procedure tCPU6502.JSR;  begin PushAdr(Pc + 2); JPA; end;

  procedure tCPU6502.RTS;  begin PC := Popadr; end;

  procedure tCPU6502.PHa;  begin Push(a); end;

  procedure tCPU6502.PLA;  begin A := Pop; end;

  procedure tCPU6502.PHp;  begin Push(p or $20); end;

  procedure tCPU6502.PLp;  begin p := Pop; end;

  procedure tCPU6502.rti;  begin plp; RTS; end;


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
    regs.sReg.incr;
    Result := bytes[sp];
  end;

  procedure tCPU6502.push(b: byte);
  begin
    bytes[sp] := b;
    regs.sReg.decr;
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
    regs.shReg.fReg:= StackBase;
    //regInit;
    setLength(fOpers, 63);
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
        if fOpInds[rec.o] <> 0 then writeln(fOpers[ind].n);
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


  begin   //(aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX, aZY, aRa, aRl, aI)
    fAdressation[aIX] := @aINDX;
    fAdressation[aZ]  := @aZP;
    fAdressation[aIM] := @aIMM;
    fAdressation[aA]  := @aABS;
    fAdressation[aIY] := @aINDY;
    fAdressation[aZX] := @aZPX;
    fAdressation[aAY] := @aABSY;
    fAdressation[aAX] := @aABSX;
    fAdressation[aZY] := @aZPY;
    fAdressation[aRa] := @aracc;
    fAdressation[aRl] := @aIMM;
    fAdressation[aI ] := @aracc;

    i := 0;
    DEF(@BRK  { ilg  }, '-?-', $01, []);
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
    DEF(@DEX  { DEX  }, 'DEX', $CA, aONLY);
    DEF(@NOP  { NOP  }, 'NOP', $EA, aONLY);
    DEF(@CPY  { CPY  }, 'CPY', $C0, aMEM  +  [AIX]);
    DEF(@CPX  { CPX  }, 'CPX', $E0, aMEM  +  [AIX]);
    DEF(@STY  { STY  }, 'STY', $80, aMEM  +  [AZX]);
    DEF(@LDY  { LDY  }, 'LDY', $A0, aSHIFT + [AIX]);
    DEF(@JPA  { JMP  }, 'JMP', $40, [aA]);   // $4C
    DEF(@JPI  { JPI  }, 'JMP', $60, [aA]);   // $6C
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
    DEF(@TAY  { TAY  }, 'TAY', $A8, aONLY);
    DEF(@TYA  { TYA  }, 'TYA', $98, aONLY);
    DEF(@TXS  { TXS  }, 'TXS', $9A, aONLY);
    DEF(@TSX  { TSX  }, 'TSX', $BA, aONLY);
    DEF(@TXA  { TXA  }, 'TXA', $8A, aONLY);
    DEF(@TAX  { TAX  }, 'TAX', $AA, aONLY);
    CNT := 0;
    for i := 0 to high(fOpers) do expander(i);
    for i := 0 to high(byte) do fprecode[i] := precodx(i);
  end;

  procedure tCPU6502.ShowTable;
    var i, j: byte; //, v, h, r, c: byte;
    const adr_s = 'aIX aZ  aIM aA  aIY aZX aAY aAX';
    {c := i and 7;  v := i shr 7; r := (i shr 4) and 7;
    h := (i shr 3) and 1;  j := c shl 2 + v shl 1 + h + r shl 5;}
    procedure amiddle(i: byte);  begin
      if (i and 7) = 0 then   Write('  ');  end;
    procedure aRow; var n: integer; begin write(#13#10#13#10'   ');
      for n := i to i+15 do begin   j := rold(rold(n,7),4);
        amiddle(n);
        write(byte2hex(j and $1f),'  ');
      end;
    end;
  begin
    write(#13#10'    ', adr_s, '   ', adr_s);
    for i := 0 to 255 do  begin
      if (i and 127) = 0 then  aRow;
      j := rold(rold(i,7),4);
      if ((i) and 15)  = 0 then  write(#13#10,byte2hex(j and $f0));
      amiddle(i);
      Write(fOpers[fOpInds[j]].n,' ');
    end;
    writeln;
  end;

  function tCPU6502.disasm(adr, cnt: word): word;
    procedure dis1();
    var val, b, len: byte;  adrs: aType; disp: array[0..2] of byte;
    begin  b := bytes[adr];adrs := precodx(b);{index or operand lemgth by type}
      len := aLen[adrs];{printing} write(word2hex(adr),'  ');  {address}
      for val := 0 to len do begin  disp[val] :=  bytes[adr]; inc(adr);
        write(byte2hex(disp[val]),' ');  { bytes}  end;
      for val := 1 to 2-len do write('   ');
      write(' ',fOpers[fOpInds[b]].n,'  ');     { name}
      if len <> 0 then begin             {addressation = no impl | no acc}
        if (adrs in  [aIX, aIY, aI]) then write('(')
          else if adrs = aIM then write('#');
        if adrs = aRl then write('$',word2hex(adr + shortint(disp[1]))) {rel adr}
          else if len = 1 then BEGIN write('$',byte2hex(disp[1]));      {byte}
            if (adrs = aIY) then write(')'); end
          else write('$',word2hex(disp[1] + disp[2] shl 8));              {word}
        case adrs of
        aIY, aZY, aAY: write(',Y');
        aZX, aAX, aIx: write(',X');
        end;
        if adrs in [aIX, aI] then write(')');
      end;
      writeln;
    end;
  begin     for cnt := cnt downto 1 do dis1; result := adr; end;

  function tCPU6502.GetPc: word;
  begin
    result := pword(@regs.pcLo)^;
  end;

  function tCPU6502.GetSp: word;
  begin
    result := pword(@regs.sReg)^;
  end;

  procedure tCPU6502.SetPc(w: word);
  begin
    pword(@regs.pcLo)^ := w;
  end;

  function  tCPU6502.GetState: flagSet;
  begin
    result := pFlagset(@regs.preg.freg)^;
  end;

  procedure tCPU6502.SetState(f: flagSet);
  begin
    pFlagset(@regs.preg.freg)^ := f;
  end;

  procedure tCPU6502.SetA(b: byte);
  begin
    regs.aReg.fReg := regs.preg.zn(b);
  end;

  procedure tCPU6502.SetY(b: byte);
  begin
    regs.YReg.fReg := regs.preg.zn(b);
  end;

  procedure tCPU6502.SetX(b: byte);
  begin
    regs.xReg.fReg := regs.preg.zn(b);
  end;

  procedure tCPU6502.INY;
  begin
    Y := Y+ 1;
  end;

  procedure tCPU6502.DEY;
  begin
    Y := Y - 1;
  end;

  procedure tCPU6502.INX;
  begin
    X := X + 1;
  end;

  procedure tCPU6502.DEX;
  begin
    X := X - 1;
  end;

  procedure tCPU6502.TAY;
  begin
    y := a;
  end;

  procedure tCPU6502.TYA;
  begin
    A := Y;
  end;

  procedure tCPU6502.TAX;
  begin
    x := a;
  end;

  procedure tCPU6502.TXA;
  begin
    a := x;
  end;

  procedure tCPU6502.TSX;
  begin
    x := regs.sReg.fReg;
  end;

  procedure tCPU6502.TXS;
  begin
    regs.sReg.fReg := x;
  end;

  procedure tCPU6502.CLV;
  begin
    regs.preg.clrFlag(fv);
  end;


initialization

  cpu := tCPU6502.Create();

finalization

  cpu.Done;

end.


