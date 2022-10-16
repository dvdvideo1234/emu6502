unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type
  aType = (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX);
  TypeSet = packed set of aType;
  TProcType = procedure() of object; // Method type
  FuncType= function() : bytep of object;
  Tprocary = array of TProcType;
  //ProcType = procedure ;
  oper{ation}= record
    p{roc}: TProcType;
    n{ame}: str3;
    o{pcode start}: byte;
    a{ddressation}: typeSet;
  end;
  operType = array of oper;


const
  aOnly = [aIX];
  aAll  = [aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX];
  aShift= [aZ, aA, aZX, aAX];
  aMem  = [aZ, aA];

type
  tCPU6502 = CLASS  (tRegisters)
    fDebug: boolean;
    Running: boolean;
    fopc, fadrs: byte;
    fAdressation: array[aType] of FuncType;
    fopers: opertype;
    fopcodes: array[byte] of tProctype;
    fOpInds: byteAry;
    fmemory: wordAdressable;

  public
    class function def(proc: TProcType; name: str3;
        opcode {start}: byte;  adrs{sation}: typeSet): oper;
    Procedure TablesMaker;

    function aINDX(): bytep;    function aZP(): bytep;
    function aIMM(): bytep;     function aABS(): bytep;
    function aINDY(): bytep;    function aZPX(): bytep;
    function aABSY(): bytep;    function aABSX(): bytep;
    function aZPY(): bytep;     function ptrValue: bytep;
    function NextByte: byte;    function NextWord: word;

    procedure doPatch(adr: word; Patch: byteAry);
    procedure dump(adr, cnt: word);

    function  pop: byte;        procedure push(b: byte);
    function  popAdr: word;     procedure pushAdr(w: word);

    {page1}
    procedure ORA;    procedure ANDm;    procedure EOR;    procedure ADC;
    procedure STA;    procedure LDA;     procedure CMP;    procedure SBC;

    {page2}
    procedure incm;   procedure decm;    procedure LDX;    procedure STX;
    procedure ASL;    procedure LSR;     procedure ROL;    procedure ROR;
    procedure iLDX;   procedure LXzx;    procedure LXax;   procedure SXzx;
    procedure nop;

    {page0}
    procedure CPY;    procedure CPX;     procedure STY;    procedure LDY;
    procedure JPA;    procedure JPI;     procedure JSR;    procedure RTS;
    procedure rti;    procedure CSF;     procedure BRK;    procedure Rel;
    procedure PLA;    procedure PHa;     procedure PHp;    procedure PLp;
    procedure iCPY;   procedure iCPX;    procedure iLDY;   procedure BIT;

    //Procedure MakeOps(OpName: str3; cpuOp : TProcType;
    //    opStart: byte;  validAdr: TypeSet);
    constructor Create(StackBase: byte = 1 );
    Destructor Done;
    procedure ExecEmul(newPc: word);
    procedure execute;
    procedure ShowTable;

    procedure Store(address: word; Value: byte);
    function  Fetch(address: word): byte;
    function  PageAdr(adr: word): word;

    property Debug: boolean read fDebug write fDebug;
    property bytes : wordAdressable read fMemory write fMemory;
  end;

var cpu: tCPU6502;


implementation

  class function tCPU6502.def(proc: TProcType; name: str3;
      opcode {start}: byte;  adrs{sation}: typeSet): oper;
  begin
    result.p:= proc;
    result.n:= name;
    result.o:= opcode;
    result.a:= adrs;
  end;

  {memory}
  function tCPU6502.Fetch(address: word): byte;
  begin
    result := fMemory[address];
  end;

  procedure tCPU6502.Store(address: word; value: byte);
  begin
    fMemory[address] := Value;
  end;

  function tCPU6502.PageAdr(adr: word): word;
  begin
    wordRec(result).lo := Fetch(adr);
    inc(wordRec(adr).Lo);
    wordRec(result).hi := Fetch(adr);
  end;

  procedure tCPU6502.doPatch(adr: word; Patch: byteAry);
  var i: word;
  begin
    for i := 0 to high(Patch) do begin
      bytes[adr] := patch[i];
      inc(adr);
    end;
  end;


  procedure tCPU6502.dump(adr, cnt: word);
    var i, ind: word;

    procedure NewLine;
    begin
      ind := 0;
      writeln;
      write(word2hex(adr), '  ');
    end;

  begin NewLine;
    for i := cnt downto 1 do begin
      write(byte2hex(bytes[adr]), ' ');
      inc(adr);
      inc(ind);
      if ind = 20  then NewLine
    end;
    writeln;
  end;

{ptrValue of addressing memory }

  function tCPU6502.ptrValue: bytep;
  begin
    bytep(result) := fAdressation[aType(fadrs)]();
  end;

  procedure tCPU6502.execute;
  begin
    fopc := bytes[pc];
    fadrs:= (fopc shr 2) and 7;
    fopcodes[fopc];
  end;

  procedure tCPU6502.nop; begin end;

  function tCPU6502.aINDX(): bytep;
  begin                       { ORA INDX }
    result := @bytes[byte(PageAdr(NextByte + X))];
  end;

  function tCPU6502.aZP(): bytep;
  begin                       { ORA ZP }
    result := @bytes[NextByte];
  end;

  function tCPU6502.aIMM(): bytep;
  begin PC := PC + 1; result:= @bytes[PC]; end;

  function tCPU6502.aABS(): bytep;         { ORA ABS }
  begin
    result := @bytes[NextWord];
  end;

  function tCPU6502.aINDY(): bytep;
  begin                      { ORA INDY }
    result := @bytes[PageAdr(NextByte) + Y];
  end;

  function tCPU6502.aZPX(): bytep;
  begin                      { ORA ZPX }
    result := @bytes[byte(NextByte + X)];
  end;

  function tCPU6502.aABSY(): bytep;
  begin                       { ORA ABSY }
    result := @bytes[NextWord + Y];
  end;

  function tCPU6502.aABSX(): bytep;
  begin                       { ORA ABSX }
    result := @bytes[NextWord + X];
  end;

  function tCPU6502.aZPY(): bytep;
  begin                      { STX ZPY }
    result := @bytes[byte(NextByte + Y)];
  end;


{tRegisters6502}

  function tCPU6502.NextByte: byte;
  begin
      pc := pc + 1;
      result := bytes[pc];
  end;

  function tCPU6502.NextWord: word;
  begin
    wordRec(result).lo := NextByte;
    wordRec(result).hi := NextByte;
  end;



  procedure tCPU6502.ORA; begin A := A  or ptrValue^; end;
  procedure tCPU6502.ANDm;begin A := A AND ptrValue^; ;end;
  procedure tCPU6502.EOR; begin A := A Xor ptrValue^;  end;
  procedure tCPU6502.ADC; begin AD(ptrValue^); end;
  procedure tCPU6502.STA; begin ptrValue^ := a; end;
  procedure tCPU6502.LDA; begin A := ptrValue^; end;
  procedure tCPU6502.CMP; begin cp(A, ptrValue^); end;
  procedure tCPU6502.SBC; begin SB(ptrValue^); end;


  procedure tCPU6502.incm; begin zn(incr(ptrValue^)); end;
  procedure tCPU6502.decm; begin zn(decr(ptrValue^));  end;
  procedure tCPU6502.LDX;  begin X := ptrValue^; end;
  procedure tCPU6502.iLDX; begin X := aimm^; end;
  procedure tCPU6502.LXzx; begin X := azpy^; end;
  procedure tCPU6502.LXax; begin X := aabsy^; end;
  procedure tCPU6502.STX;  begin ptrValue^ := X; end;
  procedure tCPU6502.SXzx; begin azpy^ := X; end;
  procedure tCPU6502.ASL;  begin zn(ASLbase(ptrValue^)); end;
  procedure tCPU6502.LSR;  begin zn(LSRbase(ptrValue^)); end;
  procedure tCPU6502.ROL;  begin zn(ROLbase(ptrValue^)); end;
  procedure tCPU6502.ROR;  begin zn(RORbase(ptrValue^)); end;


  procedure tCPU6502.CPY; begin  cp(Y, ptrValue^); end;
  procedure tCPU6502.CPX; begin cp(X, ptrValue^); end;
  procedure tCPU6502.iCPY; begin cp(Y, aIMM^); end;
  procedure tCPU6502.iCPX; begin cp(X, aIMM^); end;
  procedure tCPU6502.iLDY; begin y := aIMM^; end;

  procedure tCPU6502.STY; begin ptrValue^ := Y; end;
  procedure tCPU6502.LDY; begin y := ptrValue^; end;
  procedure tCPU6502.JPA; begin PC := NextWord; execute; end;
  procedure tCPU6502.JPI; begin PC := PageAdr(NextWord); execute; end;
  procedure tCPU6502.JSR; begin PushAdr(PC+2); JPA; end;
  procedure tCPU6502.RTS; begin PC := Popadr; end;

  procedure tCPU6502.PHa; begin Push(a); end;
  procedure tCPU6502.PLA; begin A := Pop;  end;
  procedure tCPU6502.PHp; begin Push(p or $40); end;
  procedure tCPU6502.PLp; begin p := Pop; end;
  procedure tCPU6502.rti; begin plp; RTS; end;

  procedure tCPU6502.CSF;    {clear set flag}   var opIndex: byte;
  CONST AfLAGS : ARRAY[0..3] of flags = (fc,fi,fv,fd);
  begin opIndex := fopc shr 5;
    SetFlag(AfLAGS[(opIndex) shr 1], odd((opIndex)));
  end;

  procedure tCPU6502.BRK; begin Running := False; end;

  procedure tCPU6502.Rel; CONST AfLAGS : ARRAY[0..3] of flags = (fn,fv,fc,fz);
  var offset: byte; begin  offset := nextbyte;
    if odd(fopc shr 5) <> (AfLAGS[fopc shr 6] in state) then exit;
    PC := pc + fromTwosCom(offset,$80);
  end;

  procedure tCPU6502.BIT;  var b: byte; begin  b := ptrValue^;
    setFlag(fz, (A AND b)=0);  setFlags([fv,fn], b);   end;

  procedure tCPU6502.ExecEmul(newPc: word);
  begin
    Running:=True;
    pc := newPc;
    while (Running) do  begin
      execute; {getOpCode and do housekeeping}
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
    wordRec(result).lo := pop;
    wordRec(result).Hi := pop;
  end;

  procedure tCPU6502.pushAdr(w: word);
  begin
    push(wordRec(w).Hi);
    push(wordRec(w).lo);
  end;

  constructor tCPU6502.Create(StackBase: byte = 1 );
  begin  //  aType = (aIX, aZ, aIM, aA, aIY, aZX, aAY, aAX);
    regInit;
    setStackBase(StackBase);
    setLength(fOpers, 68);
    setLength(fOpInds, 256);
    TablesMaker;
  end;

  Destructor tCPU6502.Done;
  begin
    fopers   := nil;
    fOpInds  := nil;
  end;

  Procedure tCPU6502.TablesMaker;
    var i, cnt: integer;

    procedure expander(ind: integer);
    const step = 4;
    var       j: aType;      rec:  oper;
    begin
      rec := fOpers[ind];
      if rec.a = aOnly then begin
        fopcodes[rec.o] := rec.p;
        fOpInds[rec.o] := ind;
        inc(cnt);
      end else begin
        for j := aIX to aAX do  begin
          if j in rec.a then begin
            fopcodes[rec.o] := rec.p;
            fOpInds[rec.o] := ind;
            inc(cnt);
          end;
          inc(rec.o, step);
        end;
      end;
    end;

  begin
    for i := 0 to high(fOpInds) do fOpInds[i] := $ff;

    fAdressation[aIX] :=  @aINDX;
    fAdressation[aZ]  :=  @aZP;
    fAdressation[aIM] :=  @aIMM;
    fAdressation[aA]  :=  @aABS;
    fAdressation[aIY] :=  @aINDY;
    fAdressation[aZX] :=  @aZPX;
    fAdressation[aAY] :=  @aABSY;
    fAdressation[aAX] :=  @aABSX;

    fOpers[ 0] := DEF(@ ORA  { ORA  }, 'ORA', $01, aAll);
    fOpers[ 1] := DEF(@ ANDm { ANDm }, 'AND', $21, aAll);
    fOpers[ 2] := DEF(@ EOR  { EOR  }, 'EOR', $41, aAll);
    fOpers[ 3] := DEF(@ ADC  { ADC  }, 'ADC', $61, aAll);
    fOpers[ 4] := DEF(@ STA  { STA  }, 'STA', $81, aAll-[aIM]);
    fOpers[ 5] := DEF(@ LDA  { LDA  }, 'LDA', $a1, aAll);
    fOpers[ 6] := DEF(@ CMP  { CMP  }, 'CMP', $c1, aAll);
    fOpers[ 7] := DEF(@ SBC  { SBC  }, 'SBC', $e1, aAll);
    fOpers[ 8] := DEF(@ incm { incm }, 'inc', $e2, aShift);       {page2}
    fOpers[ 9] := DEF(@ decm { decm }, 'dec', $c2, aShift);
    fOpers[10] := DEF(@ iLDX { iLDX }, 'LX#', $a2, aOnly);
    fOpers[11] := DEF(@ LXzx { LXzx }, 'Lzx', $b6, aOnly);
    fOpers[12] := DEF(@ LXax { LXax }, 'Lax', $be, aOnly);
    fOpers[13] := DEF(@ LDX  { LDX  }, 'LDX', $a2, aMem);
    fOpers[14] := DEF(@ SXzx { SXzx }, 'Szx', $96, aOnly);
    fOpers[15] := DEF(@ STX  { STX  }, 'STX', $82, aMem);
    fOpers[16] := DEF(@ ASLa { ASLa }, 'ALa', $0a, aOnly);
    fOpers[17] := DEF(@ ASL  { ASL  }, 'ASL', $02, aShift);
    fOpers[18] := DEF(@ ROLa { ROLa }, 'RLa', $2a, aOnly);
    fOpers[19] := DEF(@ ROL  { ROL  }, 'ROL', $22, aShift);
    fOpers[20] := DEF(@ LSRa { LSRa }, 'LRa', $4a, aOnly);
    fOpers[21] := DEF(@ LSR  { LSR  }, 'LSR', $42, aShift);
    fOpers[22] := DEF(@ RORa { RORa }, 'RRa', $6a, aOnly);
    fOpers[23] := DEF(@ ROR  { ROR  }, 'ROR', $62, aShift);
    fOpers[24] := DEF(@ TXS  { TXS  }, 'TXS', $9a, aOnly);
    fOpers[25] := DEF(@ TSX  { TSX  }, 'TSX', $ba, aOnly);
    fOpers[26] := DEF(@ TXA  { TXA  }, 'TXA', $aa, aOnly);
    fOpers[27] := DEF(@ TAX  { TAX  }, 'TAX', $8a, aOnly);
    fOpers[28] := DEF(@ DEX  { DEX  }, 'DEX', $ca, aOnly);
    fOpers[29] := DEF(@ nop  { nop  }, 'nop', $ea, aOnly);
    fOpers[30] := DEF(@ iCPY { iCPY }, 'CY#', $c0, aOnly);        {page0}
    fOpers[31] := DEF(@ CPY  { CPY  }, 'CPY', $c0, aMem);
    fOpers[32] := DEF(@ iCPX { iCPX }, 'CX#', $e0, aOnly);
    fOpers[33] := DEF(@ CPX  { CPX  }, 'CPX', $e0, aMem);
    fOpers[34] := DEF(@ STY  { STY  }, 'STY', $80, aMem+[aZX]);
    fOpers[35] := DEF(@ iLDY { iLDY }, 'LY#', $a0, aOnly);
    fOpers[36] := DEF(@ LDY  { LDY  }, 'LDY', $a0, aShift);
    fOpers[37] := DEF(@ JPA  { JPA  }, 'JPA', $4c, aOnly);
    fOpers[38] := DEF(@ JPI  { JPI  }, 'JPI', $6c, aOnly);
    fOpers[39] := DEF(@ JSR  { JSR  }, 'JSR', $20, aOnly);
    fOpers[40] := DEF(@ RTS  { RTS  }, 'RTS', $60, aOnly);
    fOpers[41] := DEF(@ rti  { rti  }, 'rti', $40, aOnly);
    fOpers[42] := DEF(@ CSF  { CSF  }, 'CLC', $18, aOnly);
    fOpers[43] := DEF(@ CSF  { CSF  }, 'CLI', $58, aOnly);
    fOpers[44] := DEF(@ CLV  { CLV  }, 'CLV', $b8, aOnly);
    fOpers[45] := DEF(@ CSF  { CSF  }, 'CLD', $d8, aOnly);
    fOpers[46] := DEF(@ CSF  { CSF  }, 'SEC', $38, aOnly);
    fOpers[47] := DEF(@ CSF  { CSF  }, 'SEI', $78, aOnly);
    fOpers[48] := DEF(@ CSF  { CSF  }, 'SED', $f8, aOnly);
    fOpers[49] := DEF(@ BRK  { BRK  }, 'BRK', $00, aOnly);
    fOpers[50] := DEF(@ rel  { rel  }, 'BPL', $10, aOnly);
    fOpers[51] := DEF(@ rel  { rel  }, 'BMI', $30, aOnly);
    fOpers[52] := DEF(@ rel  { rel  }, 'BVC', $50, aOnly);
    fOpers[53] := DEF(@ rel  { rel  }, 'BVC', $70, aOnly);
    fOpers[54] := DEF(@ rel  { rel  }, 'BCC', $90, aOnly);
    fOpers[55] := DEF(@ rel  { rel  }, 'BCS', $b0, aOnly);
    fOpers[56] := DEF(@ rel  { rel  }, 'BNE', $d0, aOnly);
    fOpers[57] := DEF(@ rel  { rel  }, 'BEQ', $f0, aOnly);
    fOpers[58] := DEF(@ BIT  { BIT  }, 'BIT', $20, aMem);
    fOpers[09] := DEF(@ PHp  { PHp  }, 'PHp', $08, aOnly);
    fOpers[60] := DEF(@ PLp  { PLp  }, 'PLp', $28, aOnly);
    fOpers[61] := DEF(@ PHa  { PHa  }, 'PHa', $48, aOnly);
    fOpers[62] := DEF(@ PLA  { PLA  }, 'PLA', $68, aOnly);
    fOpers[63] := DEF(@ INY  { INY  }, 'INY', $c8, aOnly);
    fOpers[64] := DEF(@ DEY  { DEY  }, 'DEY', $88, aOnly);
    fOpers[65] := DEF(@ INX  { INX  }, 'INX', $e8, aOnly);
    fOpers[66] := DEF(@ TAY  { TAY  }, 'TAY', $98, aOnly);
    fOpers[67] := DEF(@ TYA  { TYA  }, 'TYA', $a8, aOnly);
    cnt := 0;
    for i := 0 to high(fOpers) do expander(i);
    writeln(cnt);
  end;

  procedure tCPU6502.ShowTable;
  var i, j, k: integer;
  begin k := 0; writeln;
    for i := 1 to 16 do begin
      for j := 1 to 16 do begin
        if fOpInds[k] <> 255
          then write(fOpers[fOpInds[k]].n)
          else write('---');
        write(' ');
        inc(k);
      end;
      writeln;
    end;
    writeln;

  end;

initialization

  cpu := tCPU6502.Create();
  cpu.ShowTable;

finalization

  cpu.Done;

end.


