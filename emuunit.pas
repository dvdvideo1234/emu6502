unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  adrType = (INDX, ZP_, IMM_, ABS_, INDY, ZPX, ABSY, ABSX);
  pageType = (page0, page1, page2, page3);
  opType = (op0, op1, op2, op3, op4, op5, op6, op7);
  flags = (cf, zf, fi, df, bf, xf, vf, nf);
  flagSet = packed set of flags;
  wordAdressable = array[word] of byte;
  bytep = ^byte;
  str3  = string[3];
  str7  = string[7];

{
function num2hex(nr: byte): str3;
function addr2hex(address: word): str7;
procedure memStoreByte(address: word; Value: byte);
function memReadByte(address: word): byte;
function IndirAdr(adr: word): word;
function NextByte: byte;
function NextWord: word;
function ptrValue: bytep;
}

type
  tByteStack = object
    fsp: byte;
    procedure setSp(b: byte);
    function pop: byte;
    procedure push(b: byte);
    function popAdr: word;
    procedure pushAdr(w: word);
    property sp: byte read fsp write setSp;
  end;

  tRegisters6502 = object
    A: byte;         { Register accumulator }
    X: byte;         { Register index X }
    Y: byte;         { Register index Y }
    PC: word;        { Register programm counter }
    opcode: byte;    { opcode }
    pState: flagSet; { Register processeur }
    adrIndex: adrType;
    opIndex: opType;
    pageIndx: pageType;
    address: word;
    zp: byte;
    fDebugOn: boolean;

    procedure Debug;
    procedure SetFlag(f: flags; state: boolean);
    function  GetFlag(f: flags): boolean;
    function  GetState: byte;
    procedure SetState(state: byte);
    //procedure jumpBranch(offset: byte);
    procedure test(Value: byte);
    procedure doCompare(reg, val: word);
    procedure testSBC(Value: byte);
    procedure testADC(Value: byte);
    function _Rcr(var b: byte; f: boolean): boolean;
    function _Rcl(var b: byte; f: boolean): boolean;
    procedure incr(var Value: byte);
    procedure decr(var Value: byte);
    procedure ExecEmul;
    procedure SetIndex;

    property p: byte read getState write SetState;
    property state: flagSet read pState write pState;
    property DebugOn: boolean read fDebugOn write fDebugOn;
  end;


var
  codeRunning: boolean;
  memory: wordAdressable;
  stack: tByteStack;
  Regs: tRegisters6502;


implementation

{_packb}
  function _unpackb(b: byte): byte;
  var
    tmp: byte;
  begin
    tmp := b shr 4;
    _unpackb := (((tmp shl 2) + tmp) shl 1) + (b and $f);
  end;

  function _packb(w: byte): word;
  begin
    _packb := ((w div 10) shl 4) + (w mod 10);
  end; {_packb}



{Stack}

const
  StackBase = $100;

  procedure tByteStack.setSp(b: byte);
  begin
    fsp := b;
  end;

  function tByteStack.pop: byte;
  begin
    Inc(fsp);
    Result := memory[StackBase + fsp];
  end;

  procedure tByteStack.push(b: byte);
  begin
    memory[StackBase + fsp] := b;
    Dec(fsp);
  end;

  function tByteStack.popAdr: word;
  begin
    Result := pop + pop shl 8;
  end;

  procedure tByteStack.pushAdr(w: word);
  begin
    push(w shr 8);
    push(w and 255);
  end; {Stack}


  {memory}

  procedure memStoreByte(address: word; Value: byte);
  begin
    memory[address] := Value;
  end;

  function memReadByte(address: word): byte;
  begin
    memReadByte := memory[address];
  end;

  function NextByte: byte;
  begin
    with regs do begin
      PC := PC + 1;
      NextByte := memory[PC];
    end;
  end;

  function NextWord: word;
  begin
    NextWord := NextByte + (NextByte shl 8);
  end;

  function IndirAdr(adr: word): word;
  begin
    IndirAdr := memReadByte(adr) + (memReadByte(adr + 1) shl 8);
  end;


{ptrValue}
  function aINDX(): bytep;
  begin                       { ORA INDX }
    result := @memory[IndirAdr(NextByte + regs.X)];
  end;

  function aZP(): bytep;
  begin                       { ORA ZP }
    result := @memory[NextByte];
  end;

  function aIMM(): bytep;
  begin                       { ORA IMM }
    with regs do begin
      PC := PC + 1;
      result:= @memory[PC];
    end;
  end;

  function aABS(): bytep;         { ORA ABS }
  begin
    result := @memory[NextWord];
  end;

  function aINDY(): bytep;
  begin                      { ORA INDY }
    result := @memory[IndirAdr(NextByte)+regs.Y];
  end;

  function aZPX(): bytep;
  begin                      { ORA ZPX }
    result := @memory[(NextByte + regs.X) and $ff];
  end;

  function aABSY(): bytep;
  begin                       { ORA ABSY }
    result := @memory[NextWord + regs.Y];
  end;

  function aABSX(): bytep;
  begin                       { ORA ABSX }
    result := @memory[NextWord + regs.X];
  end;

  function ptrValue: bytep;
  type
    myFuncType= function() : bytep;
    adressing   = array[adrType] of myFuncType;

  const
    funcAry: adressing = (
      @aINDX,  @aIMM, @aABS , @aZP ,
      @aINDY,  @aZPX, @aABSY, @aABSX );

  begin
    result := bytep(funcAry[Regs.adrIndex]);
  end; {ptrValue}


{num2hex}
  function num2hex(nr: byte): str3;
  const
    HexStr: array[0..15] of char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  begin
    num2hex := HexStr[nr shr 4] + HexStr[nr and $F];
  end;

  function addr2hex(address: word): str7;
  begin
    addr2hex := num2hex((address shr 8)) + num2hex(address and $ff);
  end; {num2hex}

{tRegisters6502}
  function tRegisters6502.GetState: byte;
  begin
    Result := bytep(@pState)^;
  end;

  procedure tRegisters6502.SetIndex;
  begin
    opcode := memReadByte(pc);
    pageIndx := pageType(opcode and 3);
    adrIndex := adrType((opcode shr 2) and 7);
    opIndex := opType(opcode shr 5);
  end;

  procedure tRegisters6502.SetState(state: byte);
  begin
    bytep(@pState)^ := state;
  end;

  procedure tRegisters6502.SetFlag(f: flags; state: boolean);
  begin
    if state then
      pState := pState + [f]
    else
      pState := pState - [f];
  end;

  function tRegisters6502.GetFlag(f: flags): boolean;
  begin
    GetFlag := f in pState;
  end;


  procedure tRegisters6502.test(Value: byte);
  begin
    setflag(zf, Value = 0);
    setflag(nf, (Value and $80) <> 0);
  end;

  procedure tRegisters6502.doCompare(reg, val: word);
  begin
    setflag(cf, reg >= val);
    test(reg - val);
  end;

  function tRegisters6502._Rcr(var b: byte; f: boolean): boolean;
  var
    hib: byte;
  begin
    hib := b;
    result := odd(hib);
    hib := hib shr 1;
  if f then hib := hib or $80;
    test(hib);
    b := hib;
  end;

  function tRegisters6502._Rcl(var b: byte; f: boolean): boolean;
  var
    hib: byte;
  begin
    hib := b;
    _Rcl := (hib > $7f);
    hib := hib shl 1;
    if f
      then Inc(hib);
    test(hib);
    b := hib;
  end;

procedure tRegisters6502.testADC(Value: byte);
var
  tmp: word;
begin
  setflag(vf, (A xor Value) and $80 <> 0);
  if Getflag(df) then
  begin
    tmp := _unpackb(a) + _unpackb(Value) + Ord(odd(p));
    setflag(cf, tmp >= 100);
    if cf in pState then
      Dec(tmp, 100);
    tmp := _packb(tmp);
  end
  else
  begin
    tmp := A + Value + Ord(odd(p));
    setflag(cf, tmp >= $100);
    if cf in pState then
      Dec(tmp, $100);
  end;
  A := tmp and $ff;
  test(a);
end;

procedure tRegisters6502.testSBC(Value: byte);
var
  tmp: word;
begin
  setflag(vf, (A xor Value) and $80 <> 0);
  if Getflag(df) then
  begin
    tmp := 99 + _unpackb(a) - _unpackb(Value) + Ord(odd(p));
    setflag(cf, tmp >= 100);
    if odd(p) then
      Dec(tmp, 100);
    tmp := _packb(tmp);
  end
  else
  begin
    tmp := $ff + A + word(-Value) + Ord(odd(p));
    setflag(cf, tmp >= $100);
    if odd(p) then
      Dec(tmp, $100);
  end;
  A := tmp and $ff;
  test(a);
end;

procedure tRegisters6502.incr(var Value: byte);
begin
  inc(value);
  test(value);
end;

procedure tRegisters6502.decr(var Value: byte);
begin
  dec(value);
  test(value);
end;

procedure tRegisters6502.Debug;
begin
  if (DebugOn) then
    WriteLn('PC=', addr2hex(PC),
      ' opc=', num2hex(opcode),
      ' X=', num2hex(X),
      ' Y=', num2hex(Y),
      ' A=', num2hex(A),
      ' P=', num2hex(P));
end;

{1}
procedure ORA;begin with Regs do begin A := A or ptrValue^; test(a); end; end;
procedure EOR;begin with Regs do begin A := A Xor ptrValue^; test(a); end; end;
procedure AND_; begin with Regs do begin  A := A AND ptrValue^; test(a); end;end;
procedure ADC; begin  with Regs do testADC(ptrValue^); end;
procedure SBC; begin  with Regs do testSBC(ptrValue^); end;
procedure CMP; begin  with Regs do doCompare(a, ptrValue^); end;
procedure STA; begin  with Regs do ptrValue^ := a; end;
procedure LDA; begin with Regs do begin  A := ptrValue^; test(a); end;end;

procedure inc_; begin with Regs do incr(ptrValue^); end;

procedure dec_; begin with Regs do incr(ptrValue^); end;

{2} // (0 INDX, 1 ZP_, 2 IMM_, 3 ABS_, 4 INDY, 5 ZPX, 6 ABSY, 7 ABSX)
//  $a2:LDX IMM   $a6:LDX ZP  $ae:LDX ABS  $b6:LDX ZPY  $be:LDX ABSY
procedure LDX; begin
  with Regs do begin
    X := ptrValue^; test(X);
  end;
end;

procedure ASL;
var valPtr: bytep;
begin
  with Regs do begin
    if adrIndex <> IMM_
       then valPtr := ptrValue
       else valPtr := @a;
    setflag(cf, _Rcl(valPtr^, false));
  end;
end;

procedure ROL;
var valPtr: bytep;
begin
  with Regs do begin
    if adrIndex <> IMM_
       then valPtr := ptrValue
       else valPtr := @a;
    setflag(cf, _Rcl(valPtr^, valPtr^ > $7f));
  end;
end;

procedure ROR;
var valPtr: bytep;
begin
  with Regs do begin
    if adrIndex <> IMM_
       then valPtr := ptrValue
       else valPtr := @a;
    setflag(cf, _Rcr(valPtr^, odd(valPtr^)));
  end;
end;

procedure LSR;
var valPtr: bytep;
begin
  with Regs do begin
    if adrIndex <> IMM_
       then valPtr := ptrValue
       else valPtr := @a;
    setflag(cf, _Rcr(valPtr^, false));
  end;
end;

{0}
// $10:BPL $30:BMI $50:BVC $70:BVS $90:BCC $b0:BCS $d0:BNE $f0:BEQ }
procedure Rel;
CONST AfLAGS : ARRAY[0..3] of flags = (nf,vf,cf,zf);
var offset: byte;
begin
  offset := nextbyte;
  with regs do begin
    if odd(byte(opIndex)) <> (AfLAGS[byte(opIndex) shr 1] in state)
      then exit;
    inc(PC, offset);
    if offset > $7f then dec(PC, $100);
  end;
end;

// $24: { BIT ZP }  $2c: { BIT ABS }
procedure BIT;
var b: byte;
begin
  with Regs do begin
    b := ptrValue^;
    setFlag(zf, (A AND b)=0);
    P := (P and $3f) or (b and $c0);
  end;
end;

// $a0:LDY IMM  $a4:LDY ZP  $ac:LDY ABS  $b4:LDY ZPX  $bc:LDY ABSX
procedure LDY; begin
  with Regs do begin
    y := ptrValue^;
    test(Y);
  end;
end;

// $c0:CPY IMM  $c4:CPY ZP  $cc:CPY ABS
procedure CPY; begin  with Regs do doCompare(Y, ptrValue^); end;

// $e0:CPX IMM  $e4:CPX ZP  $e0:CPX IMM
procedure CPX; begin  with Regs do doCompare(X, ptrValue^); end;

procedure JPA; begin regs.PC := NextWord-1; end;
procedure JPI; begin regs.PC := IndirAdr(NextWord)-1; end;
procedure JSR; begin stack.PushAdr(regs.PC+2); JPA; end;
procedure PHP; begin stack.Push(regs.P); end;
procedure PLP; begin regs.P := stack.Pop or $20; end;
procedure CLC; begin regs.setflag(cf, false); end;
procedure SEC; begin regs.setflag(cf, true); end;
procedure PHA; begin stack.Push(regs.a); end;
$58: { CLI }     setFlag(fi, false);




type
  opArray = array [0..2] of procedure;

const
  ops: opArray = (@ADC, @ORA, @AND_);


procedure tRegisters6502.ExecEmul;
begin
  if not (codeRunning) then
    Exit;
  SetIndex; {getOpCode}
  Debug;

  case opcode of
    $00: { BRK impl } codeRunning := False;
    $01: { ORA INDX } ORA;
    $05: { ORA ZP }   ORA;
    $06: { ASL ZP }   ASL;
    $08: { PHP }      PHP;
    $09: { ORA IMM }  ORA;
    $0a: { ASL IMPL } ASL;
    $0d: { ORA ABS }  ORA;
    $0e: { ASL ABS }  ASL;
    $10: { BPL }      Rel;
    $11: { ORA INDY } ORA;
    $15: { ORA ZPX }  ORA;
    $16: { ASL ZPX }  ASL;
    $18: { CLC impl } CLC;
    $19: { ORA ABSY } ORA;
    $1d: { ORA ABSX } ORA;
    $1e: { ASL ABSX } ASL;
    $20: { JSR ABS  } JSR;
    $21: { AND INDX } AND_;
    $24: { BIT ZP }   bit;
    $25: { AND ZP }  AND_;
    $26: { ROL ZP }  ROL;
    $28: { PLP }     PLP;
    $29: { AND IMM } AND_;
    $2a: { ROL A }   ROL;
    $2c: { BIT ABS } bit;
    $2d: { AND ABS } AND_;
    $2e: { ROL ABS } ROL;
    $30: { BMI }     Rel;
    $31: { AND INDY }AND_;
    $35: { AND INDX }AND_;
    $36: { ROL ZPX } ROL;
    $38: { SEC }     SEC;
    $39: { AND ABSY }AND_;
    $3d: { AND ABSX }AND_ ;
    $3e: { ROL ABSX }ROL;
    $40: { RTI }     ;
    $41: { EOR INDX }EOR;
    $45: { EOR ZPX } EOR;
    $46: { LSR ZP }  LSR;
    $48: { PHA }     pha;
    $49: { EOR IMM } EOR;
    $4a: { LSR }     LSR;
    $4c: { JMP abs } jpa;
    $4d: { EOR abs } EOR;
    $4e: { LSR abs } LSR;
    $50: { BVC  }    Rel;
    $51: { EOR INDY }EOR;
    $55: { EOR ZPX } EOR;
    $56: { LSR ZPX } LSR;
    $58: { CLI }     setFlag(fi, false);
    $59: { EOR ABSY }EOR;
    $5d: { EOR ABSX }EOR;
    $5e: { LSR ABSX }LSR;
    $60: { RTS }     PC := stack.Popadr;
    $61: { ADC INDX }ADC;
    $65: { ADC ZP }  ADC;
    $66: { ROR ZP }  ROR;
    $68: { PLA }     begin A := stack.Pop; test(a); end;
    $69: { ADC IMM } ADC;
    $6a: { ROR A }   ROR;
    $6c: { JMP INDIR } jpi;
    $6d: { ADC ABS } ADC;
    $6e: { ROR ABS } ROR;
    $70: { BVS }     Rel;
    $71: { ADC INY } ADC;
    $75: { ADC ZPX } ADC;
    $76: { ROR ZPX } ROR;
    $78: { SEI }     setflag(fi, true);
    $79: { ADC ABSY }ADC;
    $7d: { ADC ABSX }ADC;
    $7e: { ROR ABSX }ROR;
    $81: { STA INDX } sta;
    $84: { STY ZP } memStoreByte(NextByte, Y);
    $85: { STA ZP } sta;
    $86: { STX ZP } memStoreByte(NextByte, X);
    $88: { DEY }    decr(y);
    $8a: { TXA }       begin A := X; test(A); end;
    $8c: { STY abs }   memStoreByte(NextWord, Y);
    $8d: { STA ABS } sta;
    $8e: { STX abs }   memStoreByte(NextWord, X);
    $90: { BCC }      Rel;
    $91: { STA INDY } STA;
    $94: { STY ZPX } memStoreByte(NextByte + X, Y);  //?
    $95: { STA ZPX } sta;
    $96: { STX ZPY } memStoreByte(NextByte + Y, X);  //?
    $98: { TYA }     begin A := y; test(A); end;
    $99: { STA ABSY } STA;
    $9a: { TXS }      stack.SP := X;
    $9d: { STA ABSX } STA;
    $a0: { LDY IMM }  LDY;
    $a1: { LDA INDX } LDA;
    $a2: { LDX IMM }  LDX;
    $a4: { LDY ZP }  LDY;
    $a5: { LDA ZP } LDA;
    $a6: { LDX ZP } LDX;
    $a8: { TAY }    begin Y := A; test(Y); end;
    $a9: { LDA IMM } LDA;
    $aa: { TAX }    begin X := A; test(X); end;
    $ac: { LDY ABS } LDY;
    $ad: { LDA ABS } LDA;
    $ae: { LDX ABS } LDX;
    $b0: { BCS }  Rel;
    $b1: { LDA INDY } LDA;
    $b4: { LDY ZPX } LDY;
    $b5: { LDA ZPX } LDA;
    $b6: { LDX ZPY } LDX;
    $b8: { CLV }      P := P and $bf;
    $b9: { LDA ABSY } LDA;
    $ba: { TSX }      X := stack.SP;
    $bc: { LDY ABSX } LDY;
    $bd: { LDA ABSX } lda;
    $be: { LDX ABSY } LDX;
    $c0: { CPY IMM }  CPY;
    $c1: { CMP INDY } CMP;
    $c4: { CPY ZP }   CPY;
    $c5: { CMP ZP }   CMP;
    $c6: { DEC ZP }   DEC_;
    $c8: { INY }      incr(Y);
    $c9: { CMP IMM }  CMP;
    $ca: { DEX }      decr(X);
    $cc: { CPY ABS }  CPY;
    $cd: { CMP ABS }  CMP;
    $ce: { DEC ABS }  dec_;
    $d0: { BNE }     Rel;
    $d1: { CMP INDY }  CMP;
    $d5: { CMP ZPX }   CMP;
    $d6: { DEC ZPX }   dec_;
    $d8: { CLD }       P := P and $f7;
    $d9: { CMP ABSY }  CMP;
    $dd: { CMP ABSX }   CMP;
    $de: { DEC ABSX }  dec_;
    $e0: { CPX IMM }   CPX;
    $e1: { SBC INDX }     SBC;
    $e4: { CPX ZP }    CPX;
    $e5: { SBC ZP }       SBC;
    $e6: { INC ZP }    inc_;
    $e8: { INX }       INCR(X);
    $e9: { SBC IMM }   SBC;
    $ea: { NOP }          ;
    $ec: { CPX ABS }   CPX;
    $ed: { SBC ABS }   SBC;
    $ee: { INC ABS }   inc_;
    $f0: { BEQ }       Rel;
    $f1: { SBC INDY } SBC;
    $f5: { SBC ZPX }  SBC;
    $f6: { INC ZPX }  inc_;
    $f8: { SED }      P := P or 8;
    $f9: { SBC ABSY } SBC;
    $fd: { SBC ABSX } SBC;
    $fe: { INC ABSX } inc_;
    else
    begin
      WriteLn('Adresse $', addr2hex(PC), ' - code inconnu ', opcode);
      codeRunning := False;
    end;
  end;

  if (PC = 0) or (not codeRunning) then
  begin
    WriteLn('Programme termine a PC=$', addr2hex(PC - 1));
    codeRunning := False;
  end;
end;

end.



