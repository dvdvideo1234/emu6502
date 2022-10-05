unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  adrType = (INDX, ZP_, IMM_, ABS_, INDY, ZPX, ABSY, ABSX,
          zpy, RegA, Reg_SP, RegP);
  pageType = (page0, page1, page2, page3);
  ProcType = procedure ;
  opType = (op0, op1, op2, op3, op4, op5, op6, op7);
  flags = (cf, zf, fi, df, bf, xf, vf, nf);
  flagSet = packed set of flags;
  wordAdressable = array[word] of byte;
  bytep = ^byte;
  FuncType= function() : bytep;
  str3  = string[3];
  str7  = string[7];

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
    AReg: byte;         { Register accumulator }
    XReg: byte;         { Register index X }
    YReg: byte;         { Register index Y }
    PC: word;        { Register programm counter }
    opcode: byte;    { opcode }
    pState: flagSet; { Register processeur }
    adrIndex: adrType;
    opIndex: opType;
    pageIndx: pageType;
    fDebugOn: boolean;
    codeRunning: boolean;

    Procedure InitEmul;
    procedure Debug;
    procedure SetFlag(f: flags; state: boolean);
    function  GetFlag(f: flags): boolean;
    function  GetState: byte;
    procedure SetState(state: byte);
    procedure test(Value: byte);
    procedure doCompare(reg, val: word);
    procedure testSBC(Value: byte);
    procedure testADC(Value: byte);
    procedure Rcr(var b: byte; f: boolean);
    procedure Rcl(var b: byte; f: boolean);
    procedure incr(var Value: byte);
    procedure decr(var Value: byte);
    procedure ExecEmul(newPc: word);
    procedure SetIndex;
    procedure Setareg(Value: byte);
    procedure SetXreg(Value: byte);
    procedure SetYreg(Value: byte);

    property p: byte read getState write SetState;
    property a: byte read areg write Setareg;
    property x: byte read xreg write SetXreg;
    property y: byte read yreg write SetYreg;
    property state: flagSet read pState write pState;
    property DebugOn: boolean read fDebugOn write fDebugOn;
  end;


var
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


{ptrValue of addressable memory }

  procedure nop; begin end;

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

  function aZPY(): bytep;
  begin                      { STX ZPY }
    result := @memory[(NextByte + regs.Y) and $ff];
  end;

  function aRegA(): bytep;
  begin                      { asl }
    result := @regs.A;
  end;

  function aRegS(): bytep;
  begin                      { TSX }
    result := @stack.sp;
  end;

  function aRegP(): bytep;
  begin                      { PLP }
    result := @regs.pstate;
  end;

  function ptrValue: bytep;
  type
    adressing   = array[adrType] of FuncType;

  const
    funcAry: adressing = (
      @aINDX,  @aIMM, @aABS , @aZP ,
      @aINDY,  @aZPX, @aABSY, @aABSX, @AZPY, @aRegA, @aRegS, @aRegP);
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

  procedure tRegisters6502.SetState(state: byte);
  begin
    bytep(@pState)^ := state;
  end;

  procedure tRegisters6502.Setareg(Value: byte);
  begin
    areg := Value;
    test(Value);
  end;

  procedure tRegisters6502.SetXreg(Value: byte);
  begin
    Xreg := Value;
    test(Value);
  end;

  procedure tRegisters6502.SetYreg(Value: byte);
  begin
    Yreg := Value;
    test(Value);
  end;

  Procedure tRegisters6502.InitEmul;
    Begin
      A :=0;
      X :=0;
      Y :=0;
      P :=0;
      stack.SP:=0;
    End;

  procedure tRegisters6502.SetIndex;
  const  //INDX, ZP_, IMM_, ABS_, INDY, ZPX, ABSY,   ABSX
    reind: array[INDX..ABSX] of adrType = (
           IMM_, ZP_, RegA, ABS_, INDY, zpy, Reg_SP, ABSY);
  begin
    opcode := memReadByte(pc);
    pageIndx := pageType(opcode and 3);
    adrIndex := adrType((opcode shr 2) and 7);
    opIndex := opType(opcode shr 5);
    if pred(pageIndx) = page0
       then exit;

    if adrIndex in [INDX, IMM_]
       then adrIndex := reind[adrIndex];

    IF (pageIndx = page2) then begin
      if (opIndex  in [op4,op5]) and (adrIndex in [ZPX..ABSX])
         then  adrIndex := reind[adrIndex];
      exit;
    end;

    if opcode in [$08,$28]
       then adrIndex := regp
       else if opcode = $98
               then adrIndex := regA
       ;

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

  procedure tRegisters6502.Rcr(var b: byte; f: boolean);
  var
    hib: byte;
  begin
    hib := b;
    SetFlag(cf, odd(hib));
    hib := hib shr 1;
  if f then hib := hib or $80;
    test(hib);
    b := hib;
  end;

  procedure tRegisters6502.Rcl(var b: byte; f: boolean);
  var
    hib: byte;
  begin
    hib := b;
    SetFlag(cf, (hib > $7f));
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


  {    0    1    2    3    4    5    6    7
     1 INDX ZP   IMM  ABS  INDY ZPX  ABSY ABSX
       0    4    8    0c   10   14   18   1c
  0 01 ORA  ORA  ORA  ORA  ORA  ORA  ORA  ORA
  1 21 AND  AND  AND  AND  AND  AND  AND  AND
  2 41 EOR  EOR  EOR  EOR  EOR  EOR  EOR  EOR
  3 61 ADC  ADC  ADC  ADC  ADC  ADC  ADC  ADC
  4 81 STA  STA  L__  STA  STA  STA  STA  STA
  5 a1 LDA  LDA  LDA  LDA  LDA  LDA  LDA  LDA
  6 c1 CMP  CMP  CMP  CMP  CMP  CMP  CMP  CMP
  7 e1 SBC  SBC  SBC  SBC  SBC  SBC  SBC  SBC  63 }


  procedure ORA; begin with Regs do  A := A or ptrValue^; end;
  procedure AND_;begin with Regs do  A := A AND ptrValue^; ;end;
  procedure EOR; begin with Regs do  A := A Xor ptrValue^;  end;
  procedure ADC; begin with Regs do testADC(ptrValue^); end;
  procedure STA; begin with Regs do ptrValue^ := a; end;
  procedure LDA; begin with Regs do A := ptrValue^; end;
  procedure CMP;
  var b: byte;
  begin
    b :=  ptrValue^;
    with Regs do doCompare(a, b);
  end;
  procedure SBC; begin with Regs do testSBC(ptrValue^); end;

  {    0     1    2    3    4    5        6    7
     2 INDX  ZP   IMM  ABS  INDY ZPX      ABSY ABSX
       0     4    8    0c   10   14       18   1c
  0 02 L__   ASL  ASL  ASL  L__  ASL ZPX  L__  ASL
  1 22 L__   ROL  ROL  ROL  L__  ROL ZPX  L__  ROL
  2 42 L__   LSR  LSR  LSR  L__  LSR ZPX  L__  LSR
  3 62 L__   ROR  ROR  ROR  L__  ROR ZPX  L__  ROR
  4 82 L__   STX  TXA  STX  L__  STX ZPY  TXS  L__
  5 a2 LDX#  LDX  TAX  LDX  L__  LDX ZPY  TSX  LDX
  6 c2 L__   DEC  DEX  DEC  L__  DEC ZPX  L__  DEC
  7 e2 L__   INC  NOP  INC  L__  INC ZPX  L__  INC  42 }

  procedure inc_; begin with Regs do incr(ptrValue^); end;
  procedure dec_; var valPtr: bytep; begin valPtr := ptrValue; with Regs do begin
    if adrIndex = Rega then valPtr := @X;  decr(valPtr^);  end; end;
  procedure LDX; begin with Regs do begin X := ptrValue^; test(X); end; end;
  procedure STX; begin with Regs do ptrValue^ := X; end;
  procedure ASL;begin with Regs do Rcl(ptrValue^, false); end;
  procedure LSR;begin with Regs do Rcr(ptrValue^, false); end;
  procedure ROL; var valPtr: bytep; begin  valPtr := ptrValue;
      with Regs do Rcl(valPtr^, valPtr^ > $7f);  end;
  procedure ROR; var valPtr: bytep; begin  valPtr := ptrValue;
      with Regs do Rcr(valPtr^, odd(valPtr^));  end;


  {     0    1    2    3    4    5    6      7
     0  INDX ZP   IMM  ABS  INDY ZPX  ABSY   ABSX
        00   04   08   0c   10   14   18     1c
  0 00  BRK  L__  PHP  L__  BPL  L__  CLC    L__  CZDBI_VN
  1 20  JSRA BIT  PLP  BIT  BMI  L__  SEC    L__
  2 40  RTI  L__  PHA  JMP  BVC  L__  CLI    L__
  3 60  RTS  L__  PLA  JMP  BVS  L__  SEI    L__
  4 80  L__  STY  DEY  STY  BCC  STY  TYA    L__
  5 a0  LDY# LDY  TAY  LDY  BCS  LDY  CLV    L__
  6 c0  CPY# CPY  INY  CPY  BNE  L__  CLD    L__
  7 e0  CPX# CPX  INX  CPX  BEQ  L__  SED    L__ 45 }

  procedure CPY; begin with Regs do doCompare(Y, ptrValue^); end;
  procedure CPX; begin with Regs do doCompare(X, ptrValue^); end;
  procedure STY; begin with Regs do ptrValue^ := Y; end;
  procedure LDY; begin with Regs do begin y := ptrValue^; test(Y); end; end;
  procedure JPA; begin regs.PC := NextWord-1; end;
  procedure JPI; begin regs.PC := IndirAdr(NextWord)-1; end;
  procedure JSR; begin stack.PushAdr(regs.PC+2); JPA; end;
  procedure PH; begin stack.Push(ptrValue^); end;
  procedure PL; begin ptrValue^ := stack.Pop; end;
  procedure RTS; begin regs.PC := stack.Popadr; end;
  procedure PLA; begin  with regs do  begin A := stack.Pop; test(a); end; end;
  procedure CLV; begin regs.setflag(Vf, false); end;
  procedure rti; begin RTS; regs.P := stack.Pop; end;
  procedure CSF; {clear set flag} CONST AfLAGS : ARRAY[0..3] of flags = (cf,fi,vf,df);
  begin with regs do begin SetFlag(AfLAGS[byte(opIndex) shr 1], odd(byte(opIndex)));
    end; end;
  procedure iny; begin with regs do y := succ(Y); end;
  procedure inx; begin with regs do X := succ(X); end;
  procedure dey; begin with regs do y := pred(y); end;
  procedure BRK; begin codeRunning := False; end;
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

  procedure BIT;
  var b: byte;
  begin
    with Regs do begin
      b := ptrValue^;
      setFlag(zf, (A AND b)=0);
      P := (P and $3f) or (b and $c0);
    end;
  end;

  procedure L__;
  begin

  end;

  procedure doPage0;
  {     0    1    2    3    4    5    6      7
     0  INDX ZP   IMM  ABS  INDY ZPX  ABSY   ABSX
        00   04   08   0c   10   14   18     1c   CZDBI_VN  }
  const
    ops: array[0..63] of procedure = (
     @BRK  ,@L__  ,@PH   ,@L__  ,@Rel  ,@L__  ,@CSF    ,@L__
    ,@JSR  ,@BIT  ,@PL   ,@BIT  ,@Rel  ,@L__  ,@CSF    ,@L__
    ,@RTI  ,@L__  ,@PH   ,@jpa  ,@Rel  ,@L__  ,@CSF    ,@L__
    ,@RTS  ,@L__  ,@PLA  ,@jpi  ,@Rel  ,@L__  ,@CSF    ,@L__
    ,@L__  ,@STY  ,@DEY  ,@STY  ,@Rel  ,@STY  ,@STY    ,@L__
    ,@LDY  ,@LDY  ,@LDY  ,@LDY  ,@Rel  ,@LDY  ,@CLV    ,@L__
    ,@CPY  ,@CPY  ,@INY  ,@CPY  ,@Rel  ,@L__  ,@CSF    ,@L__
    ,@CPX  ,@CPX  ,@INX  ,@CPX  ,@Rel  ,@L__  ,@CSF    ,@L__ );

  begin
    ops[regs.opcode shr 2];
  end;


  procedure doPage1;
  {    0    1    2    3    4    5    6    7
     1 INDX ZP   IMM  ABS  INDY ZPX  ABSY ABSX
       0    4    8    0c   10   14   18   1c      }
  const
    ops: array[0..63] of procedure = (

    @ORA  ,@ORA  ,@ORA  ,@ORA  ,@ORA  ,@ORA  ,@ORA  ,@ORA
   ,@and_ ,@and_ ,@and_ ,@and_ ,@and_ ,@and_ ,@and_ ,@AND_
   ,@EOR  ,@EOR  ,@EOR  ,@EOR  ,@EOR  ,@EOR  ,@EOR  ,@EOR
   ,@ADC  ,@ADC  ,@ADC  ,@ADC  ,@ADC  ,@ADC  ,@ADC  ,@ADC
   ,@STA  ,@STA  ,@L__  ,@STA  ,@STA  ,@STA  ,@STA  ,@STA
   ,@LDA  ,@LDA  ,@LDA  ,@LDA  ,@LDA  ,@LDA  ,@LDA  ,@LDA
   ,@CMP  ,@CMP  ,@CMP  ,@CMP  ,@CMP  ,@CMP  ,@CMP  ,@CMP
   ,@SBC  ,@SBC  ,@SBC  ,@SBC  ,@SBC  ,@SBC  ,@SBC  ,@SBC
     );
    begin
      ops[regs.opcode shr 2];
    end;

  procedure doPage2;
  {    0     1    2    3    4    5        6    7
     2 INDX  ZP   IMM  ABS  INDY ZPX      ABSY ABSX
       0     4    8    0c   10   14       18   1c     }
  const
    ops: array[0..63] of procedure = (

    @L__   ,@ASL  ,@ASL  ,@ASL  ,@L__  ,@ASL  ,@L__  ,@ASL
   ,@L__   ,@ROL  ,@ROL  ,@ROL  ,@L__  ,@ROL  ,@L__  ,@ROL
   ,@L__   ,@LSR  ,@LSR  ,@LSR  ,@L__  ,@LSR  ,@L__  ,@LSR
   ,@L__   ,@ROR  ,@ROR  ,@ROR  ,@L__  ,@ROR  ,@L__  ,@ROR
   ,@L__   ,@STX  ,@STX  ,@STX  ,@L__  ,@STX  ,@STX  ,@L__
   ,@LDX   ,@LDX  ,@LDX  ,@LDX  ,@L__  ,@LDX  ,@LDX  ,@LDX
   ,@L__   ,@DEC_ ,@DEC_ ,@DEC_ ,@L__  ,@DEC_ ,@L__  ,@DEC_
   ,@L__   ,@INC_ ,@NOP  ,@INC_ ,@L__  ,@INC_ ,@L__  ,@INC_ );
    begin
      ops[regs.opcode shr 2];
    end;

  procedure tRegisters6502.ExecEmul(newPc: word);
  const
    ops: array[0..3] of procedure = (
        @doPage0 ,@doPage1 ,@doPage2, @L__  );
  begin
    codeRunning:=True;
    pc := newPc;
    while (codeRunning) do  begin
      SetIndex; {getOpCode}
      Debug;
      ops[ord(pageIndx)];
      inc(pc);
    end;
  end;

end.


  case opcode of      {45}
    $00: { BRK }      BRK;
    $08: { PHP }      PH;
    $10: { BPL }      Rel;
    $18: { CLC }      CSF;
    $20: { JSR ABS}   JSR;
    $24: { BIT ZP }   bit;
    $28: { PLP }      PL;
    $2c: { BIT ABS }  bit;
    $30: { BMI }      Rel;
    $38: { SEC }      CSF;
    $40: { RTI }      RTI;
    $48: { PHA }      ph;
    $4c: { JMP abs }  jpa;
    $50: { BVS }      Rel;
    $58: { CLI }      CSF;
    $60: { RTS }      RTS;
    $68: { PLA }      PLA;
    $6c: { JMP IND }  jpi;
    $70: { BVS }      Rel;
    $78: { SEI }      CSF;
    $84: { STY ZP }   STY;
    $88: { DEY }      DEY;
    $8c: { STY abs }  STY;
    $90: { BCC }      Rel;
    $94: { STY ZPX }  STY;
    $98: { TYA }      STY;
    $a0: { LDY IMM }  LDY;
    $a4: { LDY ZP }   LDY;
    $ac: { LDY ABS }  LDY;
    $a8: { TAY }      ldy;
    $b0: { BCS }      Rel;
    $b4: { LDY ZPX }  LDY;
    $b8: { CLV }      clv;
    $bc: { LDY ABSX } LDY;
    $c0: { CPY IMM }  CPY;
    $c4: { CPY ZP }   CPY;
    $c8: { INY }      INY;
    $cc: { CPY ABS }  CPY;
    $d0: { BNE }      Rel;
    $d8: { CLD }      CSF;
    $e0: { CPX IMM }  CPX;
    $e4: { CPX ZP }   CPX;
    $e8: { INX }      INX;
    $ec: { CPX ABS }  CPX;
    $f0: { BEQ }      Rel;
    $f8: { SED }      CSF;

    $01: { ORA INDX } ORA;     {63}
    $05: { ORA ZP }   ORA;
    $09: { ORA IMM }  ORA;
    $0d: { ORA ABS }  ORA;
    $11: { ORA INDY } ORA;
    $15: { ORA ZPX }  ORA;
    $19: { ORA ABSY } ORA;
    $1d: { ORA ABSX } ORA;
    $21: { AND INDX } AND_;
    $25: { AND ZP }   AND_;
    $29: { AND IMM }  AND_;
    $2d: { AND ABS }  AND_;
    $31: { AND INDY } AND_;
    $35: { AND INDX } AND_;
    $39: { AND ABSY } AND_;
    $3d: { AND ABSX } AND_ ;
    $41: { EOR INDX } EOR;
    $45: { EOR ZPX }  EOR;
    $49: { EOR IMM }  EOR;
    $4d: { EOR abs }  EOR;
    $51: { EOR INDY } EOR;
    $55: { EOR ZPX }  EOR;
    $59: { EOR ABSY } EOR;
    $5d: { EOR ABSX } EOR;
    $61: { ADC INDX } ADC;
    $65: { ADC ZP }   ADC;
    $69: { ADC IMM }  ADC;
    $6d: { ADC ABS }  ADC;
    $71: { ADC INY }  ADC;
    $75: { ADC ZPX }  ADC;
    $79: { ADC ABSY } ADC;
    $7d: { ADC ABSX } ADC;
    $81: { STA INDX } sta;
    $85: { STA ZP }   sta;
    $8d: { STA ABS }  sta;
    $91: { STA INDY } STA;
    $95: { STA ZPX }  sta;
    $99: { STA ABSY } STA;
    $9d: { STA ABSX } STA;
    $a1: { LDA INDX } LDA;
    $a5: { LDA ZP }   LDA;
    $a9: { LDA IMM }  LDA;
    $ad: { LDA ABS }  LDA;
    $b1: { LDA INDY } LDA;
    $b5: { LDA ZPX }  LDA;
    $b9: { LDA ABSY } LDA;
    $bd: { LDA ABSX } lda;
    $c1: { CMP INDX } CMP;
    $c5: { CMP ZP }   CMP;
    $c9: { CMP IMM }  CMP;
    $cd: { CMP ABS }  CMP;
    $d1: { CMP INDY } CMP;
    $d5: { CMP ZPX }  CMP;
    $d9: { CMP ABSY } CMP;
    $dd: { CMP ABSX } CMP;
    $e1: { SBC INDX } SBC;
    $e5: { SBC ZP }   SBC;
    $e9: { SBC IMM }  SBC;
    $ed: { SBC ABS }  SBC;
    $f1: { SBC INDY } SBC;
    $f5: { SBC ZPX }  SBC;
    $f9: { SBC ABSY } SBC;
    $fd: { SBC ABSX } SBC;

    $06: { ASL ZP  }  ASL;  {42}
    $0a: { ASL a   }  ASL;
    $0e: { ASL ABS }  ASL;
    $16: { ASL ZPX }  ASL;
    $1e: { ASL ABSX } ASL;
    $26: { ROL ZP  }  ROL;
    $2a: { ROL A   }  ROL;
    $2e: { ROL ABS }  ROL;
    $36: { ROL ZPX }  ROL;
    $3e: { ROL ABSX } ROL;
    $46: { LSR ZP  }  LSR;
    $4a: { LSR a}     LSR;
    $4e: { LSR abs }  LSR;
    $56: { LSR ZPX }  LSR;
    $5e: { LSR ABSX } LSR;
    $66: { ROR ZP  }  ROR;
    $6a: { ROR A   }  ROR;
    $6e: { ROR ABS }  ROR;
    $76: { ROR ZPX }  ROR;
    $7e: { ROR ABSX } ROR;
    $86: { STX ZP  }  STX;
    $8a: { TXA     }  STX;
    $8e: { STX abs }  STX;
    $96: { STX ZPY }  STX;
    $9a: { TXS      } STX;
    $a2: { LDX IMM }  LDX;
    $a6: { LDX ZP  }  LDX;
    $aa: { TAX     }  LDX;
    $ae: { LDX ABS }  LDX;
    $b6: { LDX ZPY }  LDX;
    $ba: { TSX     }  LDX;
    $be: { LDX ABSY } LDX;
    $c6: { DEC ZP   } DEC_;
    $ca: { DEX     }  dec_;
    $ce: { DEC ABS }  dec_;
    $d6: { DEC ZPX }  dec_;
    $de: { DEC ABSX } dec_;
    $e6: { INC ZP  }  inc_;
    $ea: { NOP     }  nop;
    $ee: { INC ABS }  inc_;
    $f6: { INC ZPX }  inc_;
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



