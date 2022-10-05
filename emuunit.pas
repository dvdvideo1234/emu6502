unit EmuUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type
  tRegisters6502 = object
  private
    AReg: byte;         { Register accumulator }
    XReg: byte;         { Register index X }
    YReg: byte;         { Register index Y }
    PC: word;           { Register programm counter }
    opcode: byte;       { opcode }
    pState: flagSet;    { Register processor }
    adrIndex: adrType;
    opIndex: opType;
    pageIndx: pageType;
    fDebugOn: boolean;
    codeRunning: boolean;

  protected
    procedure SetFlags(f: flagset; NewStates: byte);
    procedure SetFlag(f: flags; state: boolean);
    function  GetFlag(f: flags): boolean;
    function  GetState: byte;
    procedure SetState(state: byte);
    procedure Setareg(Value: byte);
    procedure SetXreg(Value: byte);
    procedure SetYreg(Value: byte);
  public
    Procedure InitEmul;
    procedure Debug;
    function  NextByte: byte;
    function  NextWord: word;
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

    property p: byte read getState write SetState;
    property a: byte read areg write Setareg;
    property x: byte read xreg write SetXreg;
    property y: byte read yreg write SetYreg;
    property state: flagSet read pState write pState;
    property DebugOn: boolean read fDebugOn write fDebugOn;
  end;


var
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



{ptrValue of addressing memory }

  procedure nop; begin end;

  function aINDX(): bytep;
  begin                       { ORA INDX }
    result := @mem.bytes[byte(mem.PageAdr(regs.NextByte + regs.X))];
  end;

  function aZP(): bytep;
  begin                       { ORA ZP }
    result := @mem.bytes[regs.NextByte];
  end;

  function aIMM(): bytep;
  begin                       { ORA IMM }
    with regs do begin
      PC := PC + 1;
      result:= @mem.bytes[PC];
    end;
  end;

  function aABS(): bytep;         { ORA ABS }
  begin
    result := @mem.bytes[regs.NextWord];
  end;

  function aINDY(): bytep;
  begin                      { ORA INDY }
    result := @mem.bytes[mem.PageAdr(regs.NextByte)+regs.Y];
  end;

  function aZPX(): bytep;
  begin                      { ORA ZPX }
    result := @mem.bytes[(regs.NextByte + regs.X) and $ff];
  end;

  function aABSY(): bytep;
  begin                       { ORA ABSY }
    result := @mem.bytes[regs.NextWord + regs.Y];
  end;

  function aABSX(): bytep;
  begin                       { ORA ABSX }
    result := @mem.bytes[regs.NextWord + regs.X];
  end;

  function aZPY(): bytep;
  begin                      { STX ZPY }
    result := @mem.bytes[(regs.NextByte + regs.Y) and $ff];
  end;

  function aRegA(): bytep;
  begin                      { asl }
    result := @regs.A;
  end;

  function aRegS(): bytep;
  begin                      { TSX }
    result := stack.sadr;
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



{tRegisters6502}

  function tRegisters6502.GetState: byte;
  begin
    Result := bytep(@pState)^;
  end;

  function tRegisters6502.NextByte: byte;
  begin
      PC := PC + 1;
      result := mem.bytes[PC];
  end;

  function tRegisters6502.NextWord: word;
  begin
    wordRec(result).lo := NextByte;
    wordRec(result).hi := NextByte;
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
    opcode := mem.bytes[pc];
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

  procedure tRegisters6502.SetFlags(f: flagset; NewStates: byte);
  begin
    newStates := bytep(@f)^ and NewStates;
    pstate := pstate - f;
    p := p or  NewStates)
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
    if not (DebugOn) then exit;

      WriteLn('PC=', word2hex(PC),
        ' opc=', byte2hex(opcode),
        ' X=', byte2hex(X),
        ' Y=', byte2hex(Y),
        ' A=', byte2hex(A),
        ' P=', byte2bin(P));
      readln;
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
  procedure JPA; begin regs.PC := regs.NextWord-1; end;
  procedure JPI; begin regs.PC := mem.PageAdr(regs.NextWord)-1; end;
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
  procedure BRK; begin regs.codeRunning := False; end;
  procedure Rel;
  CONST AfLAGS : ARRAY[0..3] of flags = (nf,vf,cf,zf);
  var offset: byte;
  begin
    offset := regs.nextbyte;
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


