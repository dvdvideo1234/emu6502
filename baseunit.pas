unit BaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  bytep = ^byte;
  str3   = string[3];
  str7   = string[7];
  str15  = string[15];
  byteAry  = array of byte;
  flags = (fc, fz, fi, fd, fb, fx, fv, fn);
  flagSet = packed set of flags;
  wordAdressable = array[word] of byte;

  tRegisters = class    { An object to hold the CPU registers}
    YReg: byte;         { Register index Y }
    AReg: byte;         { Register accumulator }
    XReg: byte;         { Register index X }
    pReg: flagSet;      { Register processor }
    fPC: word;          { Register programm counter }
    fsp: word;          { Register Stack pointer }

    procedure SetFlags(f: flagset; NewStates: byte);
    procedure SetFlag(f: flags; stat: boolean);
    function  GetFlag(f: flags): boolean;
    function  AdrA: bytep;
    function  AdrY: bytep;
    function  AdrX: bytep;
    function  GetState: byte;
    procedure ZN(Value: byte);
    procedure SetState(stat: byte);
    procedure Setareg(Value: byte);
    procedure SetXreg(Value: byte);
    procedure SetYreg(Value: byte);
    procedure setSp(b: byte);
    function  GetSp: byte;
    procedure setStackBase(w: byte = 1);

    procedure ROLbase(var b: byte);
    procedure ASLbase(var b: byte);
    procedure LSRbase(var b: byte);
    procedure RORbase(var b: byte);
    procedure Cp(reg, val: byte);
    procedure SB(Value: byte);
    procedure AD(Value: byte);
    procedure incr(var b: byte);
    procedure decr(var b: byte);
    //procedure ASLa;
    //procedure LSRa;
    //procedure ROLa;
    //procedure RORa;
    procedure INY;
    procedure DEY;
    procedure INX;
    procedure DEX;
    procedure TAY;
    procedure TYA;
    procedure TAX;
    procedure TXA;
    procedure TSX;
    procedure TXS;
    procedure CLV;

    procedure regInit(pc: word=0);
    procedure Show;

    property s: byte read GetSp write setSp;
    property p: byte read getState write SetState;
    property a: byte read areg write Setareg;
    property x: byte read xreg write SetXreg;
    property y: byte read yreg write SetYreg;
    property pc: word read fpc write fpc;
    property state: flagSet read pReg write pReg;
  end;


  function byte2hex(nr: byte): str3;
  function word2hex(address: word): str7;
  function byte2bin(nr: byte): str15;
  function divmod(var w: dword; d: dword): dword;
  function fromTwosCom(v, mask: BYTE): integer;

const
  testary: array[0..7] of byte = (0,1, 2, 3, 4, 5, 6, 7);

implementation

{num2hex}

  function divmod(var w: dword; d: dword): dword; {$asmMode intel}
  begin
    asm
       xor edx,edx
       mov ecx,w
       mov eax,[ecx]
       div d
       mov [ecx],eax
       mov Result,edx
    end;
  end;

  function  RORD(d: dword; ind: byte): dword;
  begin asm
    xor edx,edx
    inc edx
    push edx
    mov cl,ind
    mov eax,d
    shl edx,cl
    dec edx // mask
    and edx,eax
    xor eax,edx
    pop ecx
    and ecx,edx
    shr edx,1
    or  eax,edx
    mov edx,ecx
    mov cl,ind
    shl edx,cl
    or  eax,edx
    mov Result,eax
  end; end;

  function  RORw(w: word; ind: byte): word;
  var mask: word;
  begin
    mask := pred(1 shl ind) and w;
    result := (w xor mask ) or ((mask and 1) shl ind) or (mask shr 1);
  end;

  function fromTwosCom(v, mask: BYTE): integer;
  begin
    result :=  (v and pred(mask)) - (v and mask);
  end;

  function FromBCD(b: byte): byte;
  var  tmp: dword;
  begin
    tmp := b;
    result := divmod(tmp, 16) + (10 * tmp);
  end;

  function ToBCD(w: byte): word;
  var  tmp: dword;
  begin
    tmp := w;
    result := divmod(tmp, 10) + (16 * tmp);
  end;


  function dig2char(nr: byte): char;
  const
    HexStr: packed array[0..15] of char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  begin
    Result :=  HexStr[nr and $F];
  end;

  function byte2hex(nr: byte): str3;
  begin
    Result := dig2char(nr shr 4) + dig2char(nr);
  end;

  function byte2bin(nr: byte): str15;
  var i: word;
  begin result := '';
    for i := 1 to 8 do begin
      Result := Result + dig2char(ord(nr > $7f));
      nr := nr shl 1;
    end;
  end;

  function word2hex(address: word): str7;
  begin
    Result := byte2hex(wordRec(address).hi) + byte2hex(wordRec(address).lo);
  end; {num2hex}


  {registers}
  function  tRegisters.AdrA: bytep;
  begin
    result := @aReg;
  end;

  function  tRegisters.AdrY: bytep;
  begin
    result := @YReg;
  end;

  function  tRegisters.AdrX: bytep;
  begin
    result := @XReg;
  end;

  procedure tRegisters.SetFlags(f: flagset; NewStates: byte);
  begin
    newStates := bytep(@f)^ and NewStates;
    state := state - f;
    p := p or  (NewStates);
  end;

  procedure tRegisters.regInit(pc: word=0);
  begin
    a := 0    ;      // Accumulator
    x := 0     ;     // General Purpose X
    y := 0     ;     // General Purpose Y
    fsp := $1ff ;    // Stack Pointer
    pc := pc  ;      // Program Counter
    p := $24;        // Flag Pointer - N|V|1|B|D|I|Z|C
  end;

  procedure tRegisters.SetFlag(f: flags; stat: boolean = true);
  begin
    if stat then
      State := State + [f]
    else
      State := State - [f];
  end;

  function tRegisters.GetFlag(f: flags): boolean;
  begin
    GetFlag := f in State;
  end;


  procedure tRegisters.ZN(Value: byte);
  begin
    setflag(fz, Value = 0);
    setflags([fn], Value);
  end;

  procedure tRegisters.Cp(reg, val: byte);
  begin
    setflag(fc, reg >= val);
    ZN(reg - val);
  end;

  function tRegisters.GetState: byte;
  begin
    Result := bytep(@pReg)^;
  end;

  procedure tRegisters.SetState(stat: byte);
  begin
    bytep(@pReg)^ := stat;
  end;

  procedure tRegisters.Setareg(Value: byte);
  begin
    ZN(Value);
    areg := Value;
  end;

  procedure tRegisters.SetXreg(Value: byte);
  begin
    ZN(Value);
    Xreg := Value;
  end;

  procedure tRegisters.SetYreg(Value: byte);
  begin
    ZN(Value);
    Yreg := Value;
  end;

  procedure tRegisters.setStackBase(w: byte);
  begin
    wordRec(fsp).Hi := w;
    wordRec(fsp).Lo := $ff;
  end;

  function tRegisters.GetSp: byte;
  begin
    Result := wordRec(fsp).lo;
  end;

  procedure tRegisters.setSp(b: byte);
  begin
    wordRec(fsp).Lo := b;
  end;

  procedure tRegisters.show;
  begin
    //if not (DebugOn) then exit;

      WriteLn('PC=', word2hex(PC),
        //' opc=', byte2hex(opcode),
        ' A=', byte2hex(A),
        ' X=', byte2hex(X),
        ' Y=', byte2hex(Y),
        ' P=', byte2bin(P));
      readln;
  end;

  procedure tRegisters.SB(Value: byte);
    var  r: integer;   v: byte;
    begin
      v := a;
      if Getflag(fd) then  begin
        r := FromBCD(v) - FromBCD(Value) - Ord(not odd(p));
        a := ToBCD(r mod 100);
      end   else   begin
        r := A - Value - Ord(not odd(p));
        A := r and $ff;
      end;
      setflag(fc, r >= 0);
      setflag(fv, (v xor Value) and (v xor r) and $80 <> 0);
    end;

  procedure tRegisters.AD(Value: byte);
    var  r: integer;   v: byte;
    begin
      v := a;
      if Getflag(fd) then   begin
        r := FromBCD(v) + FromBCD(Value) + Ord(odd(p));
        a := ToBCD(r mod 100);
        setflag(fc, r > 99);
      end   else  begin
        r := v + Value + Ord(odd(p));
        A := r and $ff;
        setflag(fc, r > $ff);
      end;
      setflag(fv, (not (v xor Value)) and (v xor r) and $80 <> 0);
    end;

  procedure  tRegisters.ROLbase(var b: byte);
  var res: byte;
  begin
    res := b;
    SetFlag(fc, (res > $7f));
    res := (res shl 1) or (res shr 7);
    b := res;
  end;

  procedure  tRegisters.ASLbase(var b: byte);
  var res: byte;
    begin
      res := b;
      SetFlag(fc, (res > $7f));
      res := res shl 1;
      zn(res);
      b := res;
    end;

  procedure  tRegisters.LSRbase(var b: byte);
  var res: byte;
  begin
    res := b;
    SetFlag(fc, odd(res));
    res := res shr 1;
    zn(res);
    b := res;
  end;

  procedure  tRegisters.RORbase(var b: byte);
  var res: byte;
  begin
    res := b;
    SetFlag(fc, odd(res));
    res := (res shr 1) or (res shl 7);
    zn(res);
    b := res;
  end;

  procedure  tRegisters.incr(var b: byte);
  var res: byte;
  begin
    res := b;
    res := succ(res);
    zn(res);
    b := res;
  end;

  procedure  tRegisters.decr(var b: byte);
  var res: byte;
  begin
    res := b;
    res := PRED(res);
    zn(res);
    b := res;
  end;

  procedure tRegisters.INY; begin incr(adry^); END;
  procedure tRegisters.DEY; begin DECr(adry^); END;
  procedure tRegisters.INX; begin incr(adrx^); END;
  procedure tRegisters.DEX; begin DECr(adrx^); END;
  procedure tRegisters.TAY; BEGIN Y := A; END;
  procedure tRegisters.TYA; BEGIN A := X; END;
  procedure tRegisters.TAX; BEGIN X := X; END;
  procedure tRegisters.TXA; BEGIN A := X; END;
  procedure tRegisters.TSX; BEGIN X := X; END;
  procedure tRegisters.TXS; BEGIN S := X; END;
  //procedure tRegisters.ASLa; begin ASLbase(adra^); end;
  //procedure tRegisters.LSRa; begin LSRbase(adra^); end;
  //procedure tRegisters.ROLa; begin ROLbase(adra^); end;
  //procedure tRegisters.RORa; begin RORbase(adra^); end;
  procedure tRegisters.CLV; begin setflag(fv, false); end;

  {registers}

end.


