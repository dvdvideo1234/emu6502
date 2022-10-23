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
  pFlagset= ^FlagSet;
  wordAdressable = array[word] of byte;

  tByteRegister = object
    fReg: byte;
    //function  Adr: bytep;
  end;

  tbyteIndex = object(tByteRegister)
    procedure incr;
    procedure decr;
  end;

  tFlagRegister = object(tByteRegister)
    procedure SetFlags(f: flagset; NewStates: byte);
    procedure SetFlag(f: flags; stat: boolean = true);
    procedure clrFlag(f: flags);
    function  GetFlag(f: flags): boolean;
    function  ZN(Value: byte): byte;
    function  GetC: boolean;
  end;


  tRegisters = packed object
    yReg, aReg, xReg:  tByteRegister;
    sReg: tbyteIndex;
    shReg: tByteRegister;
    pReg:  tFlagRegister;
    pcLo, PcHi: tByteRegister;
    procedure RL(var b: byte; f: boolean = false);
    procedure RR(var b: byte; f: boolean = false);
    procedure SB(Value: byte);
    procedure AD(Value: byte);
    procedure cp(reg, value: byte);
    procedure incr(var b: byte);
    procedure decr(var b: byte);
  end;

  function byte2hex(nr: byte): str3;
  function word2hex(address: word): str7;
  function byte2bin(nr: byte): str15;
  function divmod(var w: dword; d: dword): dword;
  function fromTwosCom(v, mask: BYTE): integer;
  function  ROLD(d: dword; ind: byte): dword;

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

  function  ROLD(d: dword; ind: byte): dword;
  label stayz;
  begin asm
    xor edx,edx
    inc edx
    mov cl,ind
    mov eax,d
    and cl,31
    shl edx,cl

    mov ecx,edx
    and ecx,eax // one bit isolate
    xor eax,ecx // and zeroed

    dec edx    // mask

    and edx,eax
    xor eax,edx
    shl edx,1
    jecxz stayz
    inc  edx
stayz:
    or  eax,edx
    mov Result,eax
  end; end;

function  RORD(d: dword; ind: byte): dword;
label stayz;
begin asm
  xor edx,edx
  inc edx
  mov cl,ind
  mov eax,d
  and cl,31
  shl edx,cl

  mov ecx,edx  // one bit isolate

  dec edx    // mask
  or  edx,ecx

  and edx,eax
  xor eax,edx
  shr edx,1
  jnc stayz
  or  edx,ecx
stayz:
  or  eax,edx
  mov Result,eax
end; end;

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
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
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


  procedure tbyteIndex.incr;
  begin
    inc(fReg);
  end;

  procedure tbyteIndex.decr;
  begin
    dec(fReg);
  end;

  function tFlagRegister.ZN(Value: byte): byte;
  begin
    setflag(fz, Value = 0);
    setflags([fn], Value);
    result := Value;
  end;

  procedure tFlagRegister.SetFlags(f: flagset; NewStates: byte);
  begin
    newStates := bytep(@f)^ and NewStates;
    pflagset(@fReg)^ := pflagset(@fReg)^ - f;
    fReg := fReg or NewStates;
  end;

  procedure tFlagRegister.clrFlag(f: flags);
  begin
    pflagset(@fReg)^ := pflagset(@fReg)^ - [f];
  end;

  procedure tFlagRegister.SetFlag(f: flags; stat: boolean);
  begin
    if stat then
      pflagset(@fReg)^ := pflagset(@fReg)^ + [f]
    else  clrFlag(f);
  end;

  function  tFlagRegister.GetFlag(f: flags): boolean;
  begin
    result := f in pflagset(@fReg)^;
  end;

  function  tFlagRegister.GetC: boolean;
  begin
    result := fc in pflagset(@fReg)^;
  end;

procedure tRegisters.SB(Value: byte);
  var  r: integer;   v: byte;
  begin
    v := aReg.fReg;
    if pReg.Getflag(fd) then  begin
      r := FromBCD(v) - FromBCD(Value) - Ord(not odd(pReg.fReg));
      aReg.fReg := ToBCD(r mod 100);
    end   else   begin
      r := aReg.fReg - Value - Ord(not odd(pReg.fReg));
      aReg.fReg := r and $ff;
    end;
    preg.setflag(fc, r >= 0);
    preg.setflag(fv, (v xor Value) and (v xor r) and $80 <> 0);
  end;

  procedure  tRegisters.cp(reg, value: byte);
  var res: SmallInt;
  begin
    res := Reg - value;
    preg.setflag(fc, res >= 0);
    preg.zn(res);
  end;

  procedure tRegisters.AD(Value: byte);
    var  r: integer;   v: byte;
    begin
      v := aReg.fReg;
      if preg.Getflag(fd) then   begin
        r := FromBCD(v) + FromBCD(Value) + Ord(odd(pReg.fReg));
        aReg.fReg := ToBCD(r mod 100);
        preg.setflag(fc, r > 99);
      end   else  begin
        r := v + Value + Ord(odd(pReg.fReg));
        aReg.fReg := r and $ff;
        preg.setflag(fc, r > $ff);
      end;
      preg.setflag(fv, (not (v xor Value)) and (v xor r) and $80 <> 0);
    end;

  procedure  tRegisters.RL(var b: byte; f: boolean);
  var res: word;
  begin
    res := (b shl 1)  or ord(f);
    b := res;
    preg.zn(res);
    preg.setflag(fc, odd(wordrec(res).Hi) );
  end;

  procedure  tRegisters.RR(var b: byte; f: boolean);
  var res: word;
  begin
    res := b;
    wordrec(res).Hi:= ord(f);
    preg.setflag(fc, odd(res) );
    res := (res shr 1);
    b := res;
    preg.zn(res);
  end;

  procedure  tRegisters.incr(var b: byte);
  var res: byte;
  begin
    res := b;
    res := succ(res);
    preg.zn(res);
    b := res;
  end;

  procedure  tRegisters.decr(var b: byte);
  var res: byte;
  begin
    res := b;
    res := PRED(res);
    preg.zn(res);
    b := res;
  end;

end.


