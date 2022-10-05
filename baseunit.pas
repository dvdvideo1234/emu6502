unit BaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  adrType = (INDX, ZP_, IMM_, ABS_, INDY, ZPX, ABSY, ABSX,
          zpy, RegA, Reg_SP, RegP);
  pageType = (page0, page1, page2, page3);
  byteAry  = array of byte;
  ProcType = procedure ;
  opType = (op0, op1, op2, op3, op4, op5, op6, op7);
  flags = (cf, zf, fi, df, bf, xf, vf, nf);
  flagSet = packed set of flags;
  wordAdressable = array[word] of byte;
  bytep = ^byte;
  FuncType= function() : bytep;
  str3   = string[3];
  str7   = string[7];
  str15  = string[15];

  tByteMymory = object
    private
      fMemory: wordAdressable;
    protected
      procedure Store(address: word; Value: byte);
      function  Fetch(address: word): byte;
    public
      function  PageAdr(adr: word): word;
      procedure doPatch(adr: word; Patch: byteAry);
      procedure dump(adr, cnt: word);

      property bytes : wordAdressable read fMemory write fMemory;
  end;


  tByteStack = object
    private
      fsp: word;
    protected
      procedure setSp(b: byte);
      function  GetSp: byte;
      function  GetSpAdr: bytep;
    public
      procedure setStackBase(w: byte = 1);
      function  pop: byte;
      procedure push(b: byte);
      function  popAdr: word;
      procedure pushAdr(w: word);

      property sp: byte read GetSp write setSp;
      property sadr: bytep read  GetSpAdr;
  end;

  function byte2hex(nr: byte): str3;
  function word2hex(address: word): str7;
  function byte2bin(nr: byte): str15;

var
  mem: tByteMymory;
  stack: tByteStack;
const
  testary: array[0..7] of byte = (0,1, 2, 3, 4, 5, 6, 7);

implementation

{num2hex}

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


  {memory}

  function tByteMymory.Fetch(address: word): byte;
  begin
    result := fMemory[address];
  end;

  procedure tByteMymory.Store(address: word; value: byte);
  begin
    fMemory[address] := Value;
  end;

  function tByteMymory.PageAdr(adr: word): word;
  begin
    wordRec(result).lo := Fetch(adr);
    inc(wordRec(adr).Lo);
    wordRec(result).hi := Fetch(adr);
  end;

  procedure tByteMymory.doPatch(adr: word; Patch: byteAry);
  var i: word;
    b: byte;
  begin
    for i := 0 to high(Patch) do begin
      mem.bytes[adr] := patch[i];
      inc(adr);
    end;
  end;


  procedure tByteMymory.dump(adr, cnt: word);
    var i, ind: word;

    procedure NewLine;
    begin
      ind := 0;
      writeln;
      write(word2hex(adr), '  ');
    end;

  begin NewLine;
    for i := cnt downto 1 do begin
      write(byte2hex(mem.bytes[adr]), ' ');
      inc(adr);
      inc(ind);
      if ind = 20  then NewLine
    end;
    writeln;
  end;



  {Stack}

  procedure tByteStack.setStackBase(w: byte);
  begin
    wordRec(fsp).Hi := w;
    wordRec(fsp).Lo := $ff;
  end;

  function tByteStack.GetSp: byte;
  begin
    Result := wordRec(fsp).lo;
  end;

  procedure tByteStack.setSp(b: byte);
  begin
    wordRec(fsp).Lo := b;
  end;

  function tByteStack.pop: byte;
  begin
    Inc(wordRec(fsp).Lo);
    Result := mem.bytes[fsp];
  end;

  procedure tByteStack.push(b: byte);
  begin
    mem.bytes[fsp] := b;
    Dec(wordRec(fsp).Lo);
  end;

  function tByteStack.popAdr: word;
  begin
    wordRec(result).lo := pop;
    wordRec(result).Hi := pop;
  end;

  procedure tByteStack.pushAdr(w: word);
  begin
    push(wordRec(w).Hi);
    push(wordRec(w).lo);
  end; {Stack}

  function  tByteStack.GetSpAdr: bytep;
  begin
    Result := @(wordRec(fsp).lo);
  end;


end.

