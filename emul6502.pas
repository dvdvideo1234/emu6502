Program emul6502;

Const
 DebugOn:Boolean=True;
StackBase = $100;
flags = (cf, zf, fi, df, bf, xf, vf, nf);

Var
 codeRunning:Boolean;
 A:Byte;     { Registre accumulateur }
 X:Byte;     { Registre d'index X }
 Y:Byte;     { Registre d'index Y }
 P:set of flags;     { Registre d'etat de processeur }
 PC:Word; { Registre de compteur der programmes }
 SP:Byte; { Registre de pointeur de pile }
 memory:	Array[0..16383] of Byte;

Procedure InitEmul;Begin
 codeRunning:=True;
 A:=0;
 X:=0;
 Y:=0;
 P:=[];
 SP:=0;
 PC:=$600;
End;

procedure SetFlag(f: flags; state: boolean);
begin
if state
	then p := p + f
else p := p - f;
end;

Function GetFlag(f: flags): boolean;
begin
GetFlag := f in p;
end;

Function num2hex(nr:Byte):String;
Const
 HexStr:Array[0..15]of Char=('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
Begin
 num2hex := HexStr[nr shr 4] + HexStr[nr and $F];
End;

Function addr2hex(address:Word):String;Begin
  addr2hex := num2hex((address shr 8))+num2hex(address and $ff);
End;

Function NextByte:Byte;
Begin
 PC := PC + 1;
 NextByte := memory[PC];
End;

Function NextWord:Word;Begin
 NextWord := NextByte + (NextByte shl 8);
End;

Procedure stackPush(value:Byte);
Begin
   memory[SP +StackBase] := value;
   SP := SP - 1;
End;

Function stackPop:Byte;
Begin
   SP := SP + 1;
   stackPop := memory[SP+StackBase];
End;

Procedure jumpBranch(offset:byte);
Begin
PC := PC + offset;
  If offset > $7f Then PC := PC - $100;
End;

Procedure memStoreByte(address:Word; value:Byte);
Begin
 memory[address] := value ;
End;

Function memReadByte(address:Word):Byte;
Begin
 memReadByte := memory[address];
End;

Procedure test(value:Byte);
Begin
setflag(zf,value = 0);
setflag(nf,(value  and $80 )<> 0 );
End;

Procedure doCompare(reg,val:Word);
Begin
setflag(cf,reg>=val);
test(reg-val);
End;

function IndirAdr(adr): word;
begin
IndirAdr := memReadByte(adr) + (memReadByte(adr+1) shl 8);
end;

function _Rcr(var b: byte; f: boolean): boolean;
var hib: byte;
begin
hib := b;
_Rcr := ord(hib);
hib := hib shr 1;
if f then hib := hib or $80;
test(hib);
b := hib;
end;

function _Rcl(var b: byte; f: boolean): boolean;
var hib: byte;
begin
hib := b;
_Rcl := ord(hib>$7f);
hib := hib shl 1;
if f then inc(hib);
test(hib);
b := hib;
end;

function _unpackb(b: byte): byte;
var tmp: byte;
begin
tmp := b shr 4;
_unpackb := ((tmp shl 2) + tmp) shl 1) + (b and %f);
end;

function _packb(w: byte): Word;
begin
_packb := ((w div 10) shl 4) + ( w mod 10);
end;


Procedure testADC(value:Byte);
Var tmp, tempv:Word;
Begin
setflag(vf,(A xor value) and $80 <> 0);
  If Getflag(df) Then Begin
	tmp := _unpackb(a) + _unpackb(value) + ord(cf in p);
setflag(cf, tmp >= 100);
If cf in p Then dec(tmp,100);
tmp := _packb(tmp); end
else begin
    tmp := A + value + ord(cf in p);
setflag(cf, tmp >= $100);
    If cf in p Then dec(tmp,$100);
end;
  A := tmp and $ff;
test(a);
End;

Procedure testSBC(value:Byte);
Var
 tmp,w:Word;
Begin
setflag(vf,(A xor value) and $80 <> 0);
  If Getflag(df) Then Begin
	tmp := 99 + _unpackb(a) - _unpackb(value) + ord(cf in p);
setflag(cf, tmp >= 100);
If cf in p Then dec(tmp,100);
tmp := _packb(tmp); end
else begin
    tmp := $ff + A - value + ord(cf in p);
setflag(cf, tmp >= $100);
    If cf in p Then dec(tmp,$100);
end;
  A := w and $ff;
test(a);
End;

Procedure ExecEmul;
Var
 opcode,zp,offset:Byte;
 currAddr,address,value,sf:Word;
Begin
  If Not(codeRunning)Then Exit;
  opcode := NextByte;
  If(DebugOn)Then WriteLn('PC=', addr2hex(PC-1), ' opcode=', opcode, ' X=', X, ' Y=', Y, ' A=', A, ' P=', P);
  Case opcode of
    $00:                            { BRK implicite }
      codeRunning := false;
    $01:Begin                       { ORA INDX }
      address := NextByte + X;
      value := IndirAdr(address);
      A := A or memReadByte(value);
	test(a);
      //If A <> 0 Then P := P and $fd else P := P or $02;
      //If A and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
    $05:Begin                       { ORA ZP }
      zp := NextByte;
      A := A or memReadByte(zp);
	test(a);
      //If A <> 0 Then P := P and $fd else P := P or $02;
      //If A and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
    $06:Begin                       { ASL ZP }
      zp := NextByte;
      value := memReadByte( zp );
      P := (P and $fe) or ((value shr 7) and 1);
      value := value shl 1;
      memStoreByte( zp, value );
      If value <> 0 Then P := P and $fd else P := P or $02;
      If value and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
    $08:                            { PHP }
      stackPush(P);
    $09:Begin                       { ORA IMM }
      A := A or NextByte;
      If A <> 0 Then P := P and $fd else P := P or $02;
      If A and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
    $0a:Begin                       { ASL IMPL }
       P := (P and $fe) or ((A shr 7) and 1);
       A := A shl 1;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $0d:                            { ORA ABS }
      Begin
       A := A or memReadByte( NextWord );
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $0e:Begin                       { ASL ABS }
       address := NextWord;
       value := memReadByte(address);
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or 2;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $10:Begin                      { BPL }
       offset := NextByte;
       If P and $80 = 0 Then jumpBranch( offset );
      End;
    $11:Begin                      { ORA INDY }
       zp := NextByte;
       value := memReadByte(zp) + (memReadByte(zp+1) shl 8) + Y;
       A := A or memReadByte(value);
       If A <> 0 Then P := P and $fd  Else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $15:Begin                      { ORA ZPX }
       address := (NextByte + X) and $ff;
       A := A or memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $16:Begin                      { ASL ZPX }
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 <> $80 Then P := P or $80 else P := P and $7f;
      End;
    $18:                            { CLC }
      P := P and $fe;
    $19:Begin                       { ORA ABSY }
       address := NextWord + Y;
       A := A or memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $1d:Begin                       { ORA ABSX }
       address := NextWord + X;
       A := A or memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $1e:Begin                       { ASL ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $20:Begin                      { JSR ABS }
       address := NextWord;
       currAddr := PC - 1;
       stackPush((currAddr shr 8) and $ff);
       stackPush(currAddr and $ff);
       PC := address;
      End;
    $21:Begin                      { AND INDX }
       address := (NextByte + X) and $ff;
       value := memReadByte(address) + (memReadByte(address + 1) shl 8);
       A := A and value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $24:Begin                      { BIT ZP }
       zp := NextByte;
       value := memReadByte( zp );
       If value and A <> 0 Then P := P and $fd else P := P or $02;
       P := (P and $3f) or (value and $c0);
      End;
    $25:Begin                      { AND ZP }
       zp := NextByte;
       A := A and memReadByte( zp );
       If A <> 0 Then P := P and $fd else P := P or 2;
       If A and $80 = $80 Then P := P and $80 else P := P and $7f;
      End;
    $26:Begin                      { ROL ZP }
       sf := (P and 1);
       address := NextByte;
       value := memReadByte( address );
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       value := value or sf;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $28:                            { PLP }
      P := stackPop or $20;
    $29:Begin                       { AND IMM }
       A := A and NextByte;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
    $2a:Begin                       { ROL A }
       sf := (P and 1);
       P := (P and $fe) or ((A shr 7) and 1);
       A := A shl 1;
       A := A or sf;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $2c:Begin                      { BIT ABS }
       value := memReadByte(NextWord);
       If value and A <> 0 Then P := P and $fd else P := P or $02;
       P := (P and $3f) or (value and $c0);
      End;
    $2d:Begin                      { AND ABS }
       value := memReadByte(NextWord);
       A := A and value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80=$80 Then P := P or $80 else P := P and $7f;
      End;
    $2e:Begin                      { ROL ABS }
       sf := P and 1;
       address := NextWord;
       value := memReadByte( address );
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       value := value or sf;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $30:Begin                       { BMI }
       offset := NextByte;
       If P and $80 <> $80 Then jumpBranch( offset );
      End;
    $31:Begin                       { AND INDY }
       zp := NextByte;
       value := memReadByte(zp) + (memReadByte(zp+1) shl 8) + Y;
       A := A and memReadByte(value);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $35:Begin                       { AND INDX }
       zp := NextByte;
       value := memReadByte(zp) + (memReadByte(zp+1) shl 8) + X;
       A := A and memReadByte(value);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $36:Begin                       { ROL ZPX }
       sf := P and 1;
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       value := value or sf;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $38:                            { SEC }
      P := P or 1;
    $39:Begin                       { AND ABSY }
       address := NextWord + Y;
       value := memReadByte( address );
       A := A and value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and  $7f;
      End;
    $3d:Begin                       { AND ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       A := A and value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $3e:Begin                       { ROL ABSX }
       sf := P and 1;
       address := NextWord + X;
       value := memReadByte(address);
       P := (P and $fe) or ((value shr 7) and 1);
       value := value shl 1;
       value := value or sf;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $40:;                           { RTI (non-supporte, =NOP) }
    $41:Begin                       { EOR INDX }
       zp := (NextByte + X) and $ff;
       value := memReadByte(zp) + (memReadByte(zp+1) shl 8);
       A := A xor memReadByte(value);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $45:Begin                       { EOR ZPX }
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       A := A xor value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $46:Begin                       { LSR ZP }
       address := NextByte and $ff;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or 2;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $48:                            { PHA }
      stackPush(A);
    $49:Begin                       { EOR IMM }
       A := A xor NextByte;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P + $80 else P := P and $7f;
      End;
    $4a:Begin                        { LSR }
       P := (P and $fe) or (A and 1);
       A := A shr 1;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $4c:                            { JMP abs }
      PC := NextWord;
    $4d:Begin                       { EOR abs }
       address := NextWord;
       value := memReadByte(address);
       A := A xor value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $4e:Begin                       { LSR abs }
       address := NextWord;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $50:Begin                      { BVC (efface le debordement) }
       offset := NextByte;
       If P and $40 = 0 Then jumpBranch(offset);
      End;
    $51:Begin                      { EOR INDY }
       zp := NextByte;
       value := memReadByte(zp) + (memReadByte(zp+1) shl 8) + Y;
       A := A xor memReadByte(value);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $55:Begin                      { EOR ZPX }
       address := (NextByte + X) and $ff;
       A := A xor memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $56:Begin                      { LSR ZPX }
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $58:;                          { CLI (ne fait rien) }
    $59:Begin                      { EOR ABSY }
       address := NextWord + Y;
       value := memReadByte(address);
       A := A xor value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $5d:Begin                      { EOR ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       A := A xor value;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $5e:Begin                      { LSR ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $60:                           { RTS }
      PC := (stackPop + 1) or (stackPop shl 8);
    $61:Begin                      { ADC INDX }
       zp := (NextByte + X) and $ff;
       address := memReadByte(zp) + (memReadByte(zp+1) shl 8);
       value := memReadByte(address);
       testADC( value );
      End;
    $65:Begin                      { ADC ZP }
       address := NextByte;
       value := memReadByte(address);
       testADC(value);
      End;
    $66:Begin                      { ROR ZP }
       sf := P and 1;
       address := NextByte;
       value := memReadByte( address );
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       If sf <> 0 Then value := value or $80;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $68:Begin                      { PLA }
       A := stackPop;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $69:Begin                      { ADC IMM }
       value := NextByte;
       testADC(value);
      End;
    $6a:Begin                      { ROR A }
      sf := P and 1;
      P := (P and $fe) or (A and 1);
      A := A shr 1;
      If sf <> 0 Then A := A or $80;
      If A <> 0 Then P := P and $fd else P := P or $02;
      If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $6c:                           { JMP INDIR }
      PC := memReadByte(NextByte) + (memReadByte(NextByte) shl 8);
    $6d:Begin                      { ADC ABS }
       address := NextWord;
       value := memReadByte(address);
       testADC(value);
      End;
    $6e:Begin                      { ROR ABS }
       sf := P and 1;
       address := NextWord;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       If sf <> 0 Then value := value or $80;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $70:Begin                     { BVS (branch on overflow set) }
       offset := NextByte;
       If P and $40 = $40 Then jumpBranch(offset);
      End;
    $71:Begin                     { ADC INY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp + 1) shl 8);
       value := memReadByte(address + Y);
       testADC(value);
      End;
    $75:Begin                     { ADC ZPX }
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       testADC(value);
      End;
    $76:Begin                     { ROR ZPX }
       sf := (P and 1);
       address := (NextByte + X) and $ff;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       If sf <> 0 Then value := value or $80;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $78:;                          { SEI (ne fait rien) }
    $79:Begin                      { ADC ABSY }
       address := NextWord;
       value := memReadByte(address + Y);
       testADC(value);
      End;
    $7d:Begin                      { ADC ABSX }
       address := NextWord;
       value := memReadByte(address + X);
       testADC(value);
      End;
    $7e:Begin                      { ROR ABSX }
       sf := P and 1;
       address := NextWord + X;
       value := memReadByte(address);
       P := (P and $fe) or (value and 1);
       value := value shr 1;
       If value <> 0 Then value := value or $80;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P and $80 else P := P and $7f;
      End;
    $81:Begin                     { STA INDX }
       zp := (NextByte + X) and $ff;
       address := memReadByte(zp) + (memReadByte(zp+1) shl 8);
       memStoreByte(address, A);
      End;
    $84:                           { STY ZP }
      memStoreByte( NextByte, Y);
    $85:                           { STA ZP }
      memStoreByte( NextByte, A);
    $86:                           { STX ZP }
      memStoreByte( NextByte, X);
    $88:Begin                      { DEY (1 octet) }
       dec(Y);
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $8a:Begin                      { TXA (1 octet); }
      A := X and $ff;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $8c:                           { STY abs }
      memStoreByte( NextWord, Y );
    $8d:                           { STA ABS (3 octets) }
      memStoreByte( NextWord, A );
    $8e:                           { STX abs }
      memStoreByte( NextWord, X );
    $90:Begin                      { BCC (branchement dans l'effacement de retenue) }
       offset := NextByte;
       If P and 1 = 0 Then jumpBranch( offset );
      End;
    $91:Begin                      { STA INDY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp + 1) shl 8) + Y;
       memStoreByte(address, A);
      End;
    $94:                           { STY ZPX }
      memStoreByte( NextByte + X, Y );
    $95:                           { STA ZPX }
      memStoreByte( NextByte + X, A );
    $96:                           { STX ZPY }
      memStoreByte( NextByte + Y, X );
    $98:Begin                      { TYA }
       A := Y and $ff;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $99:                           { STA ABSY }
      memStoreByte( NextWord + Y, A );
    $9a:                           { TXS }
      SP := X and $ff;
    $9d:Begin                      { STA ABSX }
       address := NextWord;
       memStoreByte(address + X, A);
      End;
    $a0:Begin                      { LDY IMM }
       Y := NextByte;
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a1:Begin                      { LDA INDX }
       zp := (NextByte + X) and $ff;
       address := memReadByte(zp) + (memReadByte(zp + 1) shl 8);
       A := memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a2:Begin                      { LDX IMM }
       X := NextByte;
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a4:Begin                      { LDY ZP }
      Y := memReadByte(NextByte);
      If Y <> 0 Then P := P and $fd else P := P or $02;
      If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a5:Begin                      { LDA ZP }
       A := memReadByte(NextByte);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a6:Begin                      { LDX ZP }
       X := memReadByte(NextByte);
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a8:Begin                      { TAY }
       Y := A and $ff;
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $a9:Begin                      { LDA IMM }
       A := NextByte;
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $aa:Begin                      { TAX }
       X := A and $ff;
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $ac:Begin                      { LDY ABS }
       Y := memReadByte(NextWord);
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $ad:Begin                      { LDA ABS }
       A := memReadByte(NextWord);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $ae:Begin                      { LDX ABS }
       X := memReadByte(NextWord);
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $b0:Begin                      { BCS }
       offset := NextByte;
       If P and 1 = 1 Then jumpBranch(offset);
      End;
    $b1:Begin                      { LDA INDY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp+1) shl 8) + Y;
       A := memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $b4:Begin                      { LDY ZPX }
       Y := memReadByte(NextByte + X);
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y = Y and $80 Then P := P or $80 else P := P and $7f;
      End;
    $b5:Begin                      { LDA ZPX }
       A := memReadByte((NextByte + X) and $ff);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $b6:Begin                      { LDX ZPY }
       X := memReadByte(NextByte + Y);
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $b8:                          { CLV }
      P := P and $bf;
    $b9:Begin                     { LDA ABSY }
       address := NextWord + Y;
       A := memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $ba:                          { TSX }
      X := SP and $ff;
    $bc:Begin                     { LDY ABSX }
       address := NextWord + X;
       Y := memReadByte(address);
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $bd:Begin                     { LDA ABSX }
       address := NextWord + X;
       A := memReadByte(address);
       If A <> 0 Then P := P and $fd else P := P or $02;
       If A and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $be:Begin                     { LDX ABSY }
       address := NextWord + Y;
       X := memReadByte(address);
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $c0:Begin                     { CPY IMM }
       value := NextByte;
       If Y + value > $ff Then P := P or 1 else P := P and $fe;
       value := Y-value;
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $c1:Begin                     { CMP INDY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp + 1) shl 8) + Y;
       value := memReadByte(address);
       doCompare(A, value);
      End;
    $c4:Begin                     { CPY ZP }
       value := memReadByte(NextByte);
       doCompare(Y, value);
      End;
    $c5:Begin                     { CMP ZP }
       value := memReadByte(NextByte);
       doCompare(A, value);
      End;
    $c6:Begin                     { DEC ZP }
       zp := NextByte;
       value := memReadByte(zp);
       value := value - 1;
       memStoreByte( zp, value and $ff );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $c8:Begin                     { INY }
       Y := (Y + 1) and $ff;
       If Y <> 0 Then P := P and $fd else P := P or $02;
       If Y and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $c9:Begin                     { CMP IMM }
      value := NextByte;
      doCompare( A, value );
      End;
    $ca:Begin                     { DEX }
       X := (X - 1) and $ff;
       If X <> 0 Then P := P and $fd else P := P or $02;
       If X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $cc:Begin                     { CPY ABS }
       value := memReadByte(NextWord);
       doCompare(Y, value);
      End;
    $cd:Begin                     { CMP ABS }
       value := memReadByte(NextWord);
       doCompare(A, value);
      End;
    $ce:Begin                     { DEC ABS }
       address := NextWord;
       value := memReadByte( address );
       value := value - 1;
       value := value and $ff;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P  or $02;
       If value and $80 = $80 Then P := P or $80 Else P := P and $7f;
      End;
    $d0:Begin                     { BNE }
       offset := NextByte;
       If P and 2 = 0 Then jumpBranch( offset );
      End;
    $d1:Begin                     { CMP INDY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp + 1) shl 8) + Y;
       value := memReadByte(address);
       doCompare(A, value );
      End;
    $d5:Begin                     { CMP ZPX }
       value := memReadByte( NextByte + X );
       doCompare(A, value);
      End;
    $d6:Begin                     { DEC ZPX }
       address := NextByte + X;
       value := memReadByte(address);
       value := value - 1;
       value := value and $ff;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $d8:                          { CLD (CLear Decimal) }
      P := P and $f7;
    $d9:Begin                     { CMP ABSY }
       address := NextWord + Y;
       value := memReadByte(address);
       doCompare(A, value);
      End;
    $dd:Begin                     { CMP ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       doCompare(A,value);
      End;
    $de:Begin                     { DEC ABSX }
       address := NextWord + X;
       value := memReadByte(address);
       value := value - 1;
       value := value and $ff;
       memStoreByte( address, value );
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value = value and $80 Then P := P or $80 else P := P and $7f;
      End;
    $e0:Begin                     { CPX IMM }
       value := NextByte;
       doCompare( X, value );
      End;
    $e1:Begin                     { SBC INDX }
       zp := (NextByte+X) and $ff;
       address := memReadByte(zp) + (memReadByte(zp+1) shl 8);
       value := memReadByte(address);
       testSBC(value);
      End;
    $e4:Begin                     { CPX ZP }
       value := memReadByte(NextByte);
       doCompare( X, value );
      End;
    $e5:Begin                     { SBC ZP }
       address := NextByte;
       value := memReadByte(address);
       testSBC(value);
      End;
    $e6:Begin                     { INC ZP }
       zp := NextByte;
       value := memReadByte(zp);
       value := value + 1;
       value := value and $ff;
       memStoreByte(zp, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $e8:Begin                     { INX }
       X := (X + 1) and $ff;
       if X <> 0 Then P := P and $fd else P := P or $02;
       if X and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $e9:Begin                     { SBC IMM }
       value := NextByte;
       testSBC(value);
      End;
    $ea:;                        { NOP }
    $ec:Begin                    { CPX ABS }
       value := memReadByte(NextWord);
       doCompare(X, value);
      End;
    $ed:Begin                    { SBC ABS }
       address := NextWord;
       value := memReadByte( address );
       testSBC(value);
      End;
    $ee:Begin                    { INC ABS }
       address := NextWord;
       value := memReadByte(address);
       value := value + 1;
       value := value and $ff;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $f0:Begin                     { BEQ }
       offset := NextByte;
       If P and 2 = 2 Then jumpBranch(offset);
      End;
    $f1:Begin                     { SBC INDY }
       zp := NextByte;
       address := memReadByte(zp) + (memReadByte(zp+1) shl 8);
       value := memReadByte( address + Y );
       testSBC( value );
      End;
    $f5:Begin                     { SBC ZPX }
       address := (NextByte + X) and $ff;
       value := memReadByte( address );
       P := (P and $fe) or (value and 1);
       testSBC(value);
      End;
    $f6:Begin                     { INC ZPX }
       address := NextByte + X;
       value := memReadByte(address);
       value := value + 1;
       value := value and $ff;
       memStoreByte(address, value);
       If value <> 0 Then P := P and $fd else P := P or $02;
       If value and $80 = $80 Then P := P or $80 else P := P and $7f;
      End;
    $f8:                         { SED }
      P := P or 8;
    $f9:Begin                    { SBC ABSY }
       address := NextWord;
       value := memReadByte(address + Y);
       testSBC(value);
      End;
    $fd:Begin                    { SBC ABSX }
       address := NextWord;
       value := memReadByte(address + X);
       testSBC(value);
      End;
    $fe:Begin                    { INC ABSX }
      address := NextWord + X;
      value := memReadByte(address);
      value := value + 1;
      value := value and $ff;
      memStoreByte(address, value);
      If value <> 0 Then P := P and $fd else P := P or $02;
      If value and $80 = $80 Then P := P or $80 else P := P and $7f;
     End;
     Else Begin
      WriteLn('Adresse $', addr2hex(PC), ' - code inconnu ', opcode);
      codeRunning := False;
     End;
  End;

  If (PC = 0) or (Not codeRunning) Then Begin
    WriteLn('Programme termine a PC=$', addr2hex(PC-1));
    codeRunning := False;
  End;
End;


BEGIN
 InitEmul;
 Repeat
  ExecEmul;
 Until Not codeRunning;
END.




{registers}

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
  procedure tRegisters.CLV; begin setflag(fv, false); end;

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


function tRegisters.ZN(Value: byte): byte;
begin
  setflag(fz, Value = 0);
  setflags([fn], Value);
  result := Value;
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

procedure  tRegisters.RORbase(var b: byte);
var res: byte;
begin
  res := b;
  SetFlag(fc, odd(res));
  res := (res shr 1) or (res shl 7);
  zn(res);
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


tRegisters = class    { An object to hold the CPU registers}
  YReg: byte;         { Register index Y }
  AReg: byte;         { Register accumulator }
  XReg: byte;         { Register index X }
  pReg: flagSet;      { Register processor }
  fPC: word;          { Register programm counter }
  fsp: word;          { Register Stack pointer }
  g:  tByteRegister;

  procedure SetFlags(f: flagset; NewStates: byte);
  procedure SetFlag(f: flags; stat: boolean);
  function  GetFlag(f: flags): boolean;
  function  AdrA: bytep;
  function  AdrY: bytep;
  function  AdrX: bytep;
  function  GetState: byte;
  function  ZN(Value: byte): byte;
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


