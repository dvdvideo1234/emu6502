//Il est a noter egalement que le code suivant n'est pas optimise pour le compilateur Turbo Pascal.
//Ainsi, il n'utilise pas les procedures INC et DEC afin d'incrementer ou decrementer des valeurs, 
//mais plutot une affectation manuelle. Le but de l'exemple suivant est de monter le fonctionnement 
//et de rendre facilement portable le code sous differentes marques de compilateurs Pascal. Dans 
//cet exemple, etant donne que l'emulateur ne contient rien dans la memoire de la RAM de l'emulateur 
//(variable �memory�), il arretera immediatement a l'instruction d'opcode BRK. Ce sera a vous de charger 
//des donnees afin pouvoir tester le comportement de routines Assembleur 6502.
//
//L'emulateur a la fonctionnalite du debogueur d'instruction active par defaut, lequel affiche 
//la valeur des registres et sa position d'execution entre chaque instruction executee. Il va de soit
//, que cette option n'est pas indispensables a l'emulateur mais permet aux developpeurs de suivre 
//facilement, pas a pas, ce qui se passe dans le programme tentant d'etre execute.

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

//Remarques
//
//    La banque de memoire est fixe a 16 Ko dans l'exemple du programme. Or, la puce electronique peut supporte 
//	jusqu'a 64 Ko, il faudra donc ajuster celle-ci selon vos besoins.
//    L'emulateur n'est pas cadence a une vitesse precise, ainsi, si vous executez sur une machine a 2 Ghz, 
//		votre puce executera le logiciel a la meme vitesse. Pour resoudre se probleme, il faudra donc ajouter
// un �Wait State� ou un delai d'attente utilisable avec une instruction comme DELAY dans de nombreuses marques
// de langage de programmation Pascal.
//
//    Si vous souhaitez integre d'autres puces electroniques a cette emulateur 6502, vous devrez intercepter 
//	les cellules memoires executes par chacune des instructions. Pour y arriver, vous devrez modifier 
//	la fonction �memReadByte� en lecture et la procedure memStoreByte en ecriture. Par exemple, pour renvoyer
//	systematiquement un nombre aleatoire a partir de la cellule memoire $FE, vous devriez modifier 
//	la fonction �memReadByte� de la facon suivante :

//        Function memReadByte(address:Word):Byte;Begin
//         If address= $fe Then Begin
//          memReadByte := random(256);
//         End
//          Else
//         memReadByte := memory[address];
//        End;
//
////    Pour affecter directement la memoire video a une adresse $E000, vous devriez modifier 
////	la procedure �memStoreByte� de la facon suivante :
//
//        Procedure memStoreByte(address:Word; value:Byte);Begin
//         memory[address] := value;
//         If( (address >= $E000) and (address <= $E0FF) ) Then Begin
//          Mem[$B800:address-$E000] := value;
//         End;
//        End;

//    Un emulateur a souvent tendance a contenir des bogues, mineurs ou majeurs. La raison reside dans 
//	le fait qu'il faut effectuer des operations avec un comportement precis et que si un comportement 
//	ne correspond pas a la specification du fabricant, alors l'emulateur sera donc �boguer �, car 
//	il ne reagira pas exactement comme le modele du fabricant. De plus, certaines instructions 
//	ne sont pas toujours documentees par le fabricant et certains logiciels exploitent le comportement 
//	non documente du fabricant.
//    L'emulateur dans l'exemple a ete ecrit en Pascal, mais il n'y a aucune limitation empechant d'ecrire 
//	un emulateur dans n'importe quel langage de programmation supportant des conditions et des operateurs
//	binaires et logiques.


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
    $50: { BVC }      Rel;
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



