UNIT Ariphm;

interface

function hexb( dh:byte):string; {перевод байта в Hex}
function hexw( dh:word):string; {перевод двухбайтового слова в Hex}
Function hexlong(dh:longint):string; {перевод четырехбайтового слова в Hex}

implementation                            

function hexb;
var d:byte;
var ch,cl:char;
begin
 d:=dh;
 d:=d shr 4;
 if d<10 then ch:=chr(d+48)
 else if d<16 then ch:=chr(d+55);
 d:=dh;
 d:=d shl 4;d:=d shr 4;
 if d<10 then cl:=chr(d+48)
 else if d<16 then cl:=chr(d+55);
 hexb:=ch+cl
end;

function hexw;
var d:word;
var ch1,cl1,ch0,cl0:char;
begin
 d:=dh;
 d:=d shr 12;
 if d<10 then ch1:=chr(d+48)
 else if d<16 then ch1:=chr(d+55);
 d:=dh;
 d:=d shl 4;d:=d shr 12;
 if d<10 then cl1:=chr(d+48)
 else if d<16 then cl1:=chr(d+55);
 d:=dh;
 d:=d shl 8;d:=d shr 12;
 if d<10 then ch0:=chr(d+48)
 else if d<16 then ch0:=chr(d+55);
 d:=dh;
 d:=d shl 12;d:=d shr 12;
 if d<10 then cl0:=chr(d+48)
 else if d<16 then cl0:=chr(d+55);
 hexw:=ch1+cl1+ch0+cl0;
end;

Function hexlong(dh:longint):string;
var d:word;
var st:string;
begin
  d:=word(dh);
  st:=hexw(d);
  d:=dh div 65536;
 hexlong:=hexw(d)+st;
end;

begin
end.
