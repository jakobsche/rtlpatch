{
 /***************************************************************************
                                      OP.pas
                                      ------


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus packages by Andreas Jakobsche

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{ |Subversion-Dokumentation
  |------------------------
  |$Date: 2018-09-17 11:20:37 +0200 (Mo, 17 Sep 2018) $ (letzter Aenderungszeitpunkt)
  |$Revision: 2850 $ (letzter geaenderte Revision)
  |$Author: andreas $ (letzter Autor)
  |$HeadURL: svn://192.168.2.3:3691/Lazarus/packages/rtlpatch/op.pas $ (Archivadresse)
  |$Id: op.pas 2850 2018-09-17 09:20:37Z andreas $ (eindeutige Dateikennzeichnung) }

unit OP;

{$mode objfpc}{$H+}

interface

uses SysUtils;

const Test = 0; {nur zum Subversion-Test eingefuegt (Löschen)}

type
  TCharSet = set of Char;

const
  Space: TCharSet = [#9, ' ']; { enthï¿½lt Zeichen, die als Leerraum zu
    interpretieren sind, kann bei Bedarf geï¿½ndert werden. }

function Parse(x, Separator: string; out Right: string): string; overload;{ gibt den
  Teil von x vor der Zeichenkette Separator zurück. Wenn Separator nicht in
  x enthalten ist, wird x vollständig zurückgegeben. Der Teil von x, der übrig-
  bleibt, wird in Right zurückgegeben. Der Rückgabewert und Right werden ggf.
  um führenden und abschließenden Leerraum (Zeichen in Space) verkürzt. }

function Parse(x: string; Separator: TCharSet; var Right: string): string; overload;

procedure ParseStringList(x, Separator: string; var List: array of string);
{ Zerlegt eine durch den String Separator getrennte Aufzï¿½hling, die in x
  ï¿½bergeben wird, in ein Array of string, das in List zurï¿½ckgegeben wird. List
  muï¿½ eine Variable ausreichender Grï¿½ï¿½e sein. Ist List zu klein, wird nur ein
  passendes Anfangsstï¿½ck von x zerlegt. }

function StringValue(x: PString): string; { ergibt immer einen gï¿½ltigen String,
  wenn x vom Typ PString oder kompatibel ist. x=nil ergibt ''. Die Wertzuweisung
  zu x kï¿½nnte z.B. mit AssignStr erfolgen. }

function SetStringValue(var x: PString; S: string): PString;

function Trimm(x: string): string; { entfernt fï¿½hrenden und abschlieï¿½enden Leer-
  raum (Zeichen in Space) aus x und gibt die so verï¿½nderte Zeichenkette zurï¿½ck }

implementation

function Parse(x, Separator: string; out Right: string): string;
var
  P: Byte;
begin
  x := Trimm(x);
  P := Pos(Separator, x);
  if P > 0 then begin
    Parse := Trimm(Copy(x, 1, P - 1));
    Right := Trimm(Copy(x, P + Length(Separator), Length(x)))
  end
  else begin
    Parse := Trimm(x);
    Right := ''
  end
end;

function Parse(x: string; Separator: TCharSet; var Right: string): string;
var
  L, P, Q: Longint;
  i: Char;
begin
  L := Length(x);
  P := L + 1;
  for i := #0 to #255 do
    if i in Separator then begin
      Q := Pos(i, x);
      if Q < P then P := Q
    end;
  Result := Copy(x, 1, P - 1);
  for Q := P + 1 to L do if not (x[Q] in Separator) then Break;
  Right := Copy(x, Q, L)
end;

procedure ParseStringList(x, Separator: string; var List: array of string);
var
  i, n: Integer;
begin
  n := High(List);
  for i := Low(List) to n do List[i] := Parse(x, Separator, x)
end;

function StringValue(x: PString): string;
begin
  if Assigned(x) then StringValue := x^
  else StringValue := ''
end;

function SetStringValue(var x: PString; S: string): PString;
begin
  if x <> nil then DisposeStr(x);
  x := NewStr(S);
  SetStringValue := x;
end;

function Trimm(x: string): string;
var
  i, j, n: Byte;
begin
  n := Length(x);
  for i := 1 to n do
    if not (x[i] in Space) then Break;
  for j := n downto i do
    if not (x[j] in Space) then Break;
  Trimm := Copy(x, i, j + 1 - i);
end;

end.
