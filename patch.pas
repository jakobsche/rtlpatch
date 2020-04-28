{
 /***************************************************************************
                                     patch.pas
                                     ----------


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus packages by Andreas Jakobsche

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{ |Subversion-Dokumentation
  |------------------------
  |$Date: 2018-12-02 03:32:33 +0100 (So, 02. Dez 2018) $ (letzter Aenderungszeitpunkt)
  |$Revision: 2926 $ (letzter geaenderte Revision)
  |$Author: andreas $ (letzter Autor)
  |$HeadURL: svn://martina:3691/Lazarus/packages/rtlpatch/patch.pas $ (Archivadresse)
  |$Id: patch.pas 2926 2018-12-02 02:32:33Z andreas $ (eindeutige Dateikennzeichnung) }

unit Patch;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

const
{$ifdef Windows}
  DirSeparator = '\';
{$else}
  DirSeparator = '/';
{$endif}

type
  TEnvironment = class(TStringList)
  private
    FCount: Integer;
  public
    constructor Create;
    property Count: Integer read FCount;
  end;

function BuildFileName(Dir, Filename: string): string;

procedure Append(var F: TextFile);

procedure Rename(var f: TextFile; s: string);

procedure DelTree(Dir: string);

function DirectoryExists(x: string): Boolean;

function FindFirst(Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;

function GetTempFileName(const Dir: string; const Prefix: string): string;

function GrayCodeOf(X: QWord): QWord;

function IsExecutable(FileName: string): Boolean;

{$ifdef WINDOWS}

{http://www.freepascal.org/docs-html/rtl/unix/popen.html}

function POpen(var F: text; const Prog: Ansistring; rw: Char): Longint; overload;

function POpen(var F: file; const Prog: Ansistring; rw: Char): Longint; overload;

function PClose(var F: file): Longint; overload;

function PClose(var F: text): Longint; overload;

{$endif}

function Shell(const Command: string): Longint;

function GetMem(Size: PtrUint): Pointer; overload;

implementation

uses Process
{$ifdef Windows}
  {, ShellAPI}
{$else},
  BaseUnix
{$endif};

constructor TEnvironment.Create;
var i: Integer;
begin
  inherited Create;
  FCount := GetEnvironmentVariableCount;
  for i := 1 to FCount do Add(GetEnvironmentString(i))
end;

function BuildFileName(Dir, Filename: string): string;
var
  RL: Integer;
  x, y: string;
begin
  if Dir = '' then Result := FileName
  else begin
    RL := Length(Dir);
    if Dir[RL] = DirectorySeparator then x := LeftStr(Dir, RL - 1)
    else x := Dir;
    if FileName = '' then Result := Dir
    else begin
      if FileName[1] = DirectorySeparator then y := RightStr(FileName, Length(FileName) - 1)
      else y := FileName;
      Result := x + DirectorySeparator + y;
    end
  end;
  RL := Length(Result);
  if Result[RL] = DirectorySeparator then Result := LeftStr(Result, RL - 1)
end;

procedure Append(var F: TextFile);
begin
  try
    System.Append(F)
  except
    on EInOutError do Rewrite(F);
  end;
end;

procedure Rename(var f: TextFile; s: string);
var
  InOutError: EInOutError;
begin
  try System.Rename(f, s);
  except
    on E: EInOutError do
      case E.ErrorCode of
	2 {File not found}:; {eine nicht vorhandene Datei muß nicht umbenannt
          werden, Fehler wird ubergangen}
	else begin
            InOutError := EInOutError.CreateFmt('%s-Meldung: "%s" ErrorCode = %d', [E.ClassName, E.Message, E.ErrorCode]);
	    raise InOutError;
	  end;
      end
  end
end;

procedure DelTree(Dir: string);
var
  F, i: Longint;
  R: Word;
  S: TSearchRec;
  FN: string;
  FL: TStringList;
begin
{$I-}
  RmDir(Dir);
{$I+}
  R := IOResult;
  case R of
    0 {i.O.}:;
    5 {Access denied}: begin
        FL := TStringList.Create;
        try
	  F := FindFirst(BuildFileName(Dir, '*'), faDirectory, S);
   	  while F = 0 do begin
            if (S.Name <> '.') and (S.Name <> '..') then begin
              FN := BuildFileName(Dir, S.Name);
              if faDirectory and S.Attr <> 0 then DelTree(FN)
	      else FL.Add(S.Name);
            end;
            F := FindNext(S)
          end;
	  FindClose(S);
          for i := 0 to FL.Count - 1 do DeleteFile(BuildFileName(Dir, FL[i]));
        finally
          FL.Free
        end;
	RmDir(Dir);
      end;
    else WriteLn('IOResult = ', R, ' beim L�sche3n von ', Dir);
  end
end;

function DirectoryExists(x: string): Boolean;
var
  SR: TSearchRec;
begin
  Result := FindFirst(x, faDirectory, SR) = 0;
  FindClose(SR)
end;

function FindFirst(Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  if Pos('~/', Path) = 1 then Path := BuildFileName(GetEnvironmentVariable('HOME'), Copy(Path, 2, Length(Path)));
  Result := SysUtils.FindFirst(Path, Attr, Rslt)
end;

function GetTempFileName(const Dir: string; const Prefix: string): string;
begin
  ForceDirectories(Dir);
  Result := SysUtils.GetTempFileName(Dir, Prefix)
end;

function GrayCodeOf(X: QWord): QWord;
begin
  Result := X xor (X shr 1) {Wikipedia}
end;

function IsExecutable(FileName: string): Boolean;
{$ifdef windows}
var
  Rslt: TSearchRec;
  R: Longint;
  x: string;
{$endif}
begin
{$ifdef Windows}
  x := LowerCase(ExtractFileExt(FileName));
  Result := (x = '.exe') or (x = '.com') or (x = '.pif') or (x = '.bat') or (x = '.msi');
{$else}
  Result := fpAccess(@FileName[1], X_OK) = 0
{$endif}
end;

{$ifdef WINDOWS}
{ Die folgenden Funktionen sind in der Unit Unix deklariert, die nur fuer
  unixoide Systeme verfuegbar sind. Die Implementierung fuer Windows koennte mit
  TProcess erfolgen }

var
  Proc: TProcess;

function POpen(var F: TextFile; const Prog: Ansistring; rw: Char): Longint;
begin

end;

function POpen(var F: file; const Prog: Ansistring; rw: Char): Longint;
const
  BufSize = 1024;
var
  Buffer: array[0 .. BufSize - 1] of Byte;
  Count: Integer;
begin
  Proc := TProcess.Create(nil);
  with Proc do begin
    CommandLine := Prog;
    Options := [];
    if rw in ['R', 'W', 'r', 'w'] then Options := Options + [poUsePipes];
    Execute;
    while Running do begin
      Sleep(10);
      case rw of
        'R', 'r': begin
            Count := Output.NumBytesAvailable;
            while Count > 0 do begin
              if Count < BufSize then
                BlockWrite(F, Buffer, Output.Read(Buffer, Count))
              else
                BlockWrite(F, Buffer, Output.Read(Buffer, BufSize));
              Count := Output.NumBytesAvailable
            end;
          end;
        'W', 'w': begin
            BlockRead(F, Buffer, BufSize, Count);
            while Count > 0 do begin
              Input.Write(Buffer, Count);
              BlockRead(F, Buffer, BufSize, Count)
            end;
          end;
      end;
    end;
  end;
end;

function PClose(var F: file): Longint;
begin
  CloseFile(F)
end;

function PClose(var F: text): Longint;
begin
  CloseFile(F)
end;

{$endif}

function Shell(const Command: string): Longint;
var P: TProcess;
begin
  P := TProcess.Create(nil);
  try
    P.CommandLine := Command;
    P.Options := P.Options + [poWaitOnExit];
    P.Execute;
    Result := P.ExitStatus
  finally
    P.Free
  end
end;

function GetMem(Size: PtrUint): Pointer;
var
  Backup: Boolean;
begin
  Backup := ReturnNilIfGrowHeapFails;
  ReturnNilIfGrowHeapFails := True; {GetMem ergibt im Fehlerfall einen nil-Zeiger}
  Result := System.GetMem(Size);
  if Result = nil then raise EOutOfMemory.Create('GetMem konnte keinen Speicher zuweisen.');
  ReturnNilIfGrowHeapFails := Backup
end;

end.
