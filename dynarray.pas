{
 /***************************************************************************
                                   DynArray.pas
                                   ------------


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
  |$HeadURL: svn://192.168.2.3:3691/Lazarus/packages/rtlpatch/dynarray.pas $ (Archivadresse)
  |$Id: dynarray.pas 2850 2018-09-17 09:20:37Z andreas $ (eindeutige Dateikennzeichnung) }

unit DynArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDynamicArray }

  TDynamicArray = class(TPersistent)
  private
    FMemory: Pointer;
    FMemorySize: Integer;
    procedure SetMemorySize(AValue: Integer);
  private
    FCount: Integer;
    function GetCapacity: Integer;
    function GetItemPointers(I: Integer): Pointer;
    function GetMemory: Pointer;
    procedure ReadMemory(Reader: TReader);
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetItemPointers(I: Integer; AValue: Pointer);
    procedure WriteMemory(Writer: TWriter);
  protected
    procedure AssignTo(AnObject: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeItem(Index: Integer); virtual; {nullt den Speicher des Elements
      bei Index i, muß überschrieben werden, wenn das Element weiteren
      Speicher verwaltet, der freigegeben werden muß}
    procedure FreeItems; {FreeItem für jedes Element aufrufen}
    function ItemSize: Integer; virtual; abstract;
    property MemorySize: Integer read FMemorySize write SetMemorySize;
  public
    destructor Destroy; override;
    procedure CopyFrom(var x; Size: Integer);
    procedure CopyTo(var x);
    function Equals(AnObject: TObject): Boolean; override;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property ItemPointers[I: Integer]: Pointer read GetItemPointers write SetItemPointers;
    property Memory: Pointer read GetMemory;
  end;

  TDynamicArrayClass = class of TDynamicArray;

  { TInt32Array }

  TInt32Array = class(TDynamicArray)
  private
    function GetValues(I: Integer): int32;
    procedure SetValues(I: Integer; AValue: int32);
  protected
    function ItemSize: Integer; override;
  public
    property Values[I: Integer]: int32 read GetValues write SetValues; default;
  end;

  {TQWordArray}

  TQWordArray = class(TDynamicArray)
  private
    function GetValues(I: Integer): QWord;
    procedure SetValues(I: Integer; AValue: QWord);
  protected
    function ItemSize: Integer; override;
  public
    property Values[I: Integer]: QWord read GetValues write SetValues; default;
  end;

  { TByteBoolArray }

  TByteBoolArray = class(TDynamicArray)
  private
    function GetValues(I: Integer): ByteBool;
    procedure SetValues(I: Integer; AValue: ByteBool);
  protected
    function ItemSize: Integer; override;
  public
    property Values[I: Integer]: ByteBool read GetValues write SetValues;
      default;
  end;

  { TDynamicMatrix }

  TDynamicMatrix = class(TDynamicArray)
  private
    function GetSubArrays(I: Integer): TDynamicArray;
    procedure ReadSubArrays(Reader: TReader);
    procedure SetSubArrays(I: Integer; AValue: TDynamicArray);
    procedure WriteSubArrays(Writer: TWriter);
  protected
    FColCount: Integer;
    procedure AssignTo(AnObject: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeItem(Index: Integer); override; {erwartet, daß das Element ein
      TDynamicArray-Objekt ist und gibt seinen Speicher frei}
    class function ItemClass: TDynamicArrayClass; virtual;
    function ItemSize: Integer; override;
    procedure SetColCount(AValue: Integer); virtual; abstract;
    property RowCount: Integer read FCount write SetCount;
    property ColCount: Integer read FColCount write SetColCount;
  public
    function Equals(AnObject: TObject): Boolean; override;
    property SubArrays[I: Integer]: TDynamicArray read GetSubArrays write SetSubArrays;
      default;
  end;

  TFloatPair = record
    X, Y: Extended;
  end;

  { TFloatPairArray }

  TFloatPairArray = class(TDynamicArray)
  private
    function GetValues(I: Integer): TFloatPair;
    procedure SetValues(I: Integer; AValue: TFloatPair);
  protected
    function ItemSize: Integer; override;
  public
    property Values[I: Integer]: TFloatPair read GetValues write SetValues; default;
  end;

  { TInt32Matrix }

  TInt32Matrix = class(TDynamicMatrix)
  private
    function GetValues(i, j: Integer): int32;
    procedure SetValues(i, j: Integer; AValue: int32);
  protected
    class function ItemClass: TDynamicArrayClass; override;
    procedure SetColCount(AValue: Integer); override;
  public
    property ColCount;
    property RowCount;
    property Values[i, j: Integer]: int32 read GetValues write SetValues; default;
  end;

implementation

{ TFloatPairArray }

function TFloatPairArray.GetValues(I: Integer): TFloatPair;
begin
  Move(ItemPointers[I]^, Result, ItemSize)
end;

procedure TFloatPairArray.SetValues(I: Integer; AValue: TFloatPair);
begin
  Move(AValue, ItemPointers[I]^, ItemSize)
end;

function TFloatPairArray.ItemSize: Integer;
begin
  Result := SizeOf(TFloatPair)
end;

{ TQWordArray }

function TQWordArray.GetValues(I: Integer): QWord;
begin
  Move(ItemPointers[I]^, Result, ItemSize)
end;

procedure TQWordArray.SetValues(I: Integer; AValue: QWord);
begin
  Move(AValue, ItemPointers[I]^, SizeOf(AValue))
end;

function TQWordArray.ItemSize: Integer;
begin
  Result := SizeOf(QWord)
end;

{ TInt32Matrix }

function TInt32Matrix.GetValues(i, j: Integer): int32;
begin
  if i < Count then
    if j < SubArrays[i].Count then begin
      Result := (SubArrays[i] as TInt32Array).Values[j];
      Exit
    end;
  Result := 0
end;

procedure TInt32Matrix.SetValues(i, j: Integer; AValue: int32);
begin
  (SubArrays[i] as TInt32Array).Values[j] := AValue
end;

class function TInt32Matrix.ItemClass: TDynamicArrayClass;
begin
  ItemClass := TInt32Array
end;

procedure TInt32Matrix.SetColCount(AValue: Integer);
var
  i: Integer;
begin
  if FColCount <> AValue then begin
    for i := 0 to RowCount - 1 do (SubArrays[i] as TInt32Array).Count := AValue;
    FColCount := AValue
  end;
end;

{ TDynamicMatrix }

function TDynamicMatrix.GetSubArrays(I: Integer): TDynamicArray;
type
  PDynamicArray = ^TDynamicArray;
var
  x: TDynamicArray;
begin
  x := PDynamicArray(ItemPointers[I])^;
  if x = nil then begin
    x := ItemClass.Create;
    x.Count := FColCount;
    PDynamicArray(ItemPointers[I])^ := x;
  end;
  Result := x;
end;

procedure TDynamicMatrix.ReadSubArrays(Reader: TReader);
var
  i: Integer;
begin
  with Reader do begin
    ReadListBegin;
    Count := ReadInteger;
    for i := 0 to Count - 1 do SubArrays[i].ReadMemory(Reader);
    ReadListEnd;
  end;
end;

procedure TDynamicMatrix.SetSubArrays(I: Integer; AValue: TDynamicArray);
begin
  (GetSubArrays(I) as ItemClass).Assign(AValue)
end;

procedure TDynamicMatrix.WriteSubArrays(Writer: TWriter);
var
  i: Integer;
begin
  with Writer do begin
    WriteListBegin;
    WriteInteger(Count);
    for i := 0 to Count - 1 do SubArrays[i].WriteMemory(Writer);
    WriteListEnd;
  end;
end;

procedure TDynamicMatrix.AssignTo(AnObject: TPersistent);
var
  i: Integer;
begin
  (AnObject as TDynamicMatrix).Count := Count;
  for i := 0 to Count - 1 do
    (AnObject as TDynamicMatrix).SubArrays[i].Assign(SubArrays[i]);
end;

procedure TDynamicMatrix.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SubArrays', @ReadSubArrays, @WriteSubArrays, Count > 0)
end;

procedure TDynamicMatrix.FreeItem(Index: Integer);
begin
  if Index < Count then
    TDynamicArray(ItemPointers[Index]).Free;
  inherited FreeItem(Index);
end;

function TDynamicMatrix.ItemSize: Integer;
begin
  Result := SizeOf(TDynamicArray)
end;

class function TDynamicMatrix.ItemClass: TDynamicArrayClass;
begin
  ItemClass := TDynamicArray
end;

function TDynamicMatrix.Equals(AnObject: TObject): Boolean;
var
  i: Integer;
begin
  Result := Self = AnObject;
  if Result then Exit;
  Result := AnObject.InheritsFrom(TDynamicMatrix);
  if not Result then Exit;
  Result := Count = TDynamicMatrix(AnObject).Count;
  if not Result then Exit;
  for i := 0 to Count - 1 do begin
    Result := SubArrays[i].Equals(TDynamicMatrix(AnObject).SubArrays[i]);
    if not Result then Exit
  end;
end;

{ TByteBoolArray }

function TByteBoolArray.GetValues(I: Integer): ByteBool;
begin
  Move(ItemPointers[I]^, Result, ItemSize)
end;

procedure TByteBoolArray.SetValues(I: Integer; AValue: ByteBool);
begin
  Move(AValue, ItemPointers[I]^, SizeOf(AValue))
end;

function TByteBoolArray.ItemSize: Integer;
begin
  Result := SizeOf(ByteBool)
end;

{ TInt32Array }

function TInt32Array.GetValues(I: Integer): int32;
begin
  Move(ItemPointers[I]^, Result, ItemSize)
end;

procedure TInt32Array.SetValues(I: Integer; AValue: int32);
begin
  Move(AValue, ItemPointers[I]^, SizeOf(AValue))
end;

function TInt32Array.ItemSize: Integer;
begin
  Result := SizeOf(int32)
end;

{ TDynamicArray }

procedure TDynamicArray.SetMemorySize(AValue: Integer);
var
  x: Pointer;
  i, NewCount: Integer;
begin
  if FMemorySize <> AValue then
    if AValue = 0 then begin
      FreeItems;
      FreeMem(FMemory, FMemorySize);
      FMemory := nil;
      FMemorySize := 0
    end
    else if FMemorySize = 0 then begin
      GetMem(FMemory, AValue);
      FillChar(FMemory^, AValue, 0);
      FMemorySize := AValue
    end
    else begin
      NewCount := AValue div ItemSize;
      for i := NewCount to Count - 1 do FreeItem(i);
      GetMem(x, AValue);
      FillChar(FMemory^, AValue, 0);
      CopyTo(x^);
      FreeMem(FMemory, FMemorySize);
      FMemory := x;
      FMemorySize:=AValue
    end
end;

function TDynamicArray.GetItemPointers(I: Integer): Pointer;
begin
  if I < 0 then raise Exception.CreateFmt('%d ist ein ungültiger Index für %s',
    [I, ClassName]);
  if I >= Count then Count := I + 1;
  Result := FMemory + (I * ItemSize);
end;

function TDynamicArray.GetMemory: Pointer;
begin
  Result := ItemPointers[0]
end;

function TDynamicArray.GetCapacity: Integer;
begin
  Result := MemorySize div ItemSize
end;

procedure TDynamicArray.SetCapacity(AValue: Integer);
begin
  MemorySize := AValue * ItemSize;
end;

procedure TDynamicArray.SetCount(AValue: Integer);
begin
  if FCount=AValue then Exit;
  if Capacity < AValue then Capacity := AValue;
  FCount:=AValue;
end;

procedure TDynamicArray.SetItemPointers(I: Integer; AValue: Pointer);
begin
  Move(AValue^, GetItemPointers(I)^, ItemSize)
end;

procedure TDynamicArray.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Memory', @ReadMemory, @WriteMemory, Count > 0)
end;

procedure TDynamicArray.FreeItem(Index: Integer);
begin
  FillChar(ItemPointers[Index]^, ItemSize, 0)
end;

procedure TDynamicArray.FreeItems;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do FreeItem(i)
end;

procedure TDynamicArray.ReadMemory(Reader: TReader);
begin
  with Reader do begin
    ReadListBegin;
    Count := ReadInteger;
    if Count > 0 then
      Read(ItemPointers[0]^, Count * ItemSize);
    ReadListEnd;
  end;
end;

procedure TDynamicArray.WriteMemory(Writer: TWriter);
begin
  with Writer do begin
    WriteListBegin;
    WriteInteger(Count);
    if Count > 0 then
      Write(ItemPointers[0]^, Count * ItemSize);
    WriteListEnd;
  end;
end;

procedure TDynamicArray.AssignTo(AnObject: TPersistent);
begin
  (AnObject as TDynamicArray).MemorySize := MemorySize;
  Move(ItemPointers[0]^, (AnObject as TDynamicArray).ItemPointers[0]^, MemorySize)
end;

destructor TDynamicArray.Destroy;
begin
  MemorySize := 0;
  inherited Destroy;
end;

procedure TDynamicArray.CopyFrom(var x; Size: Integer);
begin
  if MemorySize < Size then MemorySize := Size;
  Move(x, FMemory^, Size)
end;

procedure TDynamicArray.CopyTo(var x);
begin
  if Count > 0 then Move(FMemory^, x, Count * ItemSize)
end;

function TDynamicArray.Equals(AnObject: TObject): Boolean;
var
  i: Integer;
begin
  Result := inherited Equals(AnObject);
  if Result then Exit;
  Result := AnObject.InheritsFrom(TDynamicArray);
  if not Result then Exit;
  for i := 0 to MemorySize - 1 do begin
    Result := PByte(FMemory + i)^ = PByte(TDynamicArray(AnObject).FMemory + i)^;
    if not Result then Exit
  end;
end;

end.

