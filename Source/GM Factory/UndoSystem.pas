unit UndoSystem;
{
 Layered Undo/Redo System
 By Stuart "Stucuk" Carey
 Started on 2017/03/01
}

interface

uses Classes;

type
 TUndoRedoData = Record
  X,Y,
  W,H   : Integer;
  Layer : Word;    
  Data  : Pointer; // Image data
 end;
 PUndoRedoData = ^TUndoRedoData;

 TUndoRedoProc = procedure(Data : PUndoRedoData; UR : Byte); // Application sets the procedure
                                                             // which will take the data and
                                                             // apply it to the image

var
 URSystem : Array [0..1] of TList;
 UndoRedoProc : TUndoRedoProc = Nil;

procedure Undo(Layer : Word);
procedure Redo(Layer : Word);

procedure RemoveUndoRedos(Layer : Word);
procedure IncreaseUndoLayers(Layer : Word);

procedure AddUndo(Data : PUndoRedoData; UR : Byte; IsNew : Boolean = True); // Called when a draw task is done (After X seconds of no further drawing)


function GetUndoRedoCount(A : Byte; Layer : Word) : Integer;

procedure ClearUndoRedo;  // Called when frame changes

implementation

procedure FreeUndoRedo(_Type : Byte; Index : Integer);
var
 Data : PUndoRedoData;
begin
 Data := URSystem[_Type][Index];
 FreeMem(Data.Data);
 FreeMem(Data);
 URSystem[_Type].Delete(Index);
end;

procedure ClearUR(_Type : Byte);
var
 Data : PUndoRedoData;
 X    : Integer;
begin
 for X := 0 to URSystem[_Type].Count-1 do
 begin
  Data := URSystem[_Type][X];
  FreeMem(Data.Data);
  FreeMem(Data);
 end;
 URSystem[_Type].Clear;
end;

procedure ClearUndoRedo;
begin
 ClearUR(0);
 ClearUR(1);
end;

procedure RemoveUndoRedos(Layer : Word);
var
 X,UR : Integer;
begin
 X := 0;
 for UR := 0 to 1 do
 begin
  X := 0;
  While X < URSystem[UR].Count do
  begin
   if PUndoRedoData(URSystem[UR][X]).Layer = Layer then
    FreeUndoRedo(UR,X)
   else
   begin
    if PUndoRedoData(URSystem[UR][X]).Layer > Layer then
    Dec(PUndoRedoData(URSystem[UR][X]).Layer);
    
    Inc(X);
   end;
  end;
 end;
end;

procedure IncreaseUndoLayers(Layer : Word);
var
 X,UR : Integer;
begin
 X := 0;
 for UR := 0 to 1 do
 begin
  X := 0;
  While X < URSystem[UR].Count do
  begin
   if PUndoRedoData(URSystem[UR][X]).Layer >= Layer then
   Inc(PUndoRedoData(URSystem[UR][X]).Layer);
   Inc(X);
  end;
 end;
end;

procedure AddUndo(Data : PUndoRedoData; UR : Byte; IsNew : Boolean = True);
var
 X : Integer;
begin
 URSystem[UR].Add(Data);

 X := 0;
 if (UR = 0) and (IsNew) then
 While X < URSystem[1].Count do
 begin
  if PUndoRedoData(URSystem[1][X]).Layer = Data.Layer then
   FreeUndoRedo(1,X)
  else
   Inc(X);
 end;
end;

function GetUndoRedoCount(A : Byte; Layer : Word) : Integer;
var
 X : Integer;
begin
 Result := 0;

 for X := URSystem[A].Count-1 downto 0 do
 if PUndoRedoData(URSystem[A][X]).Layer = Layer then
 Inc(Result);
end;

function FindLayer(A : Byte; Layer : Word) : Integer;
var
 X : Integer;
begin
 for X := URSystem[A].Count-1 downto 0 do
 if PUndoRedoData(URSystem[A][X]).Layer = Layer then
 begin
  Result := X;
  Exit;
 end;

 Result := -1;
end;

procedure UndoRedo(A,B : Byte; Layer : Word);
var
 Data : PUndoRedoData;
 LID  : Integer;
begin
 LID := FindLayer(A,Layer);
 if LID = -1 then Exit;

 Data := URSystem[A][LID];
 URSystem[A].Delete(LID);

  UndoRedoProc(Data,B);

 FreeMem(Data.Data);
 FreeMem(Data);
end;

procedure Undo(Layer : Word);
begin
 UndoRedo(0,1,Layer);
end;

procedure Redo(Layer : Word);
begin
 UndoRedo(1,0,Layer);
end;         

initialization
 URSystem[0] := TList.Create;
 URSystem[1] := TList.Create;

finalization

end.
