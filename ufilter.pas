unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Buttons, UMetaData;
const
  Operations: array[0..6] of string = (' < ', ' > ', ' <> ', ' = ', ' <= ', ' >= ', ' LIKE ');
type

  { TFilter }

  TFilter = class
    constructor Create(APosition: TPoint; AParent: TWinControl;
      AFields: array of string; ATag: integer);
    destructor Free;
    public
      FFields: TComboBox;
      FOperations: TComboBox;
      FValue: TEdit;
      FDeleteBtn: TBitBtn;
  end;

  { TFilterList }

  TFilterList = class
    FFilters: array of TFilter;
    procedure DeleteFilter(Index: integer);
    procedure ClearFilters;
    procedure AddFilter(AParent: TWinControl; ATableTag: integer);
    procedure OnDeleteBtnUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  end;



implementation
uses
  UQueryBuild;
{ TFilterList }

procedure TFilterList.DeleteFilter(Index: integer);
begin

end;

procedure TFilterList.ClearFilters;
var
  i: integer;
begin
  for i := 0 to High(FFilters) do
    begin
      FFilters[i].Free;
    end;
  SetLength(FFilters, 0);
end;

procedure TFilterList.AddFilter(AParent: TWinControl; ATableTag: integer);
var
  i, j, k: integer;
  f: TFilter;
  p: TPoint;
  FieldsStr: array of string;
  s: string;
begin
  SetLength(FieldsStr, 0);
  if Length(FFilters) > 0 then
    begin
      p.x := 0;
      p.y := FFilters[High(FFilters)].FFields.Top + 40;
    end
  else
    begin
      p.x := 0;
      p.y := 0;
    end;
  s := BuildSelectPart(ATableTag);
  for i := 0 to High(MetaData.FTables[ATableTag].FFields) do
    begin
      if MetaData.FTables[ATableTag].FFields[i].FRefTableName <> '' then
        begin
          for j := 0 to High(MetaData.FTables) do
            begin
              if j = ATableTag then Continue;
              if MetaData.FTables[i].FRealName =
                 MetaData.FTables[ATableTag].FFields[i].FRefTableName
              then
              begin
                for k := 1 to High(MetaData.FTables[j].FFields) do
                  begin
                    SetLength(FieldsStr, Length(FieldsStr) + 1);
                    FieldsStr[High(FieldsStr)] := MetaData.FTables[j].FFields[k].FDisplayName;
                  end;

              end;
            end;
        end
      else
        begin
          SetLength(FieldsStr, Length(FieldsStr) + 1 );
          FieldsStr[High(FieldsStr)] := MetaData.FTables[ATableTag].FFields[i].FDisplayName;
        end;
    end;
  SetLength(FFilters, Length(FFilters) + 1);
  f := TFilter.Create(p, AParent, FieldsStr, High(FFilters));
  f.FDeleteBtn.OnMouseUp := @OnDeleteBtnUp;
  FFilters[High(FFilters)] := f;
end;

procedure TFilterList.OnDeleteBtnUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, tg: integer;
begin
  tg := (Sender as TBitBtn).Tag;
  for i := tg to High(FFilters) - 1 do
    begin
      FFilters[i].FFields.ItemIndex := FFilters[i + 1].FFields.ItemIndex;
      FFilters[i].FOperations.ItemIndex := FFilters[i + 1].FOperations.ItemIndex;
      FFilters[i].FValue.Text := FFilters[i + 1].FValue.Text;
    end;
  FFilters[High(FFilters)].Free;
  SetLength(FFilters, Length(FFilters) - 1);
end;



{ TFilter }

constructor TFilter.Create(APosition: TPoint; AParent: TWinControl;
  AFields: array of string; ATag: integer);
var
  i: integer;
begin
 FFields := TComboBox.Create(AParent);
 FFields.Left := APosition.x;
 FFields.Top := APosition.y;
 FFields.Items.AddStrings(AFields);
 FFields.Width := 160;
 FFields.Parent := AParent;
 FFields.Style := csDropDownList;
 FFields.ItemIndex := 0;
 APosition.x += FFields.Width + 10;
 FOperations := TComboBox.Create(AParent);
 FOperations.Left := APosition.x;
 FOperations.Top := APosition.y;
 FOperations.Style := csDropDownList;
 FOperations.Parent := AParent;
 for i := 0 to High(Operations) do
   begin
     FOperations.AddItem(Operations[i], nil);
   end;
 FOperations.ItemIndex := 0;
 APosition.x += FOperations.Width + 10;
 FValue := TEdit.Create(AParent);
 FValue.Left := APosition.x;
 FValue.Top := APosition.y;
 FValue.Parent := AParent;
 FValue.Width := 160;
 APosition.x += FValue.Width + 10;
 FDeleteBtn := TBitBtn.Create(AParent);
 FDeleteBtn.Height :=  FValue.Height;
 FDeleteBtn.Width := 100;
 FDeleteBtn.Left := APosition.x;
 FDeleteBtn.Top := APosition.y;
 FDeleteBtn.Caption := 'Delete';
 FDeleteBtn.Parent := AParent;
 FDeleteBtn.Tag := ATag;
end;

destructor TFilter.Free;
begin
  FFields.Free;
  FValue.Free;
  FOperations.Free;
  FDeleteBtn.Free;
end;

end.

