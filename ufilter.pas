unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Buttons;
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

implementation

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
 FFields.ItemIndex := 0;
 APosition.x += FFields.Width + 10;
 FOperations := TComboBox.Create(AParent);
 FOperations.Left := APosition.x;
 FOperations.Top := APosition.y;
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
 FDeleteBtn.Tag := ;
end;

destructor TFilter.Free;
begin
  FFields.Free;
  FValue.Free;
  FOperations.Free;
  FDeleteBtn.Free;
end;

end.

