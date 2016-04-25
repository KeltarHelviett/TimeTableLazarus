unit UFieldCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbCtrls, Buttons, StdCtrls, DBGrids, UMetaData, UQueryBuild, UDB, strutils,
  UNotification;

type

  TAssociativeComboBox = record
    FComboBox: TComboBox;
    FIndexes: array of integer;
  end;

  { TCardForm }

  TCardForm = class(TForm)
    CardDataSource: TDataSource;
    SaveCloseBtn: TBitBtn;
    SaveBtn: TBitBtn;
    CloseBtn: TBitBtn;
    CardSQLQuery: TSQLQuery;
    procedure CloseBtnClick(Sender: TObject);
    procedure CreateAddCard;
    procedure FillComboBox(var AComboBox: TAssociativeComboBox; ATableInd, AFieldInd: integer);
    constructor Create(AOwner: TComponent; Aid: integer; ATag: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SaveCloseBtnClick(Sender: TObject);
    procedure SetAddMode;
    function GetCardRecordQuery():string;
    procedure GetEditsTexts();
    procedure GetComboBoxesTexts();

  private
    FLabels: array of TLabel;
    FEdits: array of TEdit;
    FComboBoxes: array of TAssociativeComboBox;
    FCardMode: (cmAdd, cmEdit);
  public
    procedure CreateArrayOfIds;
    procedure BringCardToFront(Sender: TObject);
    { public declarations }
  end;

var
  CardForm: TCardForm;

  id: integer;
implementation

{$R *.lfm}

{ TCardForm }

procedure TCardForm.CreateAddCard;
var
  i, j, ind: integer;
  l: TLabel;
  e: TEdit;
  c: TAssociativeComboBox;
  p: TPoint;
begin
  p.x := 10;
  p.y := 0;
  for i := 1 to High(MetaData.FTables[Self.Tag].FFields) do
    begin
      if MetaData.FTables[Self.Tag].FFields[i].FRefTableName = '' then
        begin
          l := TLabel.Create(Self);
          l.Left := p.x;
          l.Top := p.y;
          l.Parent := Self;
          l.Caption := MetaData.FTables[Self.Tag].FFields[i].FDisplayName;
          l.AdjustSize;
          e := TEdit.Create(Self);
          e.Parent := Self;
          e.Left := l.Width + 100;
          e.Top := p.y;
          e.Width := 200;
          p.y += 30;
          SetLength(FLabels, Length(FLabels) + 1);
          FLabels[High(FLabels)] := l;
          SetLength(FEdits, Length(FEdits) + 1);
          FEdits[High(FEdits)] := e;
        end
      else
        begin
          ind := MetaData.FTables[Self.Tag].FFields[i].FRefTableInd;
          for j := 1 to High(MetaData.FTables[ind].FFields) do
            begin
              if not(MetaData.FTables[ind].FFields[j].FPermittedToShow)
              then Continue;
              l := TLabel.Create(Self);
              l.Left := p.x;
              l.Top := p.y;
              l.Parent := Self;
              l.Caption := MetaData.FTables[ind].FFields[j].FDisplayName;
              l.AdjustSize;
              SetLength(FLabels, Length(FLabels) + 1);
              FLabels[High(FLabels)] := l;
              with c do
              begin
                FComboBox := TComboBox.Create(Self);
                FComboBox.Parent := Self;
                FComboBox.Left := l.Width + 100;
                FComboBox.Top := p.y;
                FComboBox.Style := csDropDownList;
                FillComboBox(c, ind, j);
                FComboBox.ItemIndex := 0;
              end;
              p.y += 30;
              SetLength(FComboBoxes, Length(FComboBoxes) + 1);
              FComboBoxes[High(FComboBoxes)] := c;
            end;
        end;
    end;
  if (FCardMode = cmEdit) then
    begin
      GetEditsTexts();
      GetComboBoxesTexts();
    end;
end;

procedure TCardForm.CloseBtnClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TCardForm.FillComboBox(var AComboBox: TAssociativeComboBox;
  ATableInd, AFieldInd: integer);
var
  i: integer;
  s: string;
begin
  CardSQLQuery.Close;
  s := 'SELECT id, ' + MetaData.FTables[ATableInd].FFields[AFieldInd].FRealName;
  s += ' FROM ' + MetaData.FTables[ATableInd].FRealName;
  CardSQLQuery.SQL.Text := s;
  CardSQLQuery.Open;
  i := 0;
  while not CardSQLQuery.EOF do
    begin
      SetLength(AComboBox.FIndexes, Length(AComboBox.FIndexes) + 1);
      AComboBox.FIndexes[i] := CardSQLQuery.Fields[0].AsInteger;
      AComboBox.FComboBox.AddItem(
        CardSQLQuery.FieldByName(CardSQLQuery.FieldDefs.Items[1].DisplayName).Value, nil);
      CardSQLQuery.Next;
      Inc(i);
    end;
end;

constructor TCardForm.Create(AOwner: TComponent; Aid: integer; ATag: integer);
var
  s: string;
begin
  inherited Create(AOwner);
  CardSQLQuery.Close;
  Self.Tag := ATag;
  id := Aid;
  CardSQLQuery.SQL.Text := GetCardRecordQuery();
  if Aid = 0 then
    SetAddMode
  else
    FCardMode := cmEdit;
    CreateAddCard;
end;

procedure TCardForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: integer;
begin
  for i := 0 to High(FLabels) do
    FLabels[i].Free;
  SetLength(FLabels, 0);
  for i := 0 to High(FEdits) do
    FEdits[i].Free;
  SetLength(FEdits, 0);
  for i := 0 to High(FComboBoxes) do
    FComboBoxes[i].FComboBox.Free;
  SetLength(FComboBoxes, 0);
  if FCardMode = cmEdit then
    Notifier.CloseCard(Self.Tag, id);
  Notifier.Update;
end;

procedure TCardForm.FormDeactivate(Sender: TObject);
begin
end;

procedure TCardForm.SaveBtnClick(Sender: TObject);
var
  i, j, k, ind: integer;
  s: string;
begin
  CardSQLQuery.Close;
  j := 0; k := 0;
  if FCardMode = cmAdd then
    CardSQLQuery.SQL.Text := PrepareInsertPart(Self.Tag)
  else
    CardSQLQuery.SQL.Text := PrepareUpdatePart(Self.Tag, id);
  CardSQLQuery.SQL.SaveToFile('SQL.txt');
  CardSQLQuery.Prepare;
  for i := 1 to High(MetaData.FTables[Self.Tag].FFields) do
    begin
      if MetaData.FTables[Self.Tag].FFields[i].FRefTableName = '' then
        begin
          CardSQLQuery.Params[i - 1].AsString := FEdits[j].Text;
          Inc(j);
        end
      else
        begin
          if k = Length(FComboBoxes) then Continue;
          CardSQLQuery.Params[i - 1].AsInteger :=
            FComboBoxes[k].FIndexes[FComboBoxes[k].FComboBox.ItemIndex];
          Inc(k);
        end;
    end;
  CardSQLQuery.ExecSQL;
  DataModule1.SQLTransaction1.Commit;
  Notifier.Update;
end;

procedure TCardForm.SaveCloseBtnClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  SaveBtn.Click;
  CloseBtn.Click;
end;

procedure TCardForm.SetAddMode;
begin
  FCardMode := cmAdd;
  SaveCloseBtn.Caption := 'Add and close';
  SaveBtn.Caption := 'Add';
end;

procedure TCardForm.GetEditsTexts;
var
  s: string;
  i: integer;
begin
  CardSQLQuery.Close;
  CardSQLQuery.SQL.Text := GetCardRecordQuery();
  CardSQLQuery.Open;
  for i := 0 to High(FEdits) do
    begin
      FEdits[i].Text := CardSQLQuery.FieldByName(CardSQLQuery.FieldDefs.Items[i].DisplayName).Value;
    end;
end;

procedure TCardForm.GetComboBoxesTexts;
var
  i, j, k: integer;
begin
  j := 0; i := 0;
  CardSQLQuery.Close;
  CardSQLQuery.SQL.Text := GetCardRecordQuery();
  CardSQLQuery.Open;
  while i <= High(FComboBoxes) do
    begin
      FComboBoxes[i].FComboBox.Text := CardSQLQuery.FieldByName(CardSQLQuery.FieldDefs.Items[j].DisplayName).Value;
      Inc(i);
      Inc(j);
    end;
end;

procedure TCardForm.CreateArrayOfIds;
begin

end;

procedure TCardForm.BringCardToFront(Sender: TObject);
begin
  Self.Show;
end;

function TCardForm.GetCardRecordQuery: string;
var
  i, j: integer;
  t, rt: TTable;
begin
  Result := DeleteFieldFromQuery('id', BuildSelectPart(Self.Tag));
  t := MetaData.FTables[Self.Tag];
  for i := 1 to High(t.FFields) do
    if t.FFields[i].FRefTableName <> '' then
      begin
        rt := MetaData.FTables[t.FFields[i].FRefTableInd];
        for j := 1 to High(rt.FFields) do
          if not rt.FFields[j].FPermittedToShow then
            Result := DeleteFieldFromQuery(rt.FFields[j].FRealName, Result);
      end;
  Result += ' WHERE ' + t.FRealName + '.id = ' + IntToStr(id);
end;

end.

