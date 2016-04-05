unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, DBDateTimePicker, Forms, Controls,
  Graphics, Dialogs, DBGrids, ExtCtrls, Buttons, UMetaData, StdCtrls, DbCtrls,
  UQueryBuild, UFilter, UFieldCard, UNotification;

type

  { TTableForm }

  TTableForm = class(TForm)
    AddRecordBtn: TBitBtn;
    EditRecordBrn: TBitBtn;
    DeleteRecordBtn: TBitBtn;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ControlPanel: TPanel;
    AddFilterBtn: TSpeedButton;
    ClearFiltersBtn: TSpeedButton;
    ApplyBtn: TSpeedButton;
    ScrollBox1: TScrollBox;
    SQLQuery1: TSQLQuery;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure AddRecordBtnClick(Sender: TObject);
    procedure AdjustColumnNames;
    procedure AdjustColumnSize;
    procedure ApplyBtnClick(Sender: TObject);
    procedure ClearFiltersBtnClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure DeleteRecordBtnClick(Sender: TObject);
    procedure EditRecordBrnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnFilterChange(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure ShowDefaultTable;
    procedure OnDeleteBtnUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditOnKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TableForm: TTableForm;
  Filters: array of TFilter;

implementation

{$R *.lfm}

{ TTableForm }

procedure TTableForm.AdjustColumnNames;
var
  i, j, k, index, num: integer;
begin
  k := 0;
  for i := 0 to High(MetaData.FTables[Self.Tag].FFields) do
    begin
      if MetaData.FTables[Self.Tag].FFields[i].FRefTableName = '' then
        begin
          DBGrid1.Columns[k].Title.Caption :=
            MetaData.FTables[Self.Tag].FFields[i].FDisplayName;
          Inc(k);
        end
      else
        begin
          index := MetaData.FTables[Self.Tag].FFields[i].FRefTableInd;
          for j := 1 to High(MetaData.FTables[index].FFields) do
            begin
              DBGrid1.Columns[k].Title.Caption :=
                MetaData.FTables[index].FFields[j].FDisplayName;
              Inc(k);
            end;
        end;
    end;
end;

procedure TTableForm.AddFilterBtnClick(Sender: TObject);
var
  i, j, k: integer;
  f: TFilter;
  p: TPoint;
  FieldsStr: array of string;
  s: string;
begin
  if Length(Filters) > 0 then
    begin
      p.x := 0;
      p.y := Filters[High(Filters)].FFields.Top + 40;
    end
  else
    begin
      p.x := 0;
      p.y := 0;
    end;
  s := BuildSelectPart(Self.Tag);
  for i := 0 to High(MetaData.FTables[Self.Tag].FFields) do
    begin
      if MetaData.FTables[Self.Tag].FFields[i].FRefTableName <> '' then
        begin
          for j := 0 to High(MetaData.FTables) do
            begin
              if j = Self.Tag then Continue;
              if MetaData.FTables[i].FRealName =
                 MetaData.FTables[Self.Tag].FFields[i].FRefTableName
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
          FieldsStr[High(FieldsStr)] := MetaData.FTables[Self.Tag].FFields[i].FDisplayName;
        end;
    end;
  SetLength(Filters, Length(Filters) + 1);
  f := TFilter.Create(p, ScrollBox1, FieldsStr, High(Filters));
  f.FFields.OnChange := @OnFilterChange;
  f.FValue.OnChange := @OnFilterChange;
  f.FOperations.OnChange := @OnFilterChange;
  f.FDeleteBtn.OnMouseUp := @OnDeleteBtnUp;
  f.FValue.OnKeyPress := @EditOnKeyPress;
  Filters[High(Filters)] := f;
  ApplyBtn.Enabled := True;
  ClearFiltersBtn.Enabled := True;
end;

procedure TTableForm.AddRecordBtnClick(Sender: TObject);
var
  newForm: TCardForm;
begin
  newForm := TCardForm.Create(TableForm, 0, Self.Tag);
  Self.Enabled := False;
  newForm.Show;
end;


procedure TTableForm.AdjustColumnSize;
var
  i: integer;
begin
  DBGrid1.Columns[0].Width := 20;
  for i := 1 to DBGrid1.Columns.Count - 1 do
    begin
      DBGrid1.Columns[i].Width := 180;
    end;
end;

procedure TTableForm.ApplyBtnClick(Sender: TObject);
var
  i, j: integer;
  s: string;
begin
  try
  SQLQuery1.Close;
  SQLQuery1.SQL.Clear;
  s := BuildSelectPart(Self.Tag);
  s += PrepareWherePart(Self.Tag, Filters);
  SQLQuery1.SQL.Text := s;
  SQLQuery1.Prepare;
  for i := 0 to High(Filters) do
    begin
      SQLQuery1.Params[i].AsString := Filters[i].FValue.Text;
    end;
  SQLQuery1.Open;
  AdjustColumnNames();
  AdjustColumnSize;
  ApplyBtn.Enabled := False;
  except
  on EVariantError do
    begin
      ShowMessage('Poor filter content');
      ShowDefaultTable;
    end;
  end;
  Self.Enabled := True;
end;

procedure TTableForm.ClearFiltersBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(Filters) do
    begin
      Filters[i].Free;
    end;
  ShowDefaultTable;
  ApplyBtn.Enabled := False;
  SetLength(Filters, 0);
end;

procedure TTableForm.DBGrid1DblClick(Sender: TObject);
begin
  EditRecordBrn.Click;
end;

procedure TTableForm.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType
  );
begin
  if (Button = nbInsert) then
    ShowMessage('Hello world!');
end;

procedure TTableForm.DeleteRecordBtnClick(Sender: TObject);
var
  btn, ind: integer;
  s: string;
begin
  ind := DBGrid1.DataSource.DataSet.Fields.Fields[0].AsInteger;
  btn := MessageDlg('Delete record?', mtCustom, [mbYes, mbNo], 0);
  if btn = mrYes then
    begin
      SQLQuery1.Close;
      s := 'DELETE FROM ' + MetaData.FTables[Self.Tag].FRealName;
      s += ' WHERE id = ' + IntToStr(ind);
      SQLQuery1.SQL.Text := s;
      SQLQuery1.ExecSQL;
      ShowDefaultTable;
    end;
end;

procedure TTableForm.EditRecordBrnClick(Sender: TObject);
var
  newForm: TCardForm;
  ind: integer;
begin
  ind := DBGrid1.DataSource.DataSet.Fields.Fields[0].AsInteger;
  newForm := TCardForm.Create(Self, ind, Self.Tag);
  Self.Enabled := False;
  newForm.Show;
end;

procedure TTableForm.FormCreate(Sender: TObject);
begin
  Notifier.Subscribe(ApplyBtn.OnClick);
end;

procedure TTableForm.OnFilterChange(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
  ClearFiltersBtn.Enabled := True;
end;

procedure TTableForm.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TTableForm.ShowDefaultTable;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := BuildSelectPart(Self.Tag);
  SQLQuery1.Open;
  AdjustColumnNames();
  AdjustColumnSize;
end;

procedure TTableForm.OnDeleteBtnUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, tg: integer;
begin
  tg := (Sender as TBitBtn).Tag;
  for i := tg to High(Filters) - 1 do
    begin
      Filters[i].FFields.ItemIndex := Filters[i + 1].FFields.ItemIndex;
      Filters[i].FOperations.ItemIndex := Filters[i + 1].FOperations.ItemIndex;
      Filters[i].FValue.Text := Filters[i + 1].FValue.Text;
    end;
  Filters[High(Filters)].Free;
  SetLength(Filters, Length(Filters) - 1);
  ApplyBtn.Enabled := True;
end;

procedure TTableForm.EditOnKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and (ApplyBtn.Enabled) then
    ApplyBtn.Click;
end;

end.

