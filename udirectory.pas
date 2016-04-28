unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, DBDateTimePicker, Forms, Controls,
  Graphics, Dialogs, DBGrids, ExtCtrls, Buttons, UMetaData, StdCtrls, DbCtrls,
  UQueryBuild, UFilter, UFieldCard, UNotification, UDB;

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
    FOpenCardID: integer;
    Filters: TFilterList;
    FWhereCondition: string;
  public
    constructor Create(AOwner: TComponent; AWhereCondition: string;
      AFilterList: TFilterList);
    constructor Create(AOwner: TComponent; ATag: integer);
    { public declarations }
  end;

var
  TableForm: TTableForm;
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
begin
  Filters.AddFilter;
  ApplyBtn.Enabled := True;
  ClearFiltersBtn.Enabled := True;
end;

procedure TTableForm.AddRecordBtnClick(Sender: TObject);
var
  newForm: TCardForm;
begin
  newForm := TCardForm.Create(TableForm, 0, Self.Tag);
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
  s += FWhereCondition;
  s += PrepareWherePart(Self.Tag, Filters.FFilters, FWhereCondition);
  SQLQuery1.SQL.Text := s;
  SQLQuery1.SQL.SaveToFile('UDSQl.txt');
  SQLQuery1.Prepare;
  for i := 0 to High(Filters.FFilters) do
    begin
      SQLQuery1.Params[i].AsString := Filters.FFilters[i].FValue.Text;
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
  DBGrid1.DataSource.DataSet.First;
  if (FOpenCardID <> -1) then
    while (DBGrid1.DataSource.DataSet.Fields.Fields[0].Value <> FOpenCardID)
          and (not DBGrid1.DataSource.DataSet.EOF) do
      DBGrid1.DataSource.DataSet.Next;
end;

procedure TTableForm.ClearFiltersBtnClick(Sender: TObject);
var
  i: Integer;
begin
  Filters.ClearFilters;
  ShowDefaultTable;
  ApplyBtn.Enabled := False;
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
      DataModule1.SQLTransaction1.Commit;
      ShowDefaultTable;
    end;
end;

procedure TTableForm.EditRecordBrnClick(Sender: TObject);
var
  newForm: TCardForm;
begin
  FOpenCardID := DBGrid1.DataSource.DataSet.Fields.Fields[0].AsInteger;
  if not (Notifier.isCardOpened(Self.Tag, FOpenCardID)) then
    begin;
      newForm := TCardForm.Create(Self, FOpenCardID, Self.Tag);
      Notifier.RegisterCard(Self.Tag, FOpenCardID, @newForm.BringCardToFront);
      newForm.Show;
    end;
end;

procedure TTableForm.FormCreate(Sender: TObject);
begin
  Notifier.Subscribe(ApplyBtn.OnClick);
  FOpenCardID := -1;
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
  SQLQuery1.SQL.Text := BuildSelectPart(Self.Tag) + FWhereCondition;
  SQLQuery1.Open;
  AdjustColumnNames();
  AdjustColumnSize;
end;

procedure TTableForm.OnDeleteBtnUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, tg: integer;
begin

end;

procedure TTableForm.EditOnKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and (ApplyBtn.Enabled) then
    ApplyBtn.Click;
end;

constructor TTableForm.Create(AOwner: TComponent; AWhereCondition: string;
  AFilterList: TFilterList);
begin
  inherited Create(AOwner);
  SQLQuery1.SQL.Text := BuildSelectPart(High(MetaData.FTables)) + AWhereCondition;
  FWhereCondition := AWhereCondition;
  Self.Tag := High(MetaData.FTables);
  SQLQuery1.Open;
  Filters := TFilterList.Create(ScrollBox1, Self.Tag, AFilterList);
  ApplyBtn.Click;
end;

constructor TTableForm.Create(AOwner: TComponent; ATag: integer);
begin
  inherited Create(AOwner);
  Filters := TFilterList.Create(ScrollBox1, ATag, @OnFilterChange);
  Self.Tag := ATag;
end;

end.

