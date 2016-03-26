unit UDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, DBDateTimePicker, Forms, Controls,
  Graphics, Dialogs, DBGrids, ExtCtrls, Buttons, UMetaData, StdCtrls, UQueryBuild,
  UFilter;

type

  { TTableForm }

  TTableForm = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ControlPanel: TPanel;
    AddFilterBtn: TSpeedButton;
    ClearFiltersBtn: TSpeedButton;
    ApplyBtn: TSpeedButton;
    ScrollBox1: TScrollBox;
    SQLQuery1: TSQLQuery;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure AdjustColumnNames(var ATable: TTable);
    procedure AdjustColumnSize;
    procedure ApplyBtnClick(Sender: TObject);
    procedure ClearFiltersBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnFilterChange(Sender: TObject);
    procedure ShowDefaultTable;
    procedure OnDeleteBtnUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TTableForm.AdjustColumnNames(var ATable: TTable);
var
  i: integer;
begin
  for i := 0 to DBGrid1.Columns.Count - 1 do
    begin
      DBGrid1.Columns[i].Title.Caption := ATable.FFields[i].FDisplayName;
    end;
end;

procedure TTableForm.AddFilterBtnClick(Sender: TObject);
var
  i, j, k: integer;
  f: TFilter;
  p: TPoint;
  FieldsStr: array of string;
  s: string;
  sl: TStringList;
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
  sl := TStringList.Create;
  sl.Clear;
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
                    sl.Add(FieldsStr[High(FieldsStr)]);
                    //ShowMessage(MetaData.FTables[j].FFields[k].FDisplayName);
                  end;

              end;
            end;
        end
      else
        begin
          SetLength(FieldsStr, Length(FieldsStr) + 1 );
          FieldsStr[High(FieldsStr)] := MetaData.FTables[Self.Tag].FFields[i].FDisplayName;
          sl.Add(MetaData.FTables[Self.Tag].FFields[i].FDisplayName);
          //ShowMessage(MetaData.FTables[Self.Tag].FFields[i].FDisplayName);
        end;
    end;
  sl.SaveToFile('f.txt');
  f := TFilter.Create(p, ScrollBox1, FieldsStr);
  f.FFields.OnChange := @OnFilterChange;
  f.FValue.OnChange := @OnFilterChange;
  f.FOperations.OnChange := @OnFilterChange;
  f.FDeleteBtn.OnMouseUp := @OnDeleteBtnUp;
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := f;
end;

procedure TTableForm.AdjustColumnSize;
var
  i: integer;
begin
  for i := 0 to DBGrid1.Columns.Count - 1 do
    begin
      DBGrid1.Columns[i].Width := 200;
    end;
end;

procedure TTableForm.ApplyBtnClick(Sender: TObject);
var
  i, j: integer;
  s: string;
begin
  ShowMessage(BuildSelectPart(self.Tag));
  try
  SQLQuery1.Close;
  SQLQuery1.SQL.Clear;
  s := BuildSelectPart(Self.Tag);
  s += ' WHERE ';
  for i := 0 to High(Filters) do
    begin
      if (i > 0) then
         s +=' AND ';
      for j := 0 to High(MetaData.FTables[Self.Tag].FFields) do
        begin
          if (MetaData.FTables[Self.Tag].FFields[j].FDisplayName =
              Filters[i].FFields.Text)
          then
          begin
            s += ' ' + MetaData.FTables[Self.Tag].FFields[j].FRealName;
            s += ' ' + Filters[i].FOperations.Text + ':p' + IntToStr(i);
            Break;
          end;
        end;
    end;
  SQLQuery1.SQL.Text := s;
  SQLQuery1.Prepare;
  for i := 0 to High(Filters) do
    begin
      SQLQuery1.Params[i].AsString := Filters[i].FValue.Text;
    end;
  SQLQuery1.Open;
  AdjustColumnNames(MetaData.FTables[Self.Tag]);
  AdjustColumnSize;
  ApplyBtn.Enabled := False;
  except
  on EVariantError do
    begin
      ShowMessage('Poor filter content');
      ShowDefaultTable;
    end;
  end;
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

procedure TTableForm.FormCreate(Sender: TObject);
begin

end;

procedure TTableForm.OnFilterChange(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
end;

procedure TTableForm.ShowDefaultTable;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := BuildSelectPart(Self.Tag);
  SQLQuery1.Open;
 // AdjustColumnNames(MetaData.FTables[Self.Tag]);
  AdjustColumnSize;
end;

procedure TTableForm.OnDeleteBtnUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  for i := (Sender as TBitBtn).Tag to High(Filters) - 1 do
    begin
      Filters[i].FDeleteBtn.Tag := Filters[i + 1].FDeleteBtn.Tag;
      Filters[i].FFields.ItemIndex := Filters[i + 1].FFields.ItemIndex;
      Filters[i].FOperations.ItemIndex := Filters[i + 1].FOperations.ItemIndex;
      Filters[i].FValue.Text := Filters[i + 1].FValue.Text;
    end;
  Filters[High(Filters)].Free;
  SetLength(Filters, Length(Filters) - 1);
  ApplyBtn.Enabled := True;
end;

end.

