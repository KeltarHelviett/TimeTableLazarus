unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls, UMetaData, UDB, UDirectory, UFilter, sqldb,
  UQueryBuild;

type

   THeaderType = (htTop, htLeft);

   THeader = record
     FValue: string;
     FID: integer;
   end;

   THeaders = array of THeader;

  { TTimeTableForm }

  TTimeTableForm = class(TForm)
    ApplyBtn: TBitBtn;
    ApplyFiltersBtn: TBitBtn;
    AddFilterBtn: TBitBtn;
    ClearFiltersBtn: TBitBtn;
    DrawGrid: TDrawGrid;
    FiltersScrollBox: TScrollBox;
    ScrollBox2: TScrollBox;
    TopHeadersLabel: TLabel;
    LeftHeadersLabel: TLabel;
    LeftHeadersBox: TComboBox;
    TopHeadersBox: TComboBox;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure ApplyFiltersBtnClick(Sender: TObject);
    procedure ClearFiltersBtnClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCellValues: array of array of array of array of string;
    FTopHeaders: THeaders;
    FLeftHeaders: THeaders;
    FHeadersBrushColor: TColor;
    FHeightDelta: integer;
    FCurLeftTable: string;
    FCurTopTable: string;
    FFilterList: TFilterList;
    FFiltersCondition: string;
  public
    function GetHeaders(ATableTag: integer): THeaders;
    procedure GetCellValues;
    procedure OnFilterChange(Sender: TObject);
  end;

var
  TimeTableForm: TTimeTableForm;

implementation

{$R *.lfm}

{ TTimeTableForm }

procedure TTimeTableForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(MetaData.FTables) - 1 do
    begin
      LeftHeadersBox.AddItem(MetaData.FTables[i].FDisplayName, TObject(i));
      TopHeadersBox.AddItem(MetaData.FTables[i].FDisplayName, TObject(i));
    end;
  LeftHeadersBox.ItemIndex := 4;
  TopHeadersBox.ItemIndex := 1;
  FHeadersBrushColor := RGBToColor(153, 51, 255);
  FHeightDelta := Self.Height - DrawGrid.Height;
  FFilterList := TFilterList.Create(FiltersScrollBox, High(MetaData.FTables), @OnFilterChange);
  ApplyBtn.Click;
end;

procedure TTimeTableForm.FormResize(Sender: TObject);
begin
  DrawGrid.Width := Self.Width;
  DrawGrid.Height := Self.Height - FHeightDelta;
end;

procedure TTimeTableForm.ApplyBtnClick(Sender: TObject);
function HeaderTable(ACb: TComboBox): String;
  begin
      Result := MetaData.FTables[Integer(ACb.Items.Objects[ACb.ItemIndex])].FRealName;
  end;
var
  i, j, k, l, m, n: integer;
begin
  FLeftHeaders := GetHeaders(LeftHeadersBox.ItemIndex);
  FTopHeaders := GetHeaders(TopHeadersBox.ItemIndex);
  DrawGrid.RowCount := Length(FLeftHeaders) + 1;
  DrawGrid.ColCount := Length(FTopHeaders) + 1;
  FCurTopTable := HeaderTable(TopHeadersBox);
  FCurLeftTable := HeaderTable(LeftHeadersBox);
  GetCellValues;
end;

procedure TTimeTableForm.ApplyFiltersBtnClick(Sender: TObject);
var
  q: TSQLQuery;
  i: integer;
begin
  FFiltersCondition := PrepareWherePart(High(MetaData.FTables), FFilterList.FFilters, ' ');
  ApplyBtn.Click;
  DrawGrid.Invalidate;
  ApplyFiltersBtn.Enabled := False;
end;

procedure TTimeTableForm.ClearFiltersBtnClick(Sender: TObject);
begin
  FFilterList.ClearFilters;
  FFiltersCondition := '';
  ApplyFiltersBtn.Click;
end;

procedure TTimeTableForm.AddFilterBtnClick(Sender: TObject);
var
  i, j, k: integer;
  f: TFilter;
  p: TPoint;
  FieldsStr: array of string;
  s: string;
begin
  FFilterList.AddFilter;
  ApplyFiltersBtn.Enabled := True;
end;

procedure TTimeTableForm.DrawGridDblClick(Sender: TObject);
var
  t: TTableForm;
  s: string;
  k: Integer;
begin
  if (DrawGrid.Col <> 0) and (DrawGrid.Row <> 0) then
  begin
    t := TTableForm.Create(Self, BuildDrawGridCellQuery(FCurTopTable, FCurLeftTable,
      FTopHeaders[DrawGrid.Col - 1].FID, FLeftHeaders[DrawGrid.Row - 1].FID, False), FFilterList);
    t.Show;
  end;
end;

procedure TTimeTableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j, k, l, m, y, cury: integer;
begin
  if aCol = 0 then
    begin
      DrawGrid.Canvas.Brush.Color := FHeadersBrushColor;
      DrawGrid.Canvas.FillRect(aRect);
      if aRow <> 0 then
        begin
          DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 5,
            FLeftHeaders[aRow - 1].FValue);
          Exit;
        end;
    end;
  if aRow = 0 then
    begin
      DrawGrid.Canvas.Brush.Color := FHeadersBrushColor;
      DrawGrid.Canvas.FillRect(aRect);
      if aCol <> 0 then
        begin
          DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 5,
            FTopHeaders[aCol - 1].FValue);
          Exit;
        end;
    end;
  if (aCol <> 0) and (aRow <> 0) then
    begin
      for i := 0 to High(FCellValues[aCol - 1, aRow - 1]) do
        begin
          for j := 0 to High(FCellValues[aCol - 1, aRow - 1, i]) do
            begin
              aRect.Top += 1;
              DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top,
                FCellValues[aCol - 1, aRow - 1, i, j]);
              aRect.Top += 14;
            end;
          aRect.Top += 45;
        end;
      //ShowMessage(IntToStr(aRect.Top));
      if (Length(FCellValues[aCol - 1, aRow - 1]) <> 0) and
          (aRect.Bottom < aRect.Top - 45) then
        begin
          aRect.Top := aRect.Top - (i + 1) * 45 - (j + 1) * 15;
          DrawGrid.Canvas.Brush.Color := RGBToColor(204, 0, 102);
          DrawGrid.Canvas.Polygon([Point(aRect.Right, aRect.Bottom),
            Point(aRect.Right, aRect.Bottom - 15),
            Point(aRect.Right - 15, aRect.Bottom) ]);
          DrawGrid.Canvas.Brush.Color := clDefault;
        end;
    end;
end;

function TTimeTableForm.GetHeaders(ATableTag: integer): THeaders;
var
  q: TSQLQuery;
  s: string;
  i, id: integer;
  t: TTable;
  sl: TStringList;
begin
  SetLength(Result, 0);
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  q.SQL.Text := BuildSelectPart(ATableTag);
  t := MetaData.FTables[ATableTag];
  q.Open;
  s := '';
  while not(q.EOF) do
    begin
      for i := 0 to High(t.FFields) do
        begin
          if t.FFields[i].FRealName = 'id' then
            begin
              id := q.FieldByName(t.FFields[i].FRealName).AsInteger;
              Continue;
            end;
          s += q.FieldByName(t.FFields[i].FRealName).AsString + ' ';
        end;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].FValue := s;
      Result[High(Result)].FID := id;
      q.Next;
      s := '';
    end;
  q.Free;
end;

procedure TTimeTableForm.GetCellValues;
var
  q: TSQLQuery;
  i, j, k, l, m, n, lr: integer;
  s: String;
begin
  try
  SetLength(FCellValues, 0);
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  SetLength(FCellValues, Length(FTopHeaders));
  for i := 0 to High(FCellValues) do
    SetLength(FCellValues[i], Length(FLeftHeaders));
  for i := 0 to High(FTopHeaders) do
    for j := 0 to High(FLeftHeaders) do
      begin
        q.SQL.Clear;
        s := BuildDrawGridCellQuery(FCurTopTable, FCurLeftTable,
          FTopHeaders[i].FID, FLeftHeaders[j].FID, True);
        s += FFiltersCondition;
        q.SQL.Text := s;
        q.Prepare;
        for k := 0 to High(FFilterList.FFilters) do
          q.Params[k].AsString := FFilterList.FFilters[k].FValue.Text;
        q.UnPrepare;
        q.SQL.SaveToFile('SQL.txt');
        q.Open;
        while not(q.EOF) do
          begin
             SetLength(FCellValues[i, j], Length(FCellValues[i, j]) + 1);
            lr := High(FCellValues[i, j]);
            SetLength(FCellValues[i, j, lr], q.FieldDefs.Count);
            for k := 0 to q.FieldDefs.Count - 1 do
              begin
                if q.FieldDefs.Items[k].DisplayName = 'ID' then Continue;
                FCellValues[i, j, lr, k] :=
                  q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsString;
              end;
            q.Next;
          end;
        q.Close;
      end;
  q.Free;
  except
    on EVariantError do ShowMessage('Poor filter content');
  end;
end;

procedure TTimeTableForm.OnFilterChange(Sender: TObject);
begin
  ApplyFiltersBtn.Enabled := True;
end;

end.

