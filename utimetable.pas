unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls, UMetaData, UDB, UDirectory, UFilter, sqldb,
  UQueryBuild, LCLIntf, LCLType;

type

   THeaderType = (htTop, htLeft);

   THeader = record
     FValue: string;
     FID: integer;
   end;

   TVeryButton = record
     FRect: TRect;
     FID: integer;
   end;

   TButtonList = record
     FAddBtn: TRect;
     FOpenTableBtn: TRect;
     FDeleteBtns: array of TVeryButton;
     FEditBtns: array of TVeryButton;
     FPermittedToShow: Boolean;
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
    FieldsToShowPanel: TScrollBox;
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
    procedure DrawGridMouseEnter(Sender: TObject);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
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
    FCheckBoxes: array of TCheckBox;
    FCurLeftTableIndex: integer;
    FCurTopTableIndex: integer;
    FButtonLists: array of array of TButtonList;
    FAddBtnIcon: TPortableNetworkGraphic;
    FDeleteBtnIcon: TPortableNetworkGraphic;
    FEditBtnIcon: TPortableNetworkGraphic;
    FOpenTableBtn: TPortableNetworkGraphic;
    FPrevSelectedCol: integer;
    FPrevSelectedRow: integer;
  public
    function GetHeaders(ATableTag: integer): THeaders;
    procedure GetCellValues;
    procedure OnFilterChange(Sender: TObject);
    procedure GetCheckBoxes;
    procedure OnCheckBoxChange(Sender: TObject);
    procedure RecheckBoxes;
    procedure OnHeadersBoxChange(Sender: TObject);
    procedure GetButtonLists;
    procedure LoadIcons;
    procedure OpenTableBtnClick;
    procedure DeleteBtnClick(AIndex: Integer);
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
  LeftHeadersBox.OnChange := @OnHeadersBoxChange;
  TopHeadersBox.OnChange := @OnHeadersBoxChange;
  FCurLeftTableIndex := 4;
  FCurTopTableIndex := 1;
  FPrevSelectedCol := 1;
  FPrevSelectedRow := 1;
  LoadIcons;
  FHeadersBrushColor := clGray;//RGBToColor(153, 51, 255);
  FHeightDelta := Self.Height - DrawGrid.Height;
  FFilterList := TFilterList.Create(FiltersScrollBox, High(MetaData.FTables), @OnFilterChange);
  GetCheckBoxes;
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
begin
  FLeftHeaders := GetHeaders(LeftHeadersBox.ItemIndex);
  FTopHeaders := GetHeaders(TopHeadersBox.ItemIndex);
  DrawGrid.RowCount := Length(FLeftHeaders) + 1;
  DrawGrid.ColCount := Length(FTopHeaders) + 1;
  FCurTopTable := HeaderTable(TopHeadersBox);
  FCurLeftTable := HeaderTable(LeftHeadersBox);
  RecheckBoxes;
  FCurLeftTableIndex := LeftHeadersBox.ItemIndex;
  FCurTopTableIndex := TopHeadersBox.ItemIndex;
  GetButtonLists;
  GetCellValues;
  DrawGrid.Invalidate;
  ApplyBtn.Enabled := False;
end;

procedure TTimeTableForm.ApplyFiltersBtnClick(Sender: TObject);
begin
  FFiltersCondition := PrepareWherePart(High(MetaData.FTables), FFilterList.FFilters, ' ');
  GetButtonLists;
  GetCellValues;
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
begin
  FFilterList.AddFilter;
  ApplyFiltersBtn.Enabled := True;
end;

procedure TTimeTableForm.DrawGridDblClick(Sender: TObject);
begin
end;

procedure TTimeTableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j: integer;
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
      if (Length(FCellValues[aCol - 1, aRow - 1]) <> 0) or
          FButtonLists[aCol - 1][aRow - 1].FPermittedToShow
      then
        begin
          DrawGrid.Canvas.Draw(aRect.Left, aRect.Top, FOpenTableBtn);
          DrawGrid.Canvas.Draw(aRect.Left + 15, aRect.Top, FAddBtnIcon);
          FButtonLists[aCol - 1][aRow - 1].FOpenTableBtn :=
            Rect(aRect.Left, aRect.Top, aRect.Left + 15 , aRect.Top + 15);
          FButtonLists[aCol - 1][aRow - 1].FAddBtn :=
            Rect(aRect.Left + 15, aRect.Top, aRect.Left + 30, aRect.Top + 15);

          aRect.Top += 15;
        end;
      for i := 0 to High(FCellValues[aCol - 1, aRow - 1]) do
        begin
          for j := 0 to High(FCellValues[aCol - 1, aRow - 1, i]) do
            begin
              if FCellValues[aCol - 1, aRow - 1, i, j] = '' then Continue;
              aRect.Top += 1;
              DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top,
                FCellValues[aCol - 1, aRow - 1, i, j]);
              aRect.Top += 14;
            end;
          DrawGrid.Canvas.Draw(aRect.Left, aRect.Top, FDeleteBtnIcon);
          FButtonLists[aCol - 1][aRow - 1].FDeleteBtns[i].FRect :=
            Rect(aRect.Left, aRect.Top, aRect.Left + 15, aRect.Top + 15);
          DrawGrid.Canvas.Draw(aRect.Left + 15, aRect.Top, FEditBtnIcon);
          FButtonLists[aCol - 1][aRow - 1].FEditBtns[i].FRect :=
            Rect(aRect.Left + 16, aRect.Top, aRect.Left + 31, aRect.Top + 15);
          aRect.Top += 45;
        end;
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

procedure TTimeTableForm.DrawGridMouseEnter(Sender: TObject);
begin

end;

procedure TTimeTableForm.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  bl: TButtonList;
begin
  if Button = mbLeft then
    begin
      bl := FButtonLists[DrawGrid.Col - 1][DrawGrid.Row - 1];
      if PtInRect(bl.FOpenTableBtn, Point(X, Y)) then
        begin
          OpenTableBtnClick;
          Exit;
        end;
      for i := 0 to High(bl.FDeleteBtns) do
        if PtInRect(bl.FDeleteBtns[i].FRect, Point(X, Y)) then
          begin
            DeleteBtnClick(i);
            Exit;
          end;
    end;
end;

procedure TTimeTableForm.DrawGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  if Length(FButtonLists) <> 0 then
    begin
      FButtonLists[FPrevSelectedCol - 1][FPrevSelectedRow - 1].FPermittedToShow := False;
      FButtonLists[aCol - 1][aRow - 1].FPermittedToShow := True;
      FPrevSelectedCol := aCol;
      FPrevSelectedRow := aRow;
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
  d, e: TVeryButton;
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
        q.SQL.SaveToFile('q.txt');
        q.Prepare;
        for k := 0 to High(FFilterList.FFilters) do
          q.Params[k].AsString := FFilterList.FFilters[k].FValue.Text;
        q.Open;
        while not(q.EOF) do
          begin
            SetLength(FCellValues[i, j], Length(FCellValues[i, j]) + 1);
            lr := High(FCellValues[i, j]);
            SetLength(FCellValues[i, j, lr], q.FieldDefs.Count);
            SetLength(FButtonLists[i][j].FDeleteBtns,
              Length(FButtonLists[i][j].FDeleteBtns) + 1);
            SetLength(FButtonLists[i][j].FEditBtns,
              Length(FButtonLists[i][j].FEditBtns) + 1);
            for k := 0 to q.FieldDefs.Count - 1 do
              begin
                if q.FieldDefs.Items[k].DisplayName = 'ID' then
                begin
                  d.FID := q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsInteger;
                  e.FID := q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsInteger;
                  Continue;
                end;
                if FCheckBoxes[k - 1].Checked then
                  FCellValues[i, j, lr, k] :=
                    q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsString;
              end;
            with FButtonLists[i][j] do
              begin
                FDeleteBtns[High(FButtonLists[i][j].FDeleteBtns)] := d;
                FEditBtns[High(FButtonLists[i][j].FEditBtns)] := e;
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

procedure TTimeTableForm.GetCheckBoxes;
var
  chb: TCheckBox;
  l: TLabel;
  q: TSQLQuery;
  p: TPoint;
  i, j, k, tindex: integer;
begin
  p.x := 0; p.y := 0;
  k := 0;
    for i := 1 to High(MetaData.FTables[High(MetaData.FTables)].FFields) do
      begin
        tindex := MetaData.FTables[High(MetaData.FTables)].FFields[i].FRefTableInd;
        for j := 1 to High(MetaData.FTables[tindex].FFields) do
          begin
            chb := TCheckBox.Create(FieldsToShowPanel);
            chb.Parent := FieldsToShowPanel;
            chb.Left := p.x;
            chb.Top := p.y;
            chb.Tag := k;
            if (tindex <> FCurTopTableIndex) and (tindex <> FCurLeftTableIndex)
            then
              chb.Checked := True;
            Inc(k);
            chb.OnChange := @OnCheckBoxChange;
            l := TLabel.Create(FieldsToShowPanel);
            l.Parent := FieldsToShowPanel;
            l.Caption := MetaData.FTables[tindex].FFields[j].FDisplayName;
            l.Left := 15;
            l.Top := p.y;
            p.y += 15;
            SetLength(FCheckBoxes, Length(FCheckBoxes) + 1);
            FCheckBoxes[High(FCheckBoxes)] := chb;
          end;
      end;
end;

procedure TTimeTableForm.OnCheckBoxChange(Sender: TObject);
begin
  //ApplyBtn.Click;
  //ApplyBtn.Enabled := True;
  GetButtonLists;
  GetCellValues;
  DrawGrid.Invalidate;
end;

procedure TTimeTableForm.RecheckBoxes;
var
  i: integer;
begin
  {for i := 0 to High(FCheckBoxes) do
    begin
      if (i = TopHeadersBox.ItemIndex) or (i = LeftHeadersBox.ItemIndex) then
        FCheckBoxes[i].Checked := False;
      if (i = FCurLeftTableIndex) or (i = FCurTopTableIndex) then
        FCheckBoxes[i].Checked := True;
    end;}
end;

procedure TTimeTableForm.OnHeadersBoxChange(Sender: TObject);
begin
  ApplyBtn.Enabled := True;
end;

procedure TTimeTableForm.GetButtonLists;
var
  i: integer;
begin
  SetLength(FButtonLists, 0);
  SetLength(FButtonLists, DrawGrid.ColCount - 1);
  for i := 0 to High(FButtonLists) do
    SetLength(FButtonLists[i], DrawGrid.RowCount - 1);
end;

procedure TTimeTableForm.LoadIcons;
begin
  FAddBtnIcon := TPortableNetworkGraphic.Create;
  FDeleteBtnIcon := TPortableNetworkGraphic.Create;
  FEditBtnIcon := TPortableNetworkGraphic.Create;
  FOpenTableBtn := TPortableNetworkGraphic.Create;
  FAddBtnIcon.LoadFromFile('icons\PNG_PLUS.png');
  FDeleteBtnIcon.LoadFromFile('icons\PNG_MINUS.png');
  FEditBtnIcon.LoadFromFile('icons\PNG_EDIT.png');
  FOpenTableBtn.LoadFromFile('icons\PNG_TABLE.png');
end;

procedure TTimeTableForm.OpenTableBtnClick;
var
  t: TTableForm;
begin
  if (DrawGrid.Col <> 0) and (DrawGrid.Row <> 0) then
  begin
    t := TTableForm.Create(Self, BuildDrawGridCellQuery(FCurTopTable, FCurLeftTable,
      FTopHeaders[DrawGrid.Col - 1].FID, FLeftHeaders[DrawGrid.Row - 1].FID, False), FFilterList);
    t.Show;
  end;
end;

procedure TTimeTableForm.DeleteBtnClick(AIndex: Integer);
var
  btn, id: integer;
  s: string;
  q: TSQLQuery;
begin
  btn := MessageDlg('Delete record?', mtCustom, [mbYes, mbNo], 0);
  id := FButtonLists[DrawGrid.Col - 1][DrawGrid.Row - 1].FDeleteBtns[AIndex].FID;
  if btn = mrYes then
    begin
      q := TSQLQuery.Create(Self);
      q.DataBase := DataModule1.IBConnection1;
      s := 'DELETE FROM ' + MetaData.FTables[High(MetaData.FTables)].FRealName;
      s += ' WHERE id = ' + IntToStr(id);
      q.SQL.Text := s;
      q.SQL.SaveToFile('SQL.txt');
      q.ExecSQL;
      DataModule1.SQLTransaction1.Commit;
      GetCellValues;
      DrawGrid.Invalidate;
    end;
end;

end.

