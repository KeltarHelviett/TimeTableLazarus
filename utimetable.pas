unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls, UMetaData, UDB, UDirectory, UFilter, sqldb,
  UQueryBuild, LCLIntf, LCLType, Menus, UFieldCard, UNotification, UConflict;

type

   THeaderType = (htTop, htLeft);

   THeader = record
     FValue: string;
     FID: integer;
   end;

   TModifyButton = record
     FRect: TRect;
     FID: integer;
     FButtonType: (btOpen, btAdd, btDelete, btEdit, btDrag, btConflict);
   end;

   TButtonList = record
     FButtons: array of TModifyButton;
     FPermittedToShow: Boolean;
   end;

   TConflictCells = array of Boolean;

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
    MFileSaveAs: TMenuItem;
    MViewConflicts: TMenuItem;
    MView: TMenuItem;
    SaveDialogTimetable: TSaveDialog;
    TimetableMenu: TMainMenu;
    MFile: TMenuItem;
    TopHeadersLabel: TLabel;
    LeftHeadersLabel: TLabel;
    LeftHeadersBox: TComboBox;
    TopHeadersBox: TComboBox;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure ApplyFiltersBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClearFiltersBtnClick(Sender: TObject);
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseEnter(Sender: TObject);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DrawGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure MFileSaveAsClick(Sender: TObject);
    procedure MViewConflictsClick(Sender: TObject);
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
    FConflict: TPortableNetworkGraphic;
    FPrevSelectedCol: integer;
    FPrevSelectedRow: integer;
    FDragId: integer;
    FDragCell: TPoint;

    FConflictCells: array of array of Boolean;
    FConflictRecords: TConflictRecords;
    FMultiplicity: TMultiplicity;
    FCurConflictID: Integer;
  public
    function GetHeaders(ATableTag: integer): THeaders;
    procedure GetCellValues;// OLD VERSION. Query for each cell
    procedure OnFilterChange(Sender: TObject);
    procedure GetCheckBoxes;
    procedure OnCheckBoxChange(Sender: TObject);
    procedure RecheckBoxes;
    procedure OnHeadersBoxChange(Sender: TObject);
    procedure GetButtonLists;
    procedure LoadIcons;
    procedure OpenTableBtnClick;
    procedure DeleteBtnClick(AIndex: Integer);
    procedure EditBtnClick(AIndex: Integer);
    procedure ConflictBtnClick(AIndex, X, Y: Integer);
    procedure AddBtnClick;
    procedure GetRecordsInConflict;
    procedure CalculateConflictCells;
    procedure SetSaveDialogFilters;
    procedure SetTableStyle(var AText: string);
    procedure OpenConflict();
    procedure PopUpMenuClick(Sender: TObject);
    procedure GetCellValuesOptim;// NEW VERSION. 1 query for whole tabre
    function GetFieldName(ATableTag: Integer): string;
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
  GetRecordsInConflict;
  Notifier.Subscribe(ApplyBtn.OnClick);
  ApplyBtn.Click;
end;

procedure TTimeTableForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TTimeTableForm.FormResize(Sender: TObject);
begin
  DrawGrid.Width := Self.Width;
  DrawGrid.Height := Self.Height - FHeightDelta;
end;

procedure TTimeTableForm.MFileSaveAsClick(Sender: TObject);
var
  F: TextFile;
  html: string;
  i, j, row, col: integer;
  tempTop, tempLeft: integer;
begin
  if not(SaveDialogTimetable.Execute) then exit;
  tempLeft := LeftHeadersBox.ItemIndex;
  tempTop := TopHeadersBox.ItemIndex;
  TopHeadersBox.ItemIndex := tempLeft;
  LeftHeadersBox.ItemIndex := tempTop;
  ApplyBtn.Click;
  html := '';
  html += '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Timetable</title>' + #13#10;
  SetTableStyle(html);
  html += '</head><body><table width="100%" cellspacing="0" cellpadding="4" border="1">' + #13#10;

  html += #9 + '<tr>' + #13#10 + #9 + #9+ '<th>' + '</th>' + #13#10;
   for i := 0 to High(FLeftHeaders) do
     html += #9 + #9+ '<th>' + FLeftHeaders[i].FValue + '</th>' + #13#10 ;
  html += #9 + '</tr>' + #13#10;

  for row := 0 to High(FCellValues) do
  begin
    html += #9 + '<tr>' + #13#10;
    html += #9 + #9+ '<th>' + FTopHeaders[row].FValue + '</th>' + #13#10;
    for col := 0 to High(FCellValues[row]) do
    begin
      html += #9 + #9+ '<td>';
      for i := 0 to High(FCellValues[row, col]) do
      begin
        html += '<p>';
        for j := 1 to High(FCellValues[row, col, i]) do
        begin
          html += FCellValues[row, col, i, j];
          html+= '<br>';
        end;
        html +='</p>';
      end;
      html += '</td>' + #13#10;
    end;
    html += #9 + '</tr>' + #13#10;
  end;

  html += '</table></body></html>';
  TopHeadersBox.ItemIndex := tempTop;
  LeftHeadersBox.ItemIndex := tempLeft;
  ApplyBtn.Click;
  AssignFile(F, SaveDialogTimetable.FileName);
  Rewrite(F);
  Write(F, html);
  CloseFile(F);
end;

procedure TTimeTableForm.MViewConflictsClick(Sender: TObject);
var
  cf: TConlfictsForm;
begin
  cf := TConlfictsForm.Create(Self);
  cf.Show;
end;

procedure TTimeTableForm.ApplyBtnClick(Sender: TObject);
function HeaderTable(ACb: TComboBox): String;
  begin
      Result := MetaData.FTables[Integer(ACb.Items.Objects[ACb.ItemIndex])].FRealName;
  end;
begin
  FCurTopTable := HeaderTable(TopHeadersBox);
  FCurLeftTable := HeaderTable(LeftHeadersBox);
  FLeftHeaders := GetHeaders(LeftHeadersBox.ItemIndex);
  FTopHeaders := GetHeaders(TopHeadersBox.ItemIndex);
  DrawGrid.RowCount := Length(FLeftHeaders) + 1;
  DrawGrid.ColCount := Length(FTopHeaders) + 1;
  FCurTopTable := HeaderTable(TopHeadersBox);
  FCurLeftTable := HeaderTable(LeftHeadersBox);
  RecheckBoxes;
  GetButtonLists;
  FCurLeftTableIndex := LeftHeadersBox.ItemIndex;
  FCurTopTableIndex := TopHeadersBox.ItemIndex;
  //CalculateConflictCells;
  //GetRecordsInConflict;
  GetCellValuesOptim;
  //GetCellValues;
  DrawGrid.Invalidate;
  ApplyBtn.Enabled := False;
end;

procedure TTimeTableForm.ApplyFiltersBtnClick(Sender: TObject);
begin
  FFiltersCondition := PrepareWherePart(High(MetaData.FTables), FFilterList.FFilters, ' ');
  GetButtonLists;
  GetCellValuesOptim;
  DrawGrid.Invalidate;
  ApplyFiltersBtn.Enabled := False;
end;

procedure TTimeTableForm.Button1Click(Sender: TObject);
var
  F: TextFile;
  html: string;
  i, j, row, col: integer;
  tempTop, tempLeft: integer;
begin
  if not(SaveDialogTimetable.Execute) then exit;
  tempLeft := LeftHeadersBox.ItemIndex;
  tempTop := TopHeadersBox.ItemIndex;
  TopHeadersBox.ItemIndex := tempLeft;
  LeftHeadersBox.ItemIndex := tempTop;
  ApplyBtn.Click;
  html := '';
  html += '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Timetable</title>' + #13#10;
  SetTableStyle(html);
  html += '</head><body><table width="100%" cellspacing="0" cellpadding="4" border="1">' + #13#10;

  html += #9 + '<tr>' + #13#10 + #9 + #9+ '<th>' + '</th>' + #13#10;
   for i := 0 to High(FLeftHeaders) do
     html += #9 + #9+ '<th>' + FLeftHeaders[i].FValue + '</th>' + #13#10 ;
  html += #9 + '</tr>' + #13#10;

  for row := 0 to High(FCellValues) do
  begin
    html += #9 + '<tr>' + #13#10;
    html += #9 + #9+ '<th>' + FTopHeaders[row].FValue + '</th>' + #13#10;
    for col := 0 to High(FCellValues[row]) do
    begin
      html += #9 + #9+ '<td>';
      for i := 0 to High(FCellValues[row, col]) do
      begin
        html += '<p>';
        for j := 1 to High(FCellValues[row, col, i]) do
        begin
          html += FCellValues[row, col, i, j];
          html+= '<br>';
        end;
        html +='</p>';
      end;
      html += '</td>' + #13#10;
    end;
    html += #9 + '</tr>' + #13#10;
  end;

  html += '</table></body></html>';
  TopHeadersBox.ItemIndex := tempTop;
  LeftHeadersBox.ItemIndex := tempLeft;
  ApplyBtn.Click;
  AssignFile(F, 'lol.html');
  Rewrite(F);
  Write(F, html);
  CloseFile(F);
end;

procedure TTimeTableForm.ClearFiltersBtnClick(Sender: TObject);
begin
  FFilterList.ClearFilters;
  FFiltersCondition := '';
  ApplyFiltersBtn.Click;
end;

procedure TTimeTableForm.DrawGridClick(Sender: TObject);
begin

end;

procedure TTimeTableForm.AddFilterBtnClick(Sender: TObject);
begin
  FFilterList.AddFilter;
  ApplyFiltersBtn.Enabled := True;
end;

procedure TTimeTableForm.DrawGridDblClick(Sender: TObject);
begin
end;

procedure TTimeTableForm.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  Cell: TPoint;
  s: string;
  c: TCardForm;
begin
  Cell := DrawGrid.MouseToCell(Point(X, Y));
  if (Cell.x < 1) or (Cell.y < 1) then
    exit;
  if FDragCell.x = -1 then exit;
  c := TCardForm.Create(Self, FDragId, High(MetaData.FTables));
  Notifier.RegisterCard(High(MetaData.FTables), FDragId, @c.BringCardToFront );
  c.GetComboBoxesTextsWithCase(FTopHeaders[Cell.x - 1].FID,FCurTopTableIndex);
  c.GetComboBoxesTextsWithCase(FLeftHeaders[Cell.y - 1].FID, FCurLeftTableIndex);
  c.SaveCloseBtnClick(nil);
end;

procedure TTimeTableForm.DrawGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin

end;

procedure TTimeTableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j, ind: integer;
  r: TRect;
  p1, p2: TPoint;
begin
  try
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

      ind := 0;
      if (Length(FCellValues[aCol - 1, aRow - 1]) <> 0) or
          FButtonLists[aCol - 1][aRow - 1].FPermittedToShow
      then
        begin
          DrawGrid.Canvas.Draw(aRect.Left, aRect.Top, FOpenTableBtn);
          DrawGrid.Canvas.Draw(aRect.Left + 15, aRect.Top, FAddBtnIcon);
          FButtonLists[aCol - 1][aRow - 1].FButtons[0].FRect :=
            Rect(aRect.Left, aRect.Top, aRect.Left + 15 , aRect.Top + 15);
          inc(ind);
          FButtonLists[aCol - 1][aRow - 1].FButtons[1].FRect :=
            Rect(aRect.Left + 15, aRect.Top, aRect.Left + 30, aRect.Top + 15);
          aRect.Top += 15;
          inc(ind);


        end;
      ind := 2;
      {if (Length(FButtonLists[aCol - 1][aRow - 1].FButtons) >= 3) then
        inc(ind);}
      for i := 0 to High(FCellValues[aCol - 1, aRow - 1]) do
        begin
          p1 := Point(aRect.Left, aRect.Top);
          for j := 0 to High(FCellValues[aCol - 1, aRow - 1, i]) do
            begin
              if FCellValues[aCol - 1, aRow - 1, i, j] = '' then Continue;
              aRect.Top += 1;
              DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top,
                FCellValues[aCol - 1, aRow - 1, i, j]);
              aRect.Top += 14;
            end;
          DrawGrid.Canvas.Draw(aRect.Left, aRect.Top, FDeleteBtnIcon);
          FButtonLists[aCol - 1][aRow - 1].FButtons[ind].FRect :=
            Rect(aRect.Left, aRect.Top, aRect.Left + 15, aRect.Top + 15);
          DrawGrid.Canvas.Draw(aRect.Left + 15, aRect.Top, FEditBtnIcon);
          ind += 1;
          FButtonLists[aCol - 1][aRow - 1].FButtons[ind].FRect :=
            Rect(aRect.Left + 16, aRect.Top, aRect.Left + 31, aRect.Top + 15);
          ind += 1;
          {if (BinSearch(FConflictIDS, FButtonLists[aCol - 1][aRow - 1].FButtons[ind - 1].FID)) then
            begin
              DrawGrid.Canvas.Draw(aRect.Left + 30, aRect.Top, FConflict);
              FButtonLists[aCol - 1][aRow - 1].FButtons[ind].FRect :=
                Rect(aRect.Left + 32, aRect.Top, aRect.Left + 43, aRect.Top + 15);
              inc(ind);
            end;}
          p2 := Point(aRect.Right, aRect.Top);
          FButtonLists[aCol - 1][aRow - 1].FButtons[ind].FRect :=
            Rect(p1.x, p1.y, p2.x, p2.y);
          aRect.Top += 35;
          ind += 1;
        end;

      {if ind <> 2 then
        begin
          FButtonLists[aCol - 1][aRow - 1].FButtons[2].FRect :=
            Rect(p1.x, p1.y, p2.x, p2.y);
        end;                             }
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

  except
    on Exception do ;
  end;
end;

procedure TTimeTableForm.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  cell: TPoint;
  buff: TButtonList;
  a, b: integer;
begin
  cell := DrawGrid.MouseToCell(Point(x, y));
  FDragCell.y := -1;
  FDragCell.x := -1;
  try
  a := cell.x - 1;
  b := cell.y - 1;
  if (a < 0) or (b < 0) then
    exit;
  buff := FButtonLists[a, b];
  if Length(buff.FButtons) < 3 then exit;
  for i := 0 to High(buff.FButtons) do
    with buff do
      begin
        if FButtons[i].FButtonType = btDrag then
          if PtInRect(FButtons[i].FRect, Point(X, Y)) then
            begin
              FDragId := FButtons[i].FID;
              FDragCell := Point(cell.x - 1, cell.y - 1);
              Break;
            end;
      end;
  except
    on e: Exception do ;
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
      for i := 0 to High(bl.FButtons) do
        if PtInRect(bl.FButtons[i].FRect, Point(X, Y)) then
          case bl.FButtons[i].FButtonType of
            btAdd:
              AddBtnClick;
            btOpen:
              OpenTableBtnClick;
            btDelete:
              DeleteBtnClick(i);
            btEdit:
              EditBtnClick(i);
            btConflict:
              ConflictBtnClick(i, X, Y);
          end;
    end;
  DrawGrid.Invalidate;
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

procedure TTimeTableForm.DrawGridStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

function TTimeTableForm.GetHeaders(ATableTag: integer): THeaders;
var
  q: TSQLQuery;
  s: string;
  i, id: integer;
  t: TTable;
begin
  SetLength(Result, 0);
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  s := Format('%s ORDER BY %s', [BuildSelectPart(ATableTag), 'ID']);
  q.SQL.Text := s;
  t := MetaData.FTables[ATableTag];
  q.Open;
  s := '';
  while not(q.EOF) do
    begin
      for i := 0 to High(t.FFields) do
        begin
          if t.FFields[i].FRealName = 'ID' then
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
  i, j, k, l, m, n, lr, b: integer;
  s: String;
  d, e: TModifyButton;
  btns: array of TModifyButton;
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
        q.Open;
        SetLength(btns, Length(btns) + 2);
        btns[0].FButtonType := btOpen;
        btns[1].FButtonType := btAdd;
        b := 2;
        SetLength(btns, Length(btns) + 1);
        if not(q.EOF) then
          begin
            btns[b].FButtonType := btDrag;
            Inc(b);
          end;
        while not(q.EOF) do
          begin
            SetLength(FCellValues[i, j], Length(FCellValues[i, j]) + 1);
            lr := High(FCellValues[i, j]);
            SetLength(FCellValues[i, j, lr], q.FieldDefs.Count);
            SetLength(btns, Length(btns) + 2);
            for k := 0 to q.FieldDefs.Count - 1 do
              begin
                if q.FieldDefs.Items[k].DisplayName = 'ID' then
                begin
                  btns[b].FButtonType := btDelete;
                  btns[b].FID := q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsInteger;
                  Inc(b);
                  btns[b].FButtonType := btEdit;
                  btns[b].FID := btns[b - 1].FID;
                  inc(b);
                  {if BinSearch(FConflictIDS, btns[b - 1].FID) then
                    begin
                      SetLength(btns, Length(btns) + 1);
                      btns[b].FButtonType := btConflict;
                      btns[b].FID := btns[b - 1].FID;
                      inc(b);
                    end; }
                  Continue;
                end;
                if FCheckBoxes[k - 1].Checked then
                  FCellValues[i, j, lr, k] :=
                    q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsString;
              end;
            q.Next;
          end;
        q.Close;
        FButtonLists[i][j].FButtons := btns;
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
  GetCellValuesOptim;
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
  SetLength(FButtonLists, DrawGrid.ColCount);
  for i := 0 to High(FButtonLists) do
    SetLength(FButtonLists[i], DrawGrid.RowCount);
end;

procedure TTimeTableForm.LoadIcons;
begin
  FAddBtnIcon := TPortableNetworkGraphic.Create;
  FDeleteBtnIcon := TPortableNetworkGraphic.Create;
  FEditBtnIcon := TPortableNetworkGraphic.Create;
  FOpenTableBtn := TPortableNetworkGraphic.Create;
  FConflict := TPortableNetworkGraphic.Create;
  FAddBtnIcon.LoadFromFile('icons\PNG_PLUS.png');
  FDeleteBtnIcon.LoadFromFile('icons\PNG_MINUS.png');
  FEditBtnIcon.LoadFromFile('icons\PNG_EDIT.png');
  FOpenTableBtn.LoadFromFile('icons\PNG_TABLE.png');
  FConflict.LoadFromFile('icons\PNG_CONFLICT.png');
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
  id := FButtonLists[DrawGrid.Col - 1][DrawGrid.Row - 1].FButtons[AIndex].FID;
  if btn = mrYes then
    begin
      q := TSQLQuery.Create(Self);
      q.DataBase := DataModule1.IBConnection1;
      s := 'DELETE FROM ' + MetaData.FTables[High(MetaData.FTables)].FRealName;
      s += ' WHERE id = ' + IntToStr(id);
      q.SQL.Text := s;
      q.ExecSQL;
      DataModule1.SQLTransaction1.Commit;
      GetCellValuesOptim;
      DrawGrid.Invalidate;
    end;
end;

procedure TTimeTableForm.EditBtnClick(AIndex: Integer);
var
  c: TCardForm;
  id: Integer;
begin
  id := FButtonLists[DrawGrid.Col - 1][DrawGrid.Row - 1].FButtons[AIndex].FID;
  if not (Notifier.isCardOpened(High(MetaData.FTables), id)) then
  begin;
    c := TCardForm.Create(Self, id, High(MetaData.FTables));
    Notifier.RegisterCard(High(MetaData.FTables), id, @c.BringCardToFront);
    c.Show;
  end;
end;

procedure TTimeTableForm.ConflictBtnClick(AIndex, X, Y: Integer);
var
  b: TModifyButton;
  i, j: Integer;
  a: array of TConflictType;
  pup: TPopupMenu;
  m: TMenuItem;
begin
 { b := FButtonLists[DrawGrid.Col - 1][DrawGrid.Row - 1].FButtons[AIndex];
  FCurConflictID := b.FID;
  for i := 0 to High(FConflictRecords) do
    if b.FID = FConflictRecords[i].FID then
      begin
        SetLength(a, Length(a) + 1);
        a[High(a)] := FConflictRecords[i].FConflictType;
      end;
  pup := TPopupMenu.Create(Self);
  for i := 0 to High(a) do
    begin
      m := TMenuItem.Create(Self);
      case a[i] of
        ctTeacherInDiffClassesAtSameTime:
          begin
            m.Caption := 'Teacher';
            m.Tag := 0;
          end;
        ctGroupInDiffClassesAtSameTime:
          begin
            m.Caption := 'Group';
            m.Tag := 1;
          end;

      end;
      m.OnClick := @PopUpMenuClick;
      pup.Items.Add(m);
    end;
   pup.PopUp; }
end;

procedure TTimeTableForm.AddBtnClick;
var
  c: TCardForm;
begin
  c := TCardForm.Create(Self,
    FLeftHeaders[DrawGrid.Row - 1].FID, FTopHeaders[DrawGrid.Col - 1].FID,
    FCurTopTableIndex, FCurLeftTableIndex);
  c.Show;
end;

procedure TTimeTableForm.GetRecordsInConflict;
var
  cff: TConlfictsForm;
begin
  cff := TConlfictsForm.Create(Self);
  {FConflictIDS := cff.ShareConflictIDs();
  FMultiplicity := cff.ShareMultiplicity();
  FConflictRecords := cff.ShareConflictRecords();}
  cff.Free;
end;

procedure TTimeTableForm.CalculateConflictCells;
var
  i: Integer;
begin
  SetLength(FConflictCells, 0);
  SetLength(FConflictCells, Length(FTopHeaders));
  for i := 0 to High(FConflictCells) do
    SetLength(FConflictCells[i], Length(FLeftHeaders));
end;

procedure TTimeTableForm.SetSaveDialogFilters;
begin

end;

procedure TTimeTableForm.SetTableStyle(var AText: string);
begin
  AText+='<style type="text/css">' + #13#10;
  AText+= #9 + #9 +'table { overflow: scroll;} ' + #13#10;
  AText+= #9 + #9 +'td { background: white; vertical-align: top; } ' + #13#10;
  AText+= #9 + #9 +'th { background: Gainsboro; } ' + #13#10;
  AText+='</style>' + #13#10;
end;

procedure TTimeTableForm.OpenConflict;
begin

end;

procedure TTimeTableForm.PopUpMenuClick(Sender: TObject);
var
  i, j, sum, tg: integer;
  isFound: Boolean;
  s: string;
  cftype: TConflictType;
  t: TTableForm;
begin
 { sum := 0; s := '';
  tg := (Sender as TMenuItem).Tag;
  case tg of
    0 : cftype := ctTeacherInDiffClassesAtSameTime;
    1 : cftype := ctGroupInDiffClassesAtSameTime;
  end;
  for i := 0 to High(FMultiplicity) do
    begin

      for j := sum to sum + FMultiplicity[i] do
        if (FConflictRecords[j].FID = FCurConflictID) and (FConflictRecords[j].FConflictType = cftype) then
          begin
            isFound := True;
            Break
          end;
      if (isFound) then
        Break;
      sum += FMultiplicity[i];
    end;
  s += ' WHERE ';
  for j := sum to sum + FMultiplicity[i] - 1 do
    s += ' Timetable.id = '  + IntToStr(FConflictRecords[j].FID) + ' OR ';
  Delete(s, Length(s) - 3, 4);
  t := TTableForm.Create(Self, s, FFilterList);
  t.Show;     }
end;

procedure TTimeTableForm.GetCellValuesOptim;
var
  i, j, k, b, highij: integer;
  btns: array of TModifyButton;
  q: TSQLQuery;
  s, tmp, topFieldName, leftFieldName: string;
begin
  topFieldName := GetFieldName(TopHeadersBox.ItemIndex);
  leftFieldName := GetFieldName(LeftHeadersBox.ItemIndex);
  q := TSQLQuery.Create(Self);
  tmp := BuildSelectPart(High(MetaData.FTables));
  Delete(tmp, 1, 7);
  q.DataBase := DataModule1.IBConnection1;
  q.SQL.Text := Format('SELECT %s, %s, %s ORDER BY %s, %s',
                  [  'TIMETABLE.' + topFieldName,
                     'TIMETABLE.' + leftFieldName,
                     tmp,
                     FCurTopTable + '.ID',
                     FCurLeftTable + '.ID'
                  ]);
  q.SQL.SaveToFile('Op.txt');
  q.Open;
  SetLength(FCellValues, 0);
  SetLength(FCellValues, Length(FTopHeaders));
  for i := 0 to High(FCellValues) do
    SetLength(FCellValues[i], Length(FLeftHeaders));
   i := 0;
   j := 0;
  while ((i <= High(FTopHeaders))) do
    begin
      while ((j <= High(FLeftHeaders))) do
        begin
          SetLength(btns, 2);
          btns[0].FButtonType := btOpen;
          btns[1].FButtonType := btEdit;
          b := 2;
          while (not(q.EOF) and
                (q.FieldByName(topFieldName).AsInteger = FTopHeaders[i].FID) and
                (q.FieldByName(leftFieldName).AsInteger = FLeftHeaders[j].FID)) do
            begin
              SetLength(FCellValues[i][j], Length(FCellValues[i][j]) + 1);
              highij := High(FCellValues[i][j]);
              SetLength(FCellValues[i][j][highij], q.FieldCount - 2);
              for k := 2 to q.FieldCount - 1 do
                begin
                  if q.FieldDefs.Items[k].DisplayName = 'ID' then
                    begin
                      SetLength(btns, Length(btns) + 3);
                      btns[b].FButtonType := btDelete;
                      btns[b].FID := q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsInteger;
                      inc(b);
                      btns[b].FButtonType := btEdit;
                      btns[b].FID := btns[b - 1].FID;
                      inc(b);
                      btns[b].FButtonType := btDrag;
                      btns[b].FID := btns[b - 1].FID;
                      inc(b);
                      Continue;
                    end;
                  if (FCheckBoxes[k - 3].Checked) then
                    FCellValues[i][j][highij][k] :=
                      q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsString;
                end;
              q.Next;
            end;
        FButtonLists[i][j].FButtons := btns;
        inc(j);
        end;
      j := 0;
      inc(i);
    end;
  q.Free;
end;

function TTimeTableForm.GetFieldName(ATableTag: Integer): string;
var
  i: integer;
  t: TTable;
begin
  t := MetaData.FTables[High(MetaData.FTables)];
  for i := 0 to High(t.FFields) do
    if (t.FFields[i].FRefTableInd = ATableTag) then
      Exit(t.FFields[i].FRealName);
  Result := '';
end;

end.
