unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, UMetaData, UDB, UDirectory, UFilter, sqldb, UQueryBuild;

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
    DrawGrid: TDrawGrid;
    LeftHeadersBox: TComboBox;
    TopHeadersBox: TComboBox;
    procedure ApplyBtnClick(Sender: TObject);
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
  public
    function GetHeaders(ATableTag: integer): THeaders;
    procedure GetCellValues;
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
  LeftHeadersBox.ItemIndex := 0;
  TopHeadersBox.ItemIndex := 0;
  FHeadersBrushColor := RGBToColor(153, 51, 255);
  FHeightDelta := Self.Height - DrawGrid.Height;
  ApplyBtn.Click;
end;

procedure TTimeTableForm.FormResize(Sender: TObject);
begin
  DrawGrid.Width := Self.Width;
  DrawGrid.Height := Self.Height - FHeightDelta;
end;

procedure TTimeTableForm.ApplyBtnClick(Sender: TObject);
var
  i, j, k, l, m, n: integer;
begin
  FLeftHeaders := GetHeaders(LeftHeadersBox.ItemIndex);
  FTopHeaders := GetHeaders(TopHeadersBox.ItemIndex);
  DrawGrid.RowCount := Length(FLeftHeaders) + 1;
  DrawGrid.ColCount := Length(FTopHeaders) + 1;
  GetCellValues;
end;

procedure TTimeTableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j, k, l, m: integer;
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
              aRect.Top += 15;
              DrawGrid.Canvas.TextOut(aRect.Left, aRect.Top,
                FCellValues[aCol - 1, aRow - 1, i, j]);
            end;
          aRect.Top += 45;
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
  function HeaderTable(ACb: TComboBox): String;
  begin
      Result := MetaData.FTables[Integer(ACb.Items.Objects[ACb.ItemIndex])].FRealName;
  end;
var
  q: TSQLQuery;
  i, j, k, l, m, n, lr: integer;
  s: String;
  a: array of array of string;
begin
  SetLength(FCellValues, 0);
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  SetLength(FCellValues, Length(FTopHeaders));
  for i := 0 to High(FCellValues) do
    SetLength(FCellValues[i], Length(FLeftHeaders));
  for i := 0 to High(FTopHeaders) do
    for j := 0 to High(FLeftHeaders) do
      begin
        s := BuildSelectPart(High(MetaData.FTables));
        s += Format(
    ' where %s.id = %d and %s.id = %d',
    [
      HeaderTable(TopHeadersBox), FTopHeaders[i].FID,
      HeaderTable(LeftHeadersBox), FLeftHeaders[j].FID
    ]);
        q.SQL.SaveToFile('SQL.txt');
        q.SQL.Text := s;
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
end;

end.

