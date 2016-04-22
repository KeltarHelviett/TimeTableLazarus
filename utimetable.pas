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

  { TTimeTableForm }

  TTimeTableForm = class(TForm)
    ApplyBtn: TBitBtn;
    LeftHeadersBox: TComboBox;
    TopHeadersBox: TComboBox;
    DrawGrid: TDrawGrid;
    procedure ApplyBtnClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCellValues: array of array of array of array of array of string;
    FTopHeaders: array of THeader;
    FLeftHeaders: array of THeader;
  public
    procedure GetHeaders(ATableTag: integer; AHeaderType: THeaderType);
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
end;

procedure TTimeTableForm.ApplyBtnClick(Sender: TObject);
var
  i, j, k, l, m, n: integer;
begin
  n := 0;
  DrawGrid.Columns.Clear;
  SetLength(FTopHeaders, 0);
  SetLength(FLeftHeaders, 0);
  GetHeaders(High(MetaData.FTables), htTop);
  GetHeaders(High(MetaData.FTables), htLeft);
  DrawGrid.ColCount := Length(FTopHeaders) + 1;
  DrawGrid.RowCount := Length(FLeftHeaders) + 1;
  GetCellValues;
  {for i := 0 to High(FCellValues) do
    for j := 0 to High(FCellValues[i]) do
      for k := 0 to High(FCellValues[i, j]) do
        for l := 0 to High(FCellValues[i, j, k]) do
          for m := 0 to High(FCellValues[i, j, k, l]) do
            inc(n);}
  DrawGrid.Repaint;
end;

procedure TTimeTableForm.DrawGridDblClick(Sender: TObject);

  function HeaderTable(ACb: TComboBox): String;
  begin
    Result := MetaData.FTables[Integer(ACb.Items.Objects[ACb.ItemIndex])].FRealName;
  end;

var
  t: TTableForm;
begin
  if (DrawGrid.Row = 0) or (DrawGrid.Col = 0) then exit;
  t := TTableForm.Create(Self, Format(
    ' where %s.id = %d and %s.id = %d',
    [
      HeaderTable(TopHeadersBox), FTopHeaders[DrawGrid.Row - 1].FID,
      HeaderTable(LeftHeadersBox), FLeftHeaders[DrawGrid.Col - 1].FID
    ]));
  t.Show;
end;

procedure TTimeTableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j, k, l: integer;
  q: TSQLQuery;
  s: string;
begin
  if (aCol = 0) then
    begin
      DrawGrid.RowCount := Length(FLeftHeaders) + 1;
      DrawGrid.Canvas.Brush.Color := clGray;
      DrawGrid.Canvas.FillRect(aRect);
      if (aRow <> 0) then
        begin
          DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 5, FLeftHeaders[aRow - 1].FValue);
          exit;
        end;
    end;
  if (aRow = 0) then
    begin
      DrawGrid.ColCount := Length(FTopHeaders) + 1;
      DrawGrid.Canvas.Brush.Color := clGray;
      DrawGrid.Canvas.FillRect(aRect);
      if (aCol <> 0) then
        begin
          DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 5, FTopHeaders[aCol - 1].FValue);
          exit;
        end;
    end;
  if (aRow <> 0) and (aCol <> 0) then
    for i := 0 to High(FCellValues[aCol - 1, aRow - 1 ]) do
      for j := 0 to High(FCellValues[aCol - 1, aRow - 1, i]) do
        begin
        for k := 0 to High(FCellValues[aCol - 1, aRow - 1, i, j]) do
          begin
            aRect.Top += 15;
            DrawGrid.Canvas.TextOut(
              aRect.Left, aRect.Top,
              FCellValues[aCol - 1, aRow - 1, i, j, k])
          end;
        aRect.Top += 45;
        end;
end;

procedure TTimeTableForm.FormResize(Sender: TObject);
begin
  DrawGrid.Width := Self.Width;
  DrawGrid.Height := Self.Height;
end;

procedure TTimeTableForm.GetHeaders(ATableTag: integer; AHeaderType: THeaderType
  );
var
  q: TSQLQuery;
  s: string;
  i, id: integer;
  t: TTable;
  sl: TStringList;
begin
  s := '';
  sl := TStringList.Create;
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  case AHeaderType of
    htLeft:
      begin
        q.SQL.Text := BuildSelectPart(LeftHeadersBox.ItemIndex);
        t := MetaData.FTables[LeftHeadersBox.ItemIndex];
      end;
    htTop:
      begin
        q.SQL.Text := BuildSelectPart(TopHeadersBox.ItemIndex);
        t := MetaData.FTables[TopHeadersBox.ItemIndex];
      end;
  end;
  q.Open;
  ShowMessage(q.SQL.Text);
  sl.Clear;
  repeat
    for i := 0 to High(t.FFields) do
      begin
        if t.FFields[i].FRealName = 'id' then
          begin
            id := q.FieldByName(t.FFields[i].FRealName).AsInteger;
            Continue;
          end;
        s += q.FieldByName(t.FFields[i].FRealName).AsString + ' ';
      end;
    if AHeaderType = htTop then
      begin
        SetLength(FTopHeaders, Length(FTopHeaders) + 1);
        FTopHeaders[High(FTopHeaders)].FValue := s;
        FTopHeaders[High(FTopHeaders)].FID := id;
        sl.Add(s);
      end
    else
      begin
        SetLength(FLeftHeaders, Length(FLeftHeaders) + 1);
        FLeftHeaders[High(FLeftHeaders)].FValue := s;
        FLeftHeaders[High(FLeftHeaders)].FID := id;
        sl.Add(s);
      end;
      q.Next;
      s := '';
  until q.EOF;
  q.Close;
  q.Free;
  sl.SaveToFile('SL.txt');
  sl.Free;
end;

procedure TTimeTableForm.GetCellValues;

function HeaderTable(ACb: TComboBox): String;
  begin
    Result := MetaData.FTables[Integer(ACb.Items.Objects[ACb.ItemIndex])].FRealName;
  end;

var
  q: TSQLQuery;
  i, j, k, l, m, n: integer;
  s: String;
  a: array of array of string;
begin
  n := 0;
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
      HeaderTable(TopHeadersBox), FTopHeaders[DrawGrid.Row - 1].FID,
      HeaderTable(LeftHeadersBox), FLeftHeaders[DrawGrid.Col - 1].FID
    ]);
        q.SQL.Text := s;
        q.SQL.SaveToFile('ROFEL.txt');
        q.Open;
        while not(q.EOF) do
          begin
            SetLength(a, Length(a) + 1);
            SetLength(a[High(a)], q.FieldDefs.Count);
            for k := 0 to q.FieldDefs.Count - 1 do
              begin
                if q.FieldDefs.Items[k].DisplayName = 'ID' then Continue;

                a[High(a), k] := q.FieldByName(q.FieldDefs.Items[k].DisplayName).AsString;
                //ShowMessage(a[High(a), k]);
              end;
            inc(n);
            q.Next;
          end;
        q.Close;
        SetLength(FCellValues[i, j], Length(FCellValues[i, j]) + 1);
        FCellValues[i, j, High(FCellValues[i, j])] := a;
        //for m := 0 to High(a) do
          //SetLength(a[m], 0);
        SetLength(a, 0);
      end;
  ShowMessage(IntToStr(n));
end;

end.

