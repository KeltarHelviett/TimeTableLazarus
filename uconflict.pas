unit UConflict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, UDB,
  sqldb, UQueryBuild, UMetaData, UFieldCard, UNotification;

type

  TConflictType = (ctGroupInDiffClassesAtSameTime = 0, ctTeacherInDiffClassesAtSameTime = 1, ctSmthELse = 2);

  TConflictRecord = record
    FFields: TStringList;
    FID: Integer;
    FConflictType: TConflictType;
  end;

  TConflictSet = record
    FConflictWith: array of integer;
    FConflictType: TConflictType;
  end;

  TConflictInformation = record
    FID: integer;
    FActInConflicts: array of TConflictSet;
  end;

  TConflictInformationArray = array of TConflictInformation;

  TConflictRecords = array of TConflictRecord;

  TIDS = array of Integer;

  TMultiplicity = array of Integer;

  { TConlfictsForm }

  TConlfictsForm = class(TForm)
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  private
    FRecordsInConflic:TConflictRecords;
    FCFCount: Integer;
    FCFMultiplicity: TMultiplicity;
    FCFInform: TConflictInformationArray;
  public
    procedure CalculateConflic(AParams: array of string; AConflictParams: array of string; AType: TConflictType);
    function CreateConfclitRecord(AStrL: TStringList; AType: TConflictType): TConflictRecord;
    procedure FillTree;
    procedure AddConflictNode(var i: Integer; var j: Integer;
  var k: integer; AType: TConflictType; ARootNode: TTreeNode; ACaption: String;
  P1, P2, P3: Integer);
    function FindeRootNode(ACaption: String):TTreeNode;
    function GetRecordText(index: integer): String;
    function GetRecordText(index: integer; indexes: array of integer): String;
    function ShareConflictIDs(): TIDS;
    function ShareConflictRecords():TConflictRecords;
    function ShareMultiplicity(): TMultiplicity;
    procedure QuickSort(var a: TIDS; ALeft, ARight: integer);
    function GetFieldIndex(AField: String): Integer;
    function GetConflictInformation():TConflictInformationArray;
  end;

var
  ConlfictsForm: TConlfictsForm;

implementation

{$R *.lfm}

{ TConlfictsForm }

procedure TConlfictsForm.FormCreate(Sender: TObject);
begin
  {CalculateConflic(['TEACHER_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID', 'LESSON_ID'],
    ctTeacherInDiffClassesAtSameTime);
  CalculateConflic(['GROUP_ID', 'WEEKDAY_ID', 'LESSON_TIME_ID', 'LESSON_ID'],
    ctGroupInDiffClassesAtSameTime);}
  //FillTree;
end;

procedure TConlfictsForm.TreeViewDblClick(Sender: TObject);
var
  id: Integer;
  c: TCardForm;
begin
  if TreeView.Selected = Nil then exit;
  if TreeView.Selected.Data = Nil then exit;
  id := PInteger(TreeView.Selected.Data)^;
  if not (Notifier.isCardOpened(High(MetaData.FTables), id)) then
    begin;
      c := TCardForm.Create(Self, id, High(MetaData.FTables));
      Notifier.RegisterCard(High(MetaData.FTables), id, @c.BringCardToFront);
      c.Show;
    end;
end;

procedure TConlfictsForm.CalculateConflic(AParams: array of string;
  AConflictParams: array of string; AType: TConflictType);
var
  q: TSQLQuery;
  i, sum, param1, param2, param3, param4: integer;
  s: string;
  sl: TStringList;
  wasUsed: Boolean;
begin
  FCFCount := 0;
  wasUsed := False;
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  s := '';
  s += BuildSelectPart(High(MetaData.FTables));
  s += ' ORDER BY ' + Format('%s, %s, %s, %s', [AParams[0], AParams[1], AParams[2], AParams[3]]);
  q.Close;
  param1 := GetFieldIndex(AParams[0]);
  param2 := GetFieldIndex(AParams[1]);
  param3 := GetFieldIndex(AParams[2]);
  param4 := GetFieldIndex(AParams[3]);
  q.SQL.Clear;
  q.SQL.Text := s;
  q.SQL.SaveToFile('cf.txt');
  q.Open;
  sl := TStringList.Create;
  sl.Clear;
  for i := 0 to q.FieldCount - 1 do
    sl.Append(q.FieldByName(q.FieldDefs.Items[i].DisplayName).AsString);
  sl.SaveToFile('sl.txt');
  q.Next;
  while not(q.EOF) do
    begin
      if (sl[param1] = q.FieldByName(q.FieldDefs.Items[param1].DisplayName).AsString)  and
         (sl[param2] = q.FieldByName(q.FieldDefs.Items[param2].DisplayName).AsString) and
         (sl[param3] = q.FieldByName(q.FieldDefs.Items[param3].DisplayName).AsString)
      then
      begin
        if (sl[param4] <> q.FieldByName(q.FieldDefs.Items[param4].DisplayName).AsString)
        then
         begin
           SetLength(FRecordsInConflic, Length(FRecordsInConflic) + 1);
           FRecordsInConflic[High(FRecordsInConflic)] :=
             CreateConfclitRecord(sl, AType);
           FCFCount += 1;
           for i := 0 to q.FieldCount - 1 do
             sl[i] := q.FieldByName(q.FieldDefs.Items[i].DisplayName).AsString;
           SetLength(FRecordsInConflic, Length(FRecordsInConflic) + 1);
           FRecordsInConflic[High(FRecordsInConflic)] :=
             CreateConfclitRecord(sl, AType);
           FCFCount += 1;
           wasUsed := True;
         end
        else
        begin
          wasUsed := False;
          if FCFCount <> 0 then
            begin
              SetLength(FCFMultiplicity, Length(FCFMultiplicity) + 1);
              FCFMultiplicity[High(FCFMultiplicity)] := FCFCount;
              FCFCount := 0;
            end;
        end;
      end
        else
        begin
          wasUsed := False;

        end;
      if not(wasUsed) then
        for i := 0 to q.FieldCount - 1 do
          sl[i] := q.FieldByName(q.FieldDefs.Items[i].DisplayName).AsString;
      q.Next;
    end;
end;

function TConlfictsForm.CreateConfclitRecord(AStrL: TStringList;
  AType: TConflictType): TConflictRecord;
var
  i: Integer;
begin
  Result.FConflictType := AType;
  Result.FID := StrToInt(AStrL[0]);
  Result.FFields := TStringList.Create;
  Result.FFields.Clear;
  for i := 1 to AStrL.Count - 1 do
    Result.FFields.Append(AStrL[i]);
end;

procedure TConlfictsForm.FillTree;
var
  RN, CurCf, CurCFRecords: TTreeNode;
  i, j, k: Integer;
begin
  RN := TreeView.Items.AddObject(nil, 'Conf', nil);
  i := 0; j := 0; k := 0;
  AddConflictNode(i, j, k, ctTeacherInDiffClassesAtSameTime, RN, 'Teacher in Differen Classes at the same time',
    GetFieldIndex('TEACHER_ID') - 1, GetFieldIndex('WEEKDAY_ID') - 1,
    GetFieldIndex('LESSON_TIME_ID') - 1);
  AddConflictNode(i, j, k, ctGroupInDiffClassesAtSameTime, RN, 'Group in Differen Classes at the same time',
    GetFieldIndex('GROUP_ID') - 1, GetFieldIndex('WEEKDAY_ID') - 1,
    GetFieldIndex('LESSON_TIME_ID') - 1);
end;

procedure TConlfictsForm.AddConflictNode(var i: Integer; var j: Integer;
  var k: integer; AType: TConflictType; ARootNode: TTreeNode; ACaption: String;
  P1, P2, P3: Integer);
var
  CurCF, CurCFRecords: TTreeNode;
begin
  CurCf := TreeView.Items.AddChild(ARootNode, ACaption);
  while (i < Length(FRecordsInConflic)) and
        (FRecordsInConflic[i].FConflictType = AType)
  do
    begin
      CurCFRecords := TreeView.Items.AddChildObject(CurCf, Format('%s, %s, %s',
      [FRecordsInConflic[i].FFields[P1], FRecordsInConflic[i].FFields[P2],
       FRecordsInConflic[i].FFields[P3]
      ]), nil);

      while k < FCFMultiplicity[j] do
        begin
          TreeView.Items.AddChildObject(CurCFRecords, GetRecordText(i, [2, 7, 8]),
            @FRecordsInConflic[i].FID);
          Inc(k);
          i += 1;
        end;
      k := 0;
      inc(j);
    end
end;

function TConlfictsForm.FindeRootNode(ACaption: String): TTreeNode;
var LCount: Integer;
begin
  result := nil;
  LCount := 0;
  while (LCount < TreeView.Items.Count) and (result = nil) do
  begin
    if (TreeView.Items.Item[LCount].Text = ACaption) and (TreeView.Items.Item[LCount].Parent = nil) then
      result := TreeView.Items.Item[LCount];
    inc(LCount);
  end;
end;

function TConlfictsForm.GetRecordText(index: integer): String;
var
  i: Integer;
  sl: TStringList;
begin
  Result := '';
  sl := FRecordsInConflic[index].FFields;
  for i := 0 to sl.Count - 1 do
    Result += sl[i] + ' ';
end;

function TConlfictsForm.GetRecordText(index: integer; indexes: array of integer
  ): String;
var
  i, j: Integer;
  sl: TStringList;
  b: Boolean;
begin
   Result := '';
   sl := FRecordsInConflic[index].FFields;
   for i := 0 to sl.Count - 1 do
     begin
     for j := 0 to High(indexes) do
       if (i = indexes[j]) then
         begin
           b := False;
           Break;
         end
       else
         b := True;
      if b then
        Result += sl[i] + ' ';
     end;
end;

function TConlfictsForm.ShareConflictIDs: TIDS;
var
  i: Integer;
begin
  SetLength(Result, Length(FRecordsInConflic));
  for i := 0 to High(FRecordsInConflic) do
    Result[i] := FRecordsInConflic[i].FID;
  QuickSort(Result, Low(Result), High(Result));
end;

function TConlfictsForm.ShareConflictRecords: TConflictRecords;
begin
  Result := FRecordsInConflic;
end;

function TConlfictsForm.ShareMultiplicity: TMultiplicity;
begin
  Result := FCFMultiplicity;
end;

procedure TConlfictsForm.QuickSort(var a: TIDS; ALeft, ARight: integer);
var
  i, j, x, temp: integer;
begin
  x := a[(ALeft + ARight)div 2];
  i := ALeft;
  j := ARight;
  repeat
    begin
      while(a[i] < x) do
        i := i + 1;
      while(a[j] > x) do
        j := j - 1;
      if(i <= j) then
        begin
          temp := a[i];
          a[i] := a[j];
          a[j] := temp;
          i := i + 1;
          j := j - 1;
        end;
    end;
  until(i > j);
  if (i < ARight) then
    QuickSort(a, i, ARight);
  if (j > ALeft) then
    QuickSort(a, ALeft, j);
end;

function TConlfictsForm.GetFieldIndex(AField: String): Integer;
var
  i, tmp: integer;
  t: TTable;
begin
  Result := 0;
  t := MetaData.FTables[High(MetaData.FTables)];
  for i := 0 to High(t.FFields) do
    begin
      tmp := t.FFields[i].FRefTableInd;
      if tmp = -1 then
        begin
            Result += 1;
            Continue;
        end;
      if t.FFields[i].FRealName = AField then
        Break;
      Result += MetaData.FTables[t.FFields[i].FRefTableInd].FFieldCount - 1;

    end;
end;

function TConlfictsForm.GetConflictInformation: TConflictInformationArray;
var
  i, j: integer;
begin

end;

end.

