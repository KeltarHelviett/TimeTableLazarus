unit UConflict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, UDB,
  sqldb, UQueryBuild, UMetaData, UFieldCard, UNotification, db;

type

  TConflictType = (ctGroupMultipleLessons, ctGroupMultipleClassrooms,
    ctGroupMultipleTeacher, ctTeacherMultipleClassrooms, ctClassroomOverflow);

  TConflictRecord = record
    FFields: TStringList;
    FID: Integer;
  end;

  TConflictSet = record
    FConflictRecords: array of TConflictRecord;
    FConflictType: TConflictType;
  end;

  TConflictRecords = array of TConflictRecord;

  TConflictSets = array of TConflictSet;

  Integers = array of Integer;

  TMultiplicity = array of Integer;

  { TConlfictsForm }

  TConlfictsForm = class(TForm)
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  private
    FRecordsInConflic:TConflictRecords;
    FConflictSets: TConflictSets;
  public
    procedure FillTree;
    function CreateConfclitRecord(AStrL: TStringList; AType: TConflictType): TConflictRecord;
    procedure RecalculateConficts;
    procedure AddConflictNode(var i: Integer; AType: TConflictType; ARootNode: TTreeNode; ACaption: String);
    function FindeRootNode(ACaption: String):TTreeNode;
    function GetRecordText(index: integer): String;
    function GetRecordText(index: integer; indexes: array of integer): String;
    function GetFieldIndex(AField: String): Integer;
    procedure CalculateConflict(AType: TConflictType; AEQParam, AUnEQParam: array of string);
    procedure CalculateClassroomOverflowConflict;
    function CreateConflictRecordFromQuery(ABegin: Integer; AQuery: TSQLQuery): TConflictRecord;

  end;

var
  ConlfictsForm: TConlfictsForm;

implementation

{$R *.lfm}

{ TConlfictsForm }

procedure TConlfictsForm.FormCreate(Sender: TObject);
begin
  CalculateConflict(ctGroupMultipleLessons,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['LESSON_ID']);
  CalculateConflict(ctGroupMultipleClassrooms,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['CLASSROOM_ID']);
  CalculateConflict(ctGroupMultipleTeacher,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['TEACHER_ID']);
  CalculateConflict(ctTeacherMultipleClassrooms,
    ['TEACHER_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['CLASSROOM_ID']);
  CalculateClassroomOverflowConflict;

  FillTree;
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

function TConlfictsForm.CreateConfclitRecord(AStrL: TStringList;
  AType: TConflictType): TConflictRecord;
var
  i: Integer;
begin
  Result.FID := StrToInt(AStrL[0]);
  Result.FFields := TStringList.Create;
  Result.FFields.Clear;
  for i := 1 to AStrL.Count - 1 do
    Result.FFields.Append(AStrL[i]);
end;

procedure TConlfictsForm.RecalculateConficts;
begin
  CalculateConflict(ctGroupMultipleLessons,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['LESSON_ID']);
  CalculateConflict(ctGroupMultipleClassrooms,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['CLASSROOM_ID']);
  CalculateConflict(ctGroupMultipleTeacher,
    ['GROUP_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['TEACHER_ID']);
  CalculateConflict(ctTeacherMultipleClassrooms,
    ['TEACHER_ID', 'WEEKDAY_ID','LESSON_TIME_ID'],
    ['CLASSROOM_ID']);
end;

procedure TConlfictsForm.FillTree;
var
  RN, CurCf, CurCFRecords: TTreeNode;
  i, j, k: Integer;
begin
  RN := TreeView.Items.AddObject(nil, 'Conflicts', nil);
  i := 0;
  AddConflictNode(i, ctGroupMultipleLessons, RN, 'Group Mult Lessons');
  AddConflictNode(i, ctGroupMultipleClassrooms, RN, 'Group Mult Classrooms');
  AddConflictNode(i, ctGroupMultipleTeacher, RN, 'Group Mult Teachers');
  AddConflictNode(i, ctTeacherMultipleClassrooms, RN, 'Teacher Mult Classrooms');
  AddConflictNode(i, ctClassroomOverflow, RN, 'Classroom Overflow');
end;

procedure TConlfictsForm.AddConflictNode(var i: Integer; AType: TConflictType;
  ARootNode: TTreeNode; ACaption: String);
var
  CurCF, curConflictSet: TTreeNode;
  s: array of array of string;
  recordCaptions: array of string;
  commonInd, ids: Integers;
  conflictSetCaption: string;
  j, k: integer;
begin

  CurCf := TreeView.Items.AddChildObject(ARootNode, ACaption, Nil);
  conflictSetCaption := '';
  while ((i < Length(FConflictSets)) and (FConflictSets[i].FConflictType = AType)) do
    with FConflictSets[i] do
      begin
        SetLength(commonInd, FConflictRecords[0].FFields.Count);
        SetLength(s, Length(FConflictRecords));
        SetLength(recordCaptions, Length(s));
        SetLength(ids, Length(FConflictRecords));
        for j := 0 to High(FConflictRecords) do
          begin
            SetLength(s[j], FConflictRecords[j].FFields.Count);
            for k := 0 to FConflictRecords[j].FFields.Count - 1 do
              begin
                if ((j + 1) < Length(FConflictRecords)) then
                  if (FConflictRecords[j].FFields[k] <> FConflictRecords[j + 1].FFields[k]) then
                    commonInd[k] := -1;
                s[j][k] += FConflictRecords[j].FFields[k] + '  ';
              end;
            ids[j] := FConflictRecords[j].FID;
          end;
        for j := 0 to FConflictRecords[0].FFields.Count - 1 do
         if commonInd[j] <> -1 then
           conflictSetCaption += FConflictRecords[0].FFields[j] + ' ';
        curConflictSet := TreeView.Items.AddChildObject(curCf, conflictSetCaption, TObject(ids));
        for j := 0 to High(s) do
          for k := 0 to High(s[j]) do
            if commonInd[k] = -1 then
              recordCaptions[j] += s[j][k] + ' ';
        j := 0;
        for j := 0 to High(FConflictRecords) do
          TreeView.Items.AddChildObject(curConflictSet, recordCaptions[j], @FConflictRecords[j].FID);
        SetLength(commonInd, 0);
        SetLength(s, 0);
        SetLength(ids, 0);
        SetLength(recordCaptions, 0);
        conflictSetCaption := '';
        inc(i);
     end;
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

procedure TConlfictsForm.CalculateConflict(AType: TConflictType; AEQParam,
  AUnEQParam: array of string);
var
  q, qpred: TSQLQuery;
  i, j, k, bgn, lengthEQ, lengthUnEQ, last, cfstcount: Integer;
  s, tmp, paramList: String;
  f: array of TField;
  isConflict, isCurrentSet: Boolean;
  pred: TConflictRecord;
begin
  cfstcount:= 0;
  paramList := '';
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  tmp := BuildSelectPart(High(MetaData.FTables));
  Delete(tmp, 1, 7);
  for i := 0 to High(AEQParam) do
    paramList += 'TIMETABLE.' + AEQParam[i] + ', ';
  for i := 0 to High(AUnEQParam) do
    paramList += 'TIMETABLE.' + AUnEQParam[i] + ', ';
  Delete(paramList, Length(paramList) - 1, 2);
  s := ' SELECT ';
  s +=  paramList + ', ' + tmp + ' ORDER BY ' + paramList;
  q.SQL.Text := s;
  q.SQL.SaveToFile('newcf.txt');
  q.Open;
  qpred := TSQLQuery.Create(Self);
  qpred.DataBase := DataModule1.IBConnection1;
  qpred.SQL.Text := s;
  qpred.Open;
  lengthEQ := Length(AEQParam);
  lengthUnEQ := Length(AUnEQParam);
  SetLength(f, lengthEQ + lengthUnEQ);
  for i := 0 to High(AEQParam) do
    f[i] := qpred.FieldByName(AEQParam[i]);
  for i := i + 1 to lengthUnEQ - 1 + lengthEQ do
    f[i] := qpred.FieldByName(AUnEQParam[i - lengthEQ]);
  pred := CreateConflictRecordFromQuery(lengthEQ + lengthUnEQ, q);
  isCurrentSet := False;
  q.Next;
  while (not(q.EOF)) do
    begin
      isConflict := True;
      for i := 0 to lengthEQ - 1 do
        if f[i].AsInteger <> q.FieldByName(AEQParam[i]).AsInteger then
           begin
             isCurrentSet := False;
             isConflict := False;
             break;
           end;
      if isConflict then
        for i := i + 1 to lengthUnEQ - 1 + lengthEQ do
          if f[i].AsInteger = q.FieldByName(AUnEQParam[i - lengthEQ]).AsInteger then
            begin
              isConflict := False;
              break;
            end;
      if isConflict then
        if isCurrentSet then
          begin
            with FConflictSets[High(FConflictSets)] do
              begin
                SetLength(FConflictRecords, Length(FConflictRecords) + 1);
                last := High(FConflictRecords);
                FConflictRecords[last] :=
                  CreateConflictRecordFromQuery(lengthEQ + lengthUnEQ, q);
                pred := FConflictRecords[last];
              end;
          end
        else
          begin
            SetLength(FConflictSets, Length(FConflictSets) + 1);
            with FConflictSets[High(FConflictSets)] do
              begin
                SetLength(FConflictRecords, 2);
                FConflictRecords[0] := pred;
                FConflictRecords[1] :=
                  CreateConflictRecordFromQuery(lengthEQ + lengthUnEQ, q);
                pred := FConflictRecords[1];
                FConflictType := AType;
                isCurrentSet := True;
                inc(cfstcount);
              end;
          end;
       qpred.Next;
       for i := 0 to High(AEQParam) do
         f[i] := qpred.FieldByName(AEQParam[i]);
       for i := i + 1 to lengthUnEQ - 1 + lengthEQ do
         f[i] := qpred.FieldByName(AUnEQParam[i - lengthEQ]);
       if not(isConflict) then
         pred := CreateConflictRecordFromQuery(lengthEQ + lengthUnEQ, q);
      q.Next;
    end;
  q.Free;
  qpred.Free;
end;

procedure TConlfictsForm.CalculateClassroomOverflowConflict;
var
  q, qpred: TSQLQuery;
  i, j, k, sum, last, cfCount: Integer;
  s: array of array of string;
  tmp, orderList: string;
  paramList: array[0..2] of string;
  pred: array [0..2] of integer;
  str: string;
  isCurrentSet, isConflict: Boolean;
begin
  orderList := '';
  cfCount := 1;
  isCurrentSet := False;
  q := TSQLQuery.Create(Self);
  q.DataBase := DataModule1.IBConnection1;
  qpred := TSQLQuery.Create(Self);
  qpred.DataBase := DataModule1.IBConnection1;
  tmp := BuildSelectPart(High(MetaData.FTables));
  Delete(tmp, 1, 7);
  paramList[0] := 'LESSON_ID';
  paramList[1] := 'WEEKDAY_ID';
  paramList[2] := 'LESSON_TIME_ID';
  for i := 0 to High(paramList) do
    orderList += 'TIMETABLE.' + paramList[i] + ', ';
  str := 'SELECT ' + orderList + tmp + ' ORDER BY ' + orderList;
  Delete(str, Length(str) - 1, 2);
  q.SQL.Text := str;
  qpred.SQL.Text := str;
  q.SQL.SaveToFile('str.txt');
  q.Open;
  qpred.Open;
  for i := 0 to High(pred) do
    pred[i] := qpred.FieldByName(paramList[i]).AsInteger;
  sum := qpred.FieldByName('STUDENT_NUMBER').AsInteger;
  q.Next;
  while (not(q.EOF)) do
    begin
      isConflict := True;
      for i := 0 to High(pred) do
        if (pred[i] <> q.FieldByName(paramList[i]).AsInteger) then
          begin
            isConflict := False;
            isCurrentSet := False;
            Break;
          end;
      if isConflict then
        begin
          sum += q.FieldByName('STUDENT_NUMBER').AsInteger;
          Inc(cfCount);
        if (isCurrentSet) then
          with FConflictSets[High(FConflictSets)] do
            begin
              SetLength(FConflictRecords, Length(FConflictRecords) + 1);
              last := High(FConflictRecords);
              FConflictRecords[last] :=
                CreateConflictRecordFromQuery(Length(pred), q);
            end
        else
          begin
            SetLength(FConflictSets, Length(FConflictSets) + 1);
            with FConflictSets[High(FConflictSets)] do
              begin
                SetLength(FConflictRecords, 2);
                FConflictRecords[0] :=
                  CreateConflictRecordFromQuery(Length(pred), qpred);
                FConflictRecords[1] :=
                  CreateConflictRecordFromQuery(Length(pred), q);
                FConflictType := ctClassroomOverflow;
                isCurrentSet := True;
              end;
          end;
        end
      else
        begin
          if cfCount > 1 then
            begin
              if (sum < qpred.FieldByName('CAPACITY').AsInteger) then
                SetLength(FConflictSets, Length(FConflictSets) - 1);
              cfCount := 1;
              sum := q.FieldByName('STUDENT_NUMBER').AsInteger;
            end
          else
            begin
              SetLength(FConflictSets, Length(FConflictSets) + 1);
              if (sum > qpred.FieldByName('CAPACITY').AsInteger) then
                with FConflictSets[High(FConflictSets)] do
                  begin
                    SetLength(FConflictRecords, 1);
                    FConflictRecords[0] :=
                      CreateConflictRecordFromQuery(Length(pred), qpred);
                    FConflictType := ctClassroomOverflow;
                  end;
              sum := q.FieldByName('STUDENT_NUMBER').AsInteger;
            end;
        end;
      qpred.Next;
       for i := 0 to High(pred) do
         pred[i] := qpred.FieldByName(paramList[i]).AsInteger;

      q.Next;
    end;
  qpred.Free;
  q.Free;
end;

function TConlfictsForm.CreateConflictRecordFromQuery(ABegin: Integer;
  AQuery: TSQLQuery): TConflictRecord;
var
  i: Integer;
begin
  Result.FID := AQuery.FieldByName(AQuery.FieldDefs.Items[ABegin].DisplayName).AsInteger;
  Result.FFields := TStringList.Create;
  Result.FFields.Clear;
  inc(ABegin);
  for i := ABegin to AQuery.FieldCount - 1 do
    Result.FFields.Append(AQuery.FieldByName(AQuery.FieldDefs.Items[i].DisplayName).AsString);
end;

end.

