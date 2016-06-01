unit UMetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTableField = record
    FRealName: string;
    FDisplayName: string;
    FRefTableName: string;
    FRefFieldName: string;
    FRefTableInd: integer;
    FPermittedToShow: Boolean;
  end;

  TTable = record
    FRealName: string;
    FDisplayName: string;
    FFields: array of TTableField;
    FFieldCount: Integer;
  end;

  { TMetaData }

  TMetaData = class
    procedure AddTable(ARealTableName, ADisplayTableName: string);
    procedure AddField(var ATable: TTable; ARealFieldName, ADisplayFieldName,
      ARefTableName, ARefFieldName: string; ARefTableInd: integer);
    procedure AddField(var ATable: TTable; ARealFieldName, ADisplayFieldName: string;
      APermittedToShow: Boolean);
    public
      FTables: array of TTable;
  end;

var
  MetaData: TMetaData;

implementation

{ TMetaData }

procedure TMetaData.AddTable(ARealTableName, ADisplayTableName: string);
begin
  SetLength(FTables, Length(FTables) + 1);
  FTables[High(FTables)].FRealName := ARealTableName;
  FTables[High(FTables)].FDisplayName := ADisplayTableName;
  FTables[High(FTables)].FFieldCount := 0;
end;

procedure TMetaData.AddField(var ATable: TTable; ARealFieldName,
  ADisplayFieldName, ARefTableName, ARefFieldName: string; ARefTableInd: integer
  );
begin
  SetLength(ATable.FFields, Length(ATable.FFields) + 1);
  ATable.FFieldCount += 1;
  with ATable.FFields[High(ATable.FFields)] do
    begin
      FRealName := ARealFieldName;
      FDisplayName := ADisplayFieldName;
      FRefTableName := ARefTableName;
      FRefFieldName := ARefFieldName;
      FRefTableInd := ARefTableInd;
      FPermittedToShow := True;
    end;
end;

procedure TMetaData.AddField(var ATable: TTable; ARealFieldName,
  ADisplayFieldName: string; APermittedToShow: Boolean);
begin
  SetLength(ATable.FFields, Length(ATable.FFields) + 1);
  ATable.FFieldCount += 1;
  with ATable.FFields[High(ATable.FFields)] do
    begin
      FRealName := ARealFieldName;
      FDisplayName := ADisplayFieldName;
      FRefTableName := '';
      FRefFieldName := '';
      FRefTableInd := -1;
      FPermittedToShow := APermittedToShow;
    end;
end;

initialization
  MetaData := TMetaData.Create;
  with MetaData do
    begin
      AddTable('GROUPS','Группы');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'NAME', 'Имя Группы', True);
      AddTable('LESSONS','Предметы');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'NAME', 'Название Предмета', True);
      AddTable('TEACHERS','Преподаватели');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'LAST_NAME', 'Фамилия', True);
      AddField(FTables[High(FTables)], 'FIRST_NAME', 'Имя', False);
      AddField(FTables[High(FTables)], 'MIDDLE_NAME', 'Отчество', False);
      AddTable('CLASSROOMS','Аудитории');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'NAME', 'Номер Аудитории', True);
      AddTable('LESSON_TIMES','Время занятий');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'BEGIN_', 'Начало', True);
      AddField(FTables[High(FTables)], 'END_', 'Конец', True);
      AddTable('WEEKDAYS','Дни недели');
      AddField(FTables[High(FTables)], 'ID','ID', True);
      AddField(FTables[High(FTables)], 'NAME', 'День Недели', True);
      AddTable('LESSON_TYPES','Тип предмета');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'NAME', 'Тип', True);
      AddTable('TIMETABLE', 'Расписание');
      AddField(FTables[High(FTables)], 'ID', 'ID', True);
      AddField(FTables[High(FTables)], 'LESSON_ID', 'LESSON_ID', 'LESSONS', 'ID', 1);
      AddField(FTables[High(FTables)], 'LESSON_TYPE_ID', 'LESSON_TYPE_ID',
        'LESSON_TYPES', 'ID', 6);
      AddField(FTables[High(FTables)], 'TEACHER_ID', 'TEACHER_ID',
                                       'TEACHERS', 'ID', 2);
      AddField(FTables[High(FTables)], 'GROUP_ID', 'GROUP_ID', 'GROUPS', 'ID', 0
      );
      AddField(FTables[High(FTables)], 'CLASSROOM_ID', 'CLASSROOM_ID',
                                       'CLASSROOMS', 'ID', 3);
      AddField(FTables[High(FTables)], 'WEEKDAY_ID', 'WEEKDAY_ID', 'WEEKDAYS',
        'ID', 5);
      AddField(FTables[High(FTables)], 'LESSON_TIME_ID', 'LESSON_TIME_ID',
                                       'LESSON_TIMES', 'ID', 4);
    end;
end.

