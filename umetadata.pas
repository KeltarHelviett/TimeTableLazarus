unit UMetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TField = record
    FRealName: string;
    FDisplayName: string;
    FRefTableName: string;
    FRefFieldName: string;
  end;

  TTable = record
    FRealName: string;
    FDisplayName: string;
    FFields: array of TField;
  end;

  { TMetaData }

  TMetaData = class
    procedure AddTable(ARealTableName, ADisplayTableName: string);
    procedure AddField(var ATable: TTable; ARealFieldName, ADisplayFieldName,
      ARefTableName, ARefFieldName: string);
    procedure AddField(var ATable: TTable; ARealFieldName, ADisplayFieldName: string);
    public
      FHasRef: Boolean;
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
end;

procedure TMetaData.AddField(var ATable: TTable; ARealFieldName,
  ADisplayFieldName, ARefTableName, ARefFieldName: string);
begin
  with ATable do
    begin
      SetLength(FFields, Length(FFields) + 1);
      FFields[High(FFields)].FRealName := ARealFieldName;
      FFields[High(FFields)].FDisplayName := ADisplayFieldName;
      FFields[High(FFields)].FRefTableName := ARefTableName;
      FFields[High(FFields)].FRefFieldName := ARefFieldName;
    end;
end;

procedure TMetaData.AddField(var ATable: TTable; ARealFieldName,
  ADisplayFieldName: string);
begin
  with ATable do
    begin
      SetLength(FFields, Length(FFields) + 1);
      FFields[High(FFields)].FRealName := ARealFieldName;
      FFields[High(FFields)].FDisplayName := ADisplayFieldName;
      FFields[High(FFields)].FRefTableName := '';
      FFields[High(FFields)].FRefFieldName := '';
    end;
end;

initialization
  MetaData := TMetaData.Create;
  with MetaData do
    begin
      AddTable('Groups','Группы');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'name', 'Имя Группы');
      AddTable('Lessons','Предметы');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'name', 'Название Предмета');
      AddTable('Teachers','Преподаватели');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'last_name', 'Фамилия');
      AddField(FTables[High(FTables)], 'first_name', 'Имя');
      AddField(FTables[High(FTables)], 'middle_name', 'Отчество');
      AddTable('Classrooms','Аудитории');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'name', 'Номер Аудитории');
      AddTable('Lesson_Times','Время занятий');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'begin_', 'Начало');
      AddField(FTables[High(FTables)], 'end_', 'Конец');
      AddTable('Weekdays','Дни недели');
      AddField(FTables[High(FTables)], 'id','ID');
      AddField(FTables[High(FTables)], 'name', 'День Недели');
      AddTable('Lesson_Types','Тип предмета');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'name', 'Тип');
      AddTable('Timetable', 'Расписание');
      AddField(FTables[High(FTables)], 'id', 'ID');
      AddField(FTables[High(FTables)], 'lesson_id', 'lesson_id', 'Lessons', 'id');
      AddField(FTables[High(FTables)], 'lesson_type_id', 'lesson_type_id',
                                       'Lesson_Types', 'id');
      AddField(FTables[High(FTables)], 'teacher_id', 'teacher_id',
                                       'Teachers', 'id');
      AddField(FTables[High(FTables)], 'group_id', 'group_id', 'Groups', 'id');
      AddField(FTables[High(FTables)], 'classroom_id', 'classroom_id',
                                       'Classrooms', 'id');
      AddField(FTables[High(FTables)], 'weekday_id', 'weekday_id', 'Weekdays', 'id');
      AddField(FTables[High(FTables)], 'lesson_time_id', 'lesson_time_id',
                                       'Lesson_Times', 'id');
    end;
end.

