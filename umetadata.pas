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
    FRefTableInd: integer;
    FPermittedToShow: Boolean;
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
end;

procedure TMetaData.AddField(var ATable: TTable; ARealFieldName,
  ADisplayFieldName, ARefTableName, ARefFieldName: string; ARefTableInd: integer
  );
begin
SetLength(ATable.FFields, Length(ATable.FFields) + 1);
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
      AddTable('Groups','Группы');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'name', 'Имя Группы', True);
      AddTable('Lessons','Предметы');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'name', 'Название Предмета', True);
      AddTable('Teachers','Преподаватели');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'last_name', 'Фамилия', True);
      AddField(FTables[High(FTables)], 'first_name', 'Имя', False);
      AddField(FTables[High(FTables)], 'middle_name', 'Отчество', False);
      AddTable('Classrooms','Аудитории');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'name', 'Номер Аудитории', True);
      AddTable('Lesson_Times','Время занятий');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'begin_', 'Начало', True);
      AddField(FTables[High(FTables)], 'end_', 'Конец', True);
      AddTable('Weekdays','Дни недели');
      AddField(FTables[High(FTables)], 'id','ID', True);
      AddField(FTables[High(FTables)], 'name', 'День Недели', True);
      AddTable('Lesson_Types','Тип предмета');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'name', 'Тип', True);
      AddTable('Timetable', 'Расписание');
      AddField(FTables[High(FTables)], 'id', 'ID', True);
      AddField(FTables[High(FTables)], 'lesson_id', 'lesson_id', 'Lessons', 'id', 1);
      AddField(FTables[High(FTables)], 'lesson_type_id', 'lesson_type_id',
        'Lesson_Types', 'id', 6);
      AddField(FTables[High(FTables)], 'teacher_id', 'teacher_id',
                                       'Teachers', 'id', 2);
      AddField(FTables[High(FTables)], 'group_id', 'group_id', 'Groups', 'id', 0
      );
      AddField(FTables[High(FTables)], 'classroom_id', 'classroom_id',
                                       'Classrooms', 'id', 3);
      AddField(FTables[High(FTables)], 'weekday_id', 'weekday_id', 'Weekdays',
        'id', 5);
      AddField(FTables[High(FTables)], 'lesson_time_id', 'lesson_time_id',
                                       'Lesson_Times', 'id', 4);
    end;
end.

