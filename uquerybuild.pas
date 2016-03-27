unit UQueryBuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, UFilter, Dialogs;

function BuildSelectPart(ATableTag: integer): string;
function PutTableFields(ATableName: string): string;
function PrepareWherePart(ATableTag: integer; AFilters: array of TFilter): string;
function SearchTableByName(ATableName: string): integer;

implementation

function BuildSelectPart(ATableTag: integer): string;
var
  i: integer;
  tables: array of string;
  fields: array of string;
  sl: TStringList;
begin
  Result := ' SELECT ';
  for i := 0 to High(MetaData.FTables[ATableTag].FFields) do
    begin
      if MetaData.FTables[ATableTag].FFields[i].FRefTableName <> '' then
         begin
           SetLength(tables, Length(tables) + 1);
           SetLength(fields, Length(fields) + 1);
           tables[High(tables)] :=
             MetaData.FTables[ATableTag].FFields[i].FRefTableName;
           fields[High(fields)] :=
             MetaData.FTables[ATableTag].FFields[i].FRefFieldName;
           Result +=
             PutTableFields(MetaData.FTables[ATableTag].FFields[i].FRefTableName);
         end
      else
        Result +=  MetaData.FTables[ATableTag].FRealName + '.' +
                   MetaData.FTables[ATableTag].FFields[i].FRealName + ', ';
    end;
  Delete(Result, Length(Result) - 1, 2);
  Result += ' FROM ' + MetaData.FTables[ATableTag].FRealName;
  for i := 0 to High(tables) do
    begin
      Result += ' INNER JOIN ' + tables[i] + ' ON ' + tables[i] + '.' + fields[i];
      Result += ' = '  + MetaData.FTables[ATableTag].FRealName +  '.' +
                MetaData.FTables[ATableTag].FFields[i + 1].FRealName;
    end;
  sl := TStringList.Create;
  sl.Text := Result;
end;

function PutTableFields(ATableName: string): string;
var
  i, j: integer;
begin
  Result := '';
  for i := 0 to High(MetaData.FTables) do
    begin
      if MetaData.FTables[i].FRealName = ATableName then
         for j := 1 to High(MetaData.FTables[i].FFields) do
           begin
             Result += ' ' + MetaData.FTables[i].FRealName + '.'
                       + MetaData.FTables[i].FFields[j].FRealName + ', ';
           end;
    end;
end;

function PrepareWherePart(ATableTag: integer; AFilters: array of TFilter
  ): string;
var
  i, j, k: integer;
  index: integer;
begin
  if Length(AFilters) > 0 then
    Result := ' WHERE ';
  for i := 0 to High(AFilters) do
    begin
      if (i > 0) then
        Result +=' AND ';
      for j := 0 to High(MetaData.FTables[ATableTag].FFields) do
        begin
          if (MetaData.FTables[ATableTag].FFields[j].FDisplayName =
              AFilters[i].FFields.Text)
          then
            begin
              Result += ' ' + MetaData.FTables[ATableTag].FRealName;
              Result += '.' + MetaData.FTables[ATableTag].FFields[j].FRealName;
              Result += ' ' + AFilters[i].FOperations.Text + ':p' + IntToStr(i);
              Break;
            end;
          if MetaData.FTables[ATableTag].FFields[j].FRefTableName <> '' then
            begin
              index := SearchTableByName(
                MetaData.FTables[ATableTag].FFields[j].FRefTableName);
              for k := 0 to High(MetaData.FTables[index].FFields) do
                begin
                  if (MetaData.FTables[index].FFields[k].FDisplayName =
                     AFilters[i].FFields.Text)
                  then
                    begin
                      Result += ' ' + MetaData.FTables[index].FRealName + '.';
                      Result += MetaData.FTables[index].FFields[k].FRealName;
                      Result += ' ' + AFilters[i].FOperations.Text + ':p';
                      Result += IntToStr(i);
                    end;
                end;
            end;
        end;
    end;
end;

function SearchTableByName(ATableName: string): integer;
var
  i: integer;
begin
  for i := 0 to High(MetaData.FTables) do
    if MetaData.FTables[i].FRealName = ATableName then
      begin
        Exit(i);
      end;
  Result := -1;
end;

end.

