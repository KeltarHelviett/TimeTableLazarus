unit UQueryBuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, UFilter, Dialogs;

function BuildSelectPart(ATableTag: integer): string;
function PutTableFields(ATableName: string): string;
function PrepareWherePart(ATableTag: integer; AFilters: array of TFilter): string;
//function SearchTableByName(ATableName: string): integer;
function DeleteFieldFromQuery(AField, AQuery: string): string;
function PrepareInsertPart(ATableTag: integer): string;
function PrepareUpdatePart(ATableTag, Aid: integer): string;
function BuildDrawGridCellQuery(ATopTabeTag, ALeftTableTag, ATopHeaderId,
  ALeftHeaderID: integer): string;

implementation

function BuildSelectPart(ATableTag: integer): string;
var
  i: integer;
  tables: array of string;
  fields: array of string;
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
             Result += ' ' + MetaData.FTables[i].FRealName + '.' +
               MetaData.FTables[i].FFields[j].FRealName + ', ';
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
              index := MetaData.FTables[ATableTag].FFields[j].FRefTableInd;
              //SearchTableByName(
              //  MetaData.FTables[ATableTag].FFields[j].FRefTableName);
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

{function SearchTableByName(ATableName: string): integer;
var
  i: integer;
begin
  for i := 0 to High(MetaData.FTables) do
    if MetaData.FTables[i].FRealName = ATableName then
      begin
        Exit(i);
      end;
  Result := -1;
end; }

function DeleteFieldFromQuery(AField, AQuery: string): string;
var
  s: string;
  i, j, k: integer;
begin
  s := '';
  i := Pos(AField , AQuery);
  j := i + 1;
  while (AQuery[i] <> ' ') do
    begin
      AQuery[i] := ' ';
      Dec(i);
    end;
  while (AQuery[j] <> ',') and (AQuery[j] <> ' ') do
    begin
      AQuery[j] := ' ';
      Inc(j);
    end;
  AQuery[j] := ' ';
  Result := AQuery;
end;

function PrepareInsertPart(ATableTag: integer): string;
var
  i: integer;
begin
  Result := 'INSERT INTO ' + MetaData.FTables[ATableTag].FRealName + ' VALUES(0,';
  for i := 1 to High(MetaData.FTables[ATableTag].FFields) do
    Result += ':p' + IntToStr(i) + ' ,';
  Delete(Result, Length(Result), 1);
  Result += ')';
end;

function PrepareUpdatePart(ATableTag, Aid: integer): string;
var
  i: integer;
begin
  Result := 'UPDATE ' + MetaData.FTables[ATableTag].FRealName + ' SET ';
  for i := 1 to High(MetaData.FTables[ATableTag].FFields) do
    Result +=  MetaData.FTables[ATableTag].FFields[i].FRealName + ' = :p'
      + IntToStr(i) + ', ';
  Delete(Result, Length(Result) - 1, 1);
  Result += ' WHERE id = ' + IntToStr(Aid);
end;

function BuildDrawGridCellQuery(ATopTabeTag, ALeftTableTag, ATopHeaderId,
  ALeftHeaderID: integer): string;
begin
  Result := BuildSelectPart(High(MetaData.FTables));
  Result += Format(
    ' where %s.id = %d and %s.id = %d ',
    [
      MetaData.FTables[ATopTabeTag].FRealName, ATopHeaderId,
      MetaData.FTables[ALeftTableTag].FRealName, ALeftHeaderID
    ]);
end;

end.

