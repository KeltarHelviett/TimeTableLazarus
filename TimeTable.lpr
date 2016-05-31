program TimeTable;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, UMain, UDirectory, UDB, UMetaData, UQueryBuild, UFilter,
  UFieldCard, UNotification, UTimeTable, UConflict, uexcel
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TCardForm, CardForm);
  Application.CreateForm(TTimeTableForm, TimeTableForm);
  Application.CreateForm(TConlfictsForm, ConlfictsForm);
  Application.Run;
end.

