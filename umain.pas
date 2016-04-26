unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  UMetaData, UDirectory, UDB, UTimeTable;

type

  { TMain }

  TMain = class(TForm)
    MainMenu1: TMainMenu;
    MTimeTable: TMenuItem;
    MHelpAbout: TMenuItem;
    MHelp: TMenuItem;
    MFile: TMenuItem;
    MTables: TMenuItem;
    MFileExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MTimeTableClick(Sender: TObject);
    procedure MFileExitClick(Sender: TObject);
    procedure MHelpAboutClick(Sender: TObject);
    procedure MTablesItemClick(Sender: TObject);
  private
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }


procedure TMain.MHelpAboutClick(Sender: TObject);
begin
  ShowMessage('Б8103а, Терехов Дмитрий.');
end;

procedure TMain.MTablesItemClick(Sender: TObject);
var
  newForm: TTableForm;
begin
  newForm := TTableForm.Create(Main, (Sender as TMenuItem).Tag);
  newForm.Caption := (Sender as TMenuItem).Caption;
  newForm.ShowDefaultTable;

  newForm.Show;
end;

procedure TMain.MFileExitClick(Sender: TObject);
begin
  Main.Close;
end;

procedure TMain.FormCreate(Sender: TObject);
var
  m: TMenuItem;
  i: integer;
begin
  for i := 0 to High(MetaData.FTables) do
    begin
      m := TMenuItem.Create(Main);
      m.Caption := MetaData.FTables[i].FDisplayName;
      m.Tag := i;
      m.OnClick := @MTablesItemClick;
      MTables.Add(m);
    end;
end;

procedure TMain.MTimeTableClick(Sender: TObject);
var
  f: TTimeTableForm;
  i: integer;
begin
  f := TTimeTableForm.Create(Self);
  f.Show;
end;

end.

