unit UDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    IBConnection1: TIBConnection;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin

end;

end.

