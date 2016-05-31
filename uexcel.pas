unit UExcel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, comobj, Dialogs;

procedure SaveToExcel(ASaveDialog: TSaveDialog);
implementation

procedure SaveToExcel(ASaveDialog: TSaveDialog);
var
  XL: variant;
begin

  XL := CreateOleObject('Excel.Application');
  XL.DisplayAlerts := False;

  XL.WorkBooks.Open(WideString(ASaveDialog.FileName));
  XL.WorkBooks.item[1].SaveAs(WideString(ASaveDialog.FileName), 51);
  XL.Quit;
end;

end.

