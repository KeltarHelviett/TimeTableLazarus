unit UNotification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TNotification }

  TNotification = class
    procedure Subscribe(AMethod: TNotifyEvent);
    procedure Update;
    private
      Subscibers: array of TNotifyEvent;
  end;
var
  Notifier: TNotification;

implementation



{ TNotification }

procedure TNotification.Subscribe(AMethod: TNotifyEvent);
begin
  SetLength(Subscibers, Length(Subscibers) + 1);
  Subscibers[High(Subscibers)] := AMethod;
end;

procedure TNotification.Update;
var
  i: integer;
begin
  for i := 0 to High(Subscibers) do
    Subscibers[i](nil);
end;

initialization
  Notifier := TNotification.Create;
end.

