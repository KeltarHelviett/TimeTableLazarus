unit UNotification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  TCard = record
    FTableTag: integer;
    FCardId: integer;
    FBringToFront: TNotifyEvent;
  end;

  { TNotifier }

  TNotifier = class
    procedure Subscribe(AMethod: TNotifyEvent);
    procedure Update;
    function isCardOpened(ATableTag, ACardId: Integer): Boolean;
    procedure CloseCard(ATableTag, ACardId: Integer);
    procedure RegisterCard(ATableTag, ACardId: Integer; ABringCardToFront: TNotifyEvent);
    private
      FSubscibers: array of TNotifyEvent;
      FOpenedCards: array of TCard;
  end;
var
  Notifier: TNotifier;

implementation



{ TNotifier }

procedure TNotifier.Subscribe(AMethod: TNotifyEvent);
begin
  SetLength(FSubscibers, Length(FSubscibers) + 1);
  FSubscibers[High(FSubscibers)] := AMethod;
end;

procedure TNotifier.Update;
var
  i: integer;
begin
  for i := 0 to High(FSubscibers) do
    FSubscibers[i](nil);
end;

function TNotifier.isCardOpened(ATableTag, ACardId: Integer): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to High(FOpenedCards) do
    if (FOpenedCards[i].FCardId = ACardId) and
       (FOpenedCards[i].FTableTag = ATableTag) then
       begin
         FOpenedCards[i].FBringToFront(nil);
         Result := True;
         Break;
       end;
end;

procedure TNotifier.CloseCard(ATableTag, ACardId: Integer);
var
  i, j: integer;
begin
  for i := 0 to High(FOpenedCards) do
    if (FOpenedCards[i].FTableTag = ATableTag) and
       (FOpenedCards[i].FCardId = ACardId) then
          for j := i + 1 to High(FOpenedCards) do
            FOpenedCards[j - 1] := FOpenedCards[j];
  SetLength(FOpenedCards, Length(FOpenedCards) - 1);
end;

procedure TNotifier.RegisterCard(ATableTag, ACardId: Integer;
  ABringCardToFront: TNotifyEvent);
begin
  SetLength(FOpenedCards, Length(FOpenedCards) + 1);
  with FOpenedCards[High(FOpenedCards)] do
    begin
      FTableTag := ATableTag;
      FCardId := ACardId;
      FBringToFront := ABringCardToFront;
    end;
end;

initialization
  Notifier := TNotifier.Create;
end.

