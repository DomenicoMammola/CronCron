unit ccThread;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  eventlog, syncobjs,
  ccCronTab;

type

  { TccControlThread }

  TccControlThread = class(TThread)
  strict private
    const SLEEPING_FOR_MILLISECONDS : integer = 1000;
  strict private
    FLog: TEventLog;
    FDiedEvent: TSimpleEvent;
  public
    Constructor Create(aLog : TEventLog; aDiedEvent : TSimpleEvent);
    procedure Execute; override;
  end;

implementation

uses
  sysutils;

{ TccControlThread }

constructor TccControlThread.Create(aLog: TEventLog; aDiedEvent: TSimpleEvent);
begin
  FLog:= aLog;
  FreeOnTerminate:= true;
  FDiedEvent := aDiedEvent;
  inherited Create(false);
end;

procedure TccControlThread.Execute;
var
  lastMinute, newMinute : integer;
  hour, minute, second, millisecond : word;
  currentTime : TDateTime;
begin
  lastMinute := -1;

  Repeat
    currentTime := Now;
    DecodeTime(currentTime, hour, minute, second, millisecond);
    newMinute := (hour * 1000) + minute;
    if lastMinute <> newMinute then
    begin
      // FLog.Info('New minute: ' + TimeToStr(currentTime));
      lastMinute := newMinute;

    end;

    Sleep(SLEEPING_FOR_MILLISECONDS);
  Until Terminated;

  FDiedEvent.SetEvent;
  FDiedEvent := nil;
end;

end.
