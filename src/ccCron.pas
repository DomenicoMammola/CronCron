// This is part of the CronCron Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit ccCron;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  eventlog, syncobjs,
  ccCronTab, ccThread;

type
  TccCron = class
  strict private
    const DEFAULT_TIMEOUT : integer = 20000;
  strict private
    FTasks : TccScheduledTasks;
    FEventLog: TEventLog;
    FRunning : boolean;
    FTimeout : integer;

    FThread: TccControlThread;
    FDiedEvent : TSimpleEvent;
  public
    constructor Create(aEventLog : TEventLog);
    destructor Destroy; override;
    procedure Start;
    function Stop : boolean;

    property Tasks : TccScheduledTasks read FTasks;
    property Running : boolean read FRunning;

    property Timeout : integer read FTimeout write FTimeout;
  end;


implementation

{ TccCron }

constructor TccCron.Create(aEventLog: TEventLog);
begin
  FEventLog := aEventLog;
  FTasks := TccScheduledTasks.Create;
  FRunning := false;
  FDiedEvent := TSimpleEvent.Create;
  FTimeout:= DEFAULT_TIMEOUT;
end;

destructor TccCron.Destroy;
begin
  FDiedEvent.Free;
  FTasks.Free;
  inherited Destroy;
end;

procedure TccCron.Start;
begin
  if FRunning or Assigned(FThread) then
  begin
    FEventLog.Warning('Cron tab already running');
    exit;
  end;

  FDiedEvent.ResetEvent;
  FRunning := true;
  FThread := TccControlThread.Create(FEventLog, FDiedEvent, FTasks);
end;

function TccCron.Stop : boolean;
var
  wr : TWaitResult;
  tm : integer;
  i : integer;
begin
  Result := false;
  FThread.Terminate;

  tm := FTimeout div 20;
  wr := FDiedEvent.WaitFor(tm);
  i := 0;
  while (wr = wrTimeout) and (i < 20) do
  begin
    FEventLog.Info('Thread stopping, waiting..');
    wr := FDiedEvent.WaitFor(tm);
    inc(i);
  end;

  if wr = wrSignaled then
  begin
    FEventLog.Info('Thread stopped successfully');
    Result:= true;
    FRunning:= false;
  end
  else if wr = wrTimeout then
    FEventLog.Error('Timeout while waiting that thread stops')
  else
    FEventLog.Error('Unable to stop thread');

end;


end.
