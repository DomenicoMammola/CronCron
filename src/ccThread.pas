// This is part of the CronCron Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit ccThread;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs,
  eventlog, syncobjs,
  ccCronTab;

type

  { TccControlThread }

  TccControlThread = class(TThread)
  strict private
    const SLEEPING_FOR_MILLISECONDS : integer = 1000;
    const TIMEOUT_EXECUTOR_TO_END : integer = 5000;
  strict private
    FLog: TEventLog;
    FDiedEvent: TSimpleEvent;
    FDiedEvents  : TObjectList;
    FToBeFreed : TObjectList;
    FTasks : TccScheduledTasks;
    function CheckTerminated : Boolean;
  public
    constructor Create(aLog : TEventLog; aDiedEvent : TSimpleEvent; aScheduledTasks : TccScheduledTasks);
    destructor Destroy; override;
    procedure Execute; override;
  end;


  { TccExecutorThread }

  TccExecutorThread = class(TThread)
  strict private
    FDiedEvent: TSimpleEvent;
    FTask : TccScheduledTask;
    FCheckTerminatedProcedure : TCheckTerminatedProcedure;
  public
    constructor Create(aDiedEvent : TSimpleEvent; aTask : TccScheduledTask; aCheckTerminatedProcedure: TCheckTerminatedProcedure);
    procedure Execute; override;
  end;

implementation

uses
  sysutils, dateutils;

{ TccExecutorThread }

constructor TccExecutorThread.Create(aDiedEvent: TSimpleEvent; aTask: TccScheduledTask; aCheckTerminatedProcedure: TCheckTerminatedProcedure);
begin
  FreeOnTerminate:= true;
  FDiedEvent := aDiedEvent;
  FTask := aTask;
  FCheckTerminatedProcedure:= aCheckTerminatedProcedure;
  inherited Create(false);
end;

procedure TccExecutorThread.Execute;
begin
  if Assigned(FTask.TaskProcedureOfObject) then
    FTask.TaskProcedureOfObject(FCheckTerminatedProcedure)
  else if Assigned(FTask.TaskProcedure) then
    FTask.TaskProcedure(FCheckTerminatedProcedure);
  FDiedEvent.SetEvent;
end;

{ TccControlThread }

function TccControlThread.CheckTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

constructor TccControlThread.Create(aLog: TEventLog; aDiedEvent: TSimpleEvent; aScheduledTasks : TccScheduledTasks);
begin
  FLog:= aLog;
  FreeOnTerminate:= true;
  FDiedEvent := aDiedEvent;
  FDiedEvents:= TObjectList.Create(false);
  FToBeFreed := TObjectList.Create(true);
  FTasks := aScheduledTasks;
  inherited Create(false);
end;

destructor TccControlThread.Destroy;
begin
  FDiedEvents.Free;
  FToBeFreed.Free;
  inherited Destroy;
end;

procedure TccControlThread.Execute;
var
  lastMinute, newMinute : integer;
  hour, minute : word;
  year, month, day, weekday : word;
  i : integer;
  newDiedEvent : TSimpleEvent;
  wr : TWaitResult;
begin
  ccDecodeDateTime(Now, hour, minute, year, month, day, weekday);
  lastMinute := (hour * 1000) + minute;
  Sleep(SLEEPING_FOR_MILLISECONDS div 2);
  repeat
    ccDecodeDateTime(Now, hour, minute, year, month, day, weekday);
    newMinute := (hour * 1000) + minute;
    if lastMinute <> newMinute then
    begin
      // FLog.Info('New minute: ' + TimeToStr(currentTime));
      lastMinute := newMinute;
      for i := 0 to FTasks.Count - 1 do
      begin
        if FTasks.Get(i).MustRun(minute, hour, day, month, year, weekday) then
        begin
          newDiedEvent := TSimpleEvent.Create;
          FDiedEvents.Add(newDiedEvent);
          newDiedEvent.ResetEvent;
          TccExecutorThread.Create(newDiedEvent, FTasks.Get(i), Self.CheckTerminated);
        end;
      end;
    end;

    if not Terminated then
      Sleep(SLEEPING_FOR_MILLISECONDS);
  until Terminated;

  for i := 0 to FDiedEvents.Count - 1 do
  begin
    wr := (FDiedEvents.Items[i] as TSimpleEvent).WaitFor(TIMEOUT_EXECUTOR_TO_END);
    if wr <> wrSignaled then
      FLog.Warning('Timeout stopping task #' + IntToStr(i) + ' ' +  FTasks.Get(i).TaskDescription)
    else
      FToBeFreed.Add(FDiedEvents.Items[i]);
  end;

  FDiedEvent.SetEvent;
  FDiedEvent := nil;
end;

end.
