program CronCronConsoleApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,eventlog, syncobjs
  ,ccThread, ccCronTab;

type

  { TFrescoBOTConsole }

  { TCronCronConsole }

  TCronCronConsole = class(TCustomApplication)
  strict private
    FEventLog : TEventLog;
    procedure Test1Minute(aCheckTerminated : TCheckTerminatedProcedure);
    procedure Test2Minutes(aCheckTerminated : TCheckTerminatedProcedure);
    procedure Test3Minutes(aCheckTerminated : TCheckTerminatedProcedure);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCronCronConsole }

procedure TCronCronConsole.Test1Minute(aCheckTerminated: TCheckTerminatedProcedure);
begin
  FEventLog.Info('1 minute! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.Test2Minutes(aCheckTerminated: TCheckTerminatedProcedure);
begin
  FEventLog.Info('2 minutes! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.Test3Minutes(aCheckTerminated: TCheckTerminatedProcedure);
begin
  FEventLog.Info('3 minutes! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.DoRun;
var
  ErrorMsg: String;
  CntThread: TccControlThread;
  tmpDiedEvent : TSimpleEvent;
  i : integer;
  wr : TWaitResult;
  FScheduledTasks : TccScheduledTasks;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  tmpDiedEvent := TSimpleEvent.Create;
  FEventLog:=TEventlog.Create(nil);
  FScheduledTasks := TccScheduledTasks.Create;
  try
    //tmpEventLog.RaiseExceptionOnError:=False;
    FEventLog.LogType:=ltFile;
    FEventLog.FileName:= 'log.txt'; // IncludeTrailingPathDelimiter(ExtractFileDir(ApplicationName)) + 'log.txt';
    FEventLog.Active:= true;
    tmpDiedEvent.ResetEvent;

    with FScheduledTasks.Add do
    begin
      TaskDescription:= '2 minutes';
      TaskProcedureOfObject:= @Test2Minutes;
      ScheduleString:= '*/2 * * * *';
    end;

    with FScheduledTasks.Add do
    begin
      TaskDescription:= '3 minutes';
      TaskProcedureOfObject:= @Test3Minutes;
      ScheduleString:= '*/3 * * * *';
    end;

    with FScheduledTasks.Add do
    begin
      TaskDescription:= '1 minute';
      TaskProcedureOfObject:= @Test1Minute;
      ScheduleString:= '*/1 * * * *';
    end;

    CntThread := TccControlThread.Create(FEventLog, tmpDiedEvent, FScheduledTasks);

    WriteLn('Press ENTER to stop thread');
    ReadLn;

    CntThread.Terminate;

    wr := tmpDiedEvent.WaitFor(1000);
    i := 0;
    while (wr = wrTimeout) and (i < 20) do
    begin
      WriteLn('Thread stopping, waiting..');
      FEventLog.Info('Thread stopping, waiting..');
      wr := tmpDiedEvent.WaitFor(1000);
      inc(i);
    end;

    if wr = wrSignaled then
    begin
      WriteLn('Thread stopped successfully');
      FEventLog.Info('Thread stopped successfully');
    end
    else if wr = wrTimeout then
    begin
      WriteLn('Timeout while waiting that thread stops');
      FEventLog.Error('Timeout while waiting that thread stops');
    end
    else
    begin
      WriteLn('Unable to stop thread');
      FEventLog.Error('Unable to stop thread');
    end;
    WriteLn('Finished');
    FEventLog.Info('Finished');
  finally
    FEventLog.Free;
    tmpDiedEvent.Free;
    FScheduledTasks.Free;
  end;

  WriteLn('Press ENTER to exit');
  ReadLn;

  // stop program loop
  Terminate;
end;

constructor TCronCronConsole.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCronCronConsole.Destroy;
begin
  inherited Destroy;
end;

procedure TCronCronConsole.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TCronCronConsole;
begin
  Application:=TCronCronConsole.Create(nil);
  Application.Run;
  Application.Free;
end.

