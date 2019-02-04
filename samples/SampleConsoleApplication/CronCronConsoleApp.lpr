program CronCronConsoleApp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,eventlog, syncobjs
  ,ccCron, ccCronTab;

type

  { TFrescoBOTConsole }

  { TCronCronConsole }

  TCronCronConsole = class(TCustomApplication)
  strict private
    FEventLog : TEventLog;
    procedure Test1Minute(aCheckTerminated : TCheckTerminatedProcedure; aEventLog : TEventLog);
    procedure Test2Minutes(aCheckTerminated : TCheckTerminatedProcedure; aEventLog : TEventLog);
    procedure Test3Minutes(aCheckTerminated : TCheckTerminatedProcedure; aEventLog : TEventLog);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCronCronConsole }

procedure TCronCronConsole.Test1Minute(aCheckTerminated: TCheckTerminatedProcedure; aEventLog : TEventLog);
begin
  aEventLog.Info('1 minute! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.Test2Minutes(aCheckTerminated: TCheckTerminatedProcedure; aEventLog : TEventLog);
begin
  aEventLog.Info('2 minutes! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.Test3Minutes(aCheckTerminated: TCheckTerminatedProcedure; aEventLog : TEventLog);
begin
  aEventLog.Info('3 minutes! ' + DateTimeToStr(Now));
end;

procedure TCronCronConsole.DoRun;
var
  cron : TccCron;
  ErrorMsg : String;
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

  FEventLog:=TEventlog.Create(nil);
  cron := TccCron.Create(FEventLog);
  try
    //tmpEventLog.RaiseExceptionOnError:=False;
    FEventLog.LogType:=ltFile;
    FEventLog.FileName:= 'log.txt';
    FEventLog.Active:= true;

    with cron.Tasks.Add do
    begin
      TaskDescription:= '2 minutes';
      TaskProcedureOfObject:= @Test2Minutes;
      ScheduleString:= '*/2 * * * * *';
    end;

    with cron.Tasks.Add do
    begin
      TaskDescription:= '3 minutes';
      TaskProcedureOfObject:= @Test3Minutes;
      ScheduleString:= '*/3 * * * * *';
    end;

    with cron.Tasks.Add do
    begin
      TaskDescription:= '1 minute';
      TaskProcedureOfObject:= @Test1Minute;
      ScheduleString:= '*/1 * * * * *';
    end;

    cron.Start;

    WriteLn('Press ENTER to stop thread');
    ReadLn;

    if cron.Stop then
    begin
      WriteLn('Cron stopped successfully');
      FEventLog.Info('Cron stopped successfully');
    end
    else
    begin
      WriteLn('Unable to stop cron');
      FEventLog.Error('Unable to stop cron');
    end;
    WriteLn('Finished');
    FEventLog.Info('Finished');
  finally
    FEventLog.Free;
    cron.Free;
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

