unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  ccCronTab;

type

  { TTestCronTask }

  TTestCronTask= class(TTestCase)
  published
    procedure TestDecodeScheduleString;
    procedure TestMustRun;
  end;

implementation
uses
  dateutils;

procedure TTestCronTask.TestDecodeScheduleString;
var
  sched: TccSchedulation;
begin
  sched := TccSchedulation.Create;
  try
    sched.DecodeScheduleString('*/15 11-12 * * 1 *');
    CheckEquals(sched.Strings[TccSchedulation.WEEKDAY_IDX], '1', 'Wrong weekday');
    CheckEquals(sched.Strings[TccSchedulation.MINUTE_IDX], '*/15', 'Wrong minutes');
    CheckEquals(sched.Strings[TccSchedulation.HOUR_IDX], '11-12', 'Wrong hours');
    CheckEquals(sched.Strings[TccSchedulation.DAY_IDX], '*','Wrong day');
    CheckEquals(sched.Strings[TccSchedulation.MONTH_IDX], '*', 'Wrong month');
    CheckEquals(sched.Strings[TccSchedulation.YEAR_IDX], '*', 'Wrong year');
  finally
    sched.Free;
  end;
end;

procedure TTestCronTask.TestMustRun;
var
  tmpTask : TccScheduledTask;
  tmpDate : TDateTime;
begin
  tmpTask := TccScheduledTask.Create;
  try
    tmpTask.ScheduleString:= '*/2 * * * * *';
    tmpDate := EncodeDateTime(2019,5,12,0,0,0,0); // 00:00 12/05/2019
    CheckTrue(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,5,12,11,3,0,0); // 11:03 12/05/2019
    CheckFalse(tmpTask.MustRun(tmpDate));

    tmpTask.ScheduleString:= '*/15 11-12 * * 1 *'; // monday, every 15 minutes beetween 11:00 and 12:00
    tmpDate := EncodeDateTime(2019,1,14,11,30,0,0); // monday 11:30 14/01/2019
    CheckTrue(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,1,14,11,00,0,0); // monday 11:00 14/01/2019
    CheckTrue(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,1,14,12,00,0,0); // monday 12:00 14/01/2019
    CheckTrue(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,1,15,12,00,0,0); // tuesday 12:00 15/01/2019
    CheckFalse(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,2,18,13,15,0,0); // monday 13:15 18/02/2019
    CheckFalse(tmpTask.MustRun(tmpDate));
    tmpDate := EncodeDateTime(2019,2,18,11,45,0,0); // monday 11:45 18/02/2019
    CheckTrue(tmpTask.MustRun(tmpDate));


  finally
    tmpTask.Free;
  end;
end;



initialization

  RegisterTest(TTestCronTask);
end.

