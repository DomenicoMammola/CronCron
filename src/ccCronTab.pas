// This is part of the CronCron Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit ccCronTab;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, contnrs, eventlog, syncobjs;

// https://help.ubuntu.com/community/CronHowto

resourcestring
  rsErrorWrongFormat = 'Wrong format of cron schedule string: %s';

type

  TCheckTerminatedProcedure = function : boolean of object;
  TccTaskProcedureOfObject = procedure (aCheckTerminated : TCheckTerminatedProcedure; aEventLog : TEventLog) of Object;
  TccTaskProcedure = procedure (aCheckTerminated : TCheckTerminatedProcedure; aEventLog : TEventLog);

  { FSchedulation }

  TccSchedulation = class
  strict private
    const ARRAY_SIZE = 6;
  public
    const MINUTE_IDX : integer = 0;
    const HOUR_IDX : integer = 1;
    const DAY_IDX : integer = 2;
    const MONTH_IDX : integer = 3;
    const WEEKDAY_IDX : integer = 4;
    const YEAR_IDX : integer = 5;
  public
    Strings : array of String;
  public
    constructor Create;

    procedure Clear;
    procedure DecodeScheduleString (const aScheduleString : String);
  end;


  { TccScheduledTask }

  TccScheduledTask = class
  strict private
    FTaskProcedureOfObject: TccTaskProcedureOfObject;
    FTaskProcedure : TccTaskProcedure;
    FScheduleString : String;
    FTaskDescription : String;
    FDecodedSchedule : TccSchedulation;
    procedure SetScheduleString(AValue: String);
    function CheckValue (const aString : String; const aValue : word; out aAnyValueAllowed : boolean): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function MustRun (const aMinute, aHour, aDay, aMonth, aYear, aWeekday : word) : boolean; overload;
    function MustRun (const aDateTime : TDateTime): boolean; overload;

    property TaskDescription : String read FTaskDescription write FTaskDescription;
    property TaskProcedureOfObject : TccTaskProcedureOfObject read FTaskProcedureOfObject write FTaskProcedureOfObject;
    property TaskProcedure : TccTaskProcedure read FTaskProcedure write FTaskProcedure;
    property ScheduleString : String read FScheduleString write SetScheduleString;
  end;

  { TccScheduledTasks }

  TccScheduledTasks = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count : integer;
    function Get(const aIndex : integer) : TccScheduledTask;
    function Add : TccScheduledTask;
    procedure Clear;
  end;

  procedure ccDecodeDateTime(const aDateTime: TDateTime; out hour, minute, year, month, day, weekday : word);


implementation

uses
  sysutils, dateutils;


procedure RaiseWrongScheduleStringException (const aValue : String);
begin
  raise Exception.Create(Format(rsErrorWrongFormat, [aValue]));
end;

procedure ccDecodeDateTime(const aDateTime: TDateTime; out hour, minute, year, month, day, weekday: word);
var
  second, millisecond : word;
begin
  DecodeDate(aDateTime, year, month, day);
  DecodeTime(aDateTime, hour, minute, second, millisecond);
  weekday:= DayOfTheWeek(aDateTime); // day-of-THE-week -> ISO 8601 compliant
end;

{ TccScheduledTasks }

constructor TccScheduledTasks.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TccScheduledTasks.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TccScheduledTasks.Count: integer;
begin
  Result := FList.Count;
end;

function TccScheduledTasks.Get(const aIndex: integer): TccScheduledTask;
begin
  Result := FList.Items[aIndex] as TccScheduledTask;
end;

function TccScheduledTasks.Add: TccScheduledTask;
begin
  Result := TccScheduledTask.Create;
  FList.Add(Result);
end;

procedure TccScheduledTasks.Clear;
begin
  FList.Clear;
end;

{ TccSchedulation }

constructor TccSchedulation.Create;
begin
  SetLength(Strings, ARRAY_SIZE);
  Self.Clear;
end;

procedure TccSchedulation.Clear;
var
  i : integer;
begin
  for i := Low(Strings) to High(Strings) do
    Strings[i] := '';
end;

procedure TccSchedulation.DecodeScheduleString(const aScheduleString: String);
var
  i, cur : integer;
  lastStr, tmp : String;
begin
  Self.Clear;
  tmp := Trim(aScheduleString);
  cur := 0;
  for i := 1 to Length(aScheduleString) do
  begin
    if aScheduleString[i] <> ' ' then
      lastStr := lastStr + aScheduleString[i]
    else
    begin
      if lastStr <> '' then
      begin
        if (cur >= ARRAY_SIZE) then
          RaiseWrongScheduleStringException(aScheduleString);
        Strings[cur] := lastStr;
        lastStr := '';
        inc(cur);
      end;
    end;
  end;
  if lastStr <> '' then
  begin
    if (cur >= ARRAY_SIZE) then
      RaiseWrongScheduleStringException(aScheduleString);
    Strings[cur] := lastStr;
  end;

  if cur < ARRAY_SIZE - 1 then
    RaiseWrongScheduleStringException(aScheduleString);
end;

{ TccScheduledTask }

procedure TccScheduledTask.SetScheduleString(AValue: String);
begin
  if FScheduleString=AValue then Exit;
  FScheduleString:=AValue;
  FDecodedSchedule.DecodeScheduleString(FScheduleString);
end;

constructor TccScheduledTask.Create;
begin
  FTaskProcedureOfObject := nil;
  FTaskProcedure:= nil;
  FScheduleString:= '';
  FTaskDescription:= '';
  FDecodedSchedule:= TccSchedulation.Create;
end;

destructor TccScheduledTask.Destroy;
begin
  FDecodedSchedule.Free;
  inherited Destroy;
end;

function TccScheduledTask.CheckValue (const aString : String; const aValue : word; out aAnyValueAllowed : boolean): boolean;
var
  i, k : integer;
  tmpValue1, tmpValue2 : integer;
  list : TStringList;
begin
  Result := false;
  aAnyValueAllowed:= false;

  if aString = '*' then
  begin
    aAnyValueAllowed:= true;
    Result := true;
    exit;
  end;

  i := Pos('/', aString);
  if i > 0 then
  begin
    if TryStrToInt(Copy(aString, i + 1, 999), tmpValue1) then
    begin
      if (aValue mod tmpValue1) = 0 then
        Result := true;
      exit;
    end
    else
      RaiseWrongScheduleStringException(aString);
  end;

  i := Pos('-', aString);
  if i > 0 then
  begin
    list := TStringList.Create;
    try
      list.Delimiter:= '-';
      list.DelimitedText:= aString;
      if list.Count <> 2 then
        RaiseWrongScheduleStringException(aString);
      if not TryStrToInt(Trim(list.Strings[0]), tmpValue1) then
        RaiseWrongScheduleStringException(aString);
      if not TryStrToInt(Trim(list.Strings[1]), tmpValue2) then
        RaiseWrongScheduleStringException(aString);

      if (aValue >= tmpValue1) and (aValue <= tmpValue2) then
        Result := true;
      exit;
    finally
      list.Free;
    end;
  end;

  i := Pos(',', aString);
  if i > 0 then
  begin
    list := TStringList.Create;
    try
      list.Delimiter:= ',';
      list.DelimitedText:= aString;
      for k := 0 to list.Count - 1 do
      begin
        if not TryStrToInt(Trim(list.Strings[k]), tmpValue1) then
          RaiseWrongScheduleStringException(aString);
        if aValue = tmpValue1 then
          Result := true;
        exit;
      end;
    finally
      list.Free;
    end;
  end;

  if TryStrToInt(aString, tmpValue1) then
  begin
    if (aValue = tmpValue1) then
      Result := true;
  end
  else
    RaiseWrongScheduleStringException(aString);
end;

function TccScheduledTask.MustRun(const aMinute, aHour, aDay, aMonth, aYear, aWeekday: word) : boolean;
var
  minOk, hourOk, dayOk, monthOk, weekdayOk, yearOk : boolean;
  anyMin, anyHour, anyDay, anyMonth, anyWeekday, anyYear : boolean;
begin
  Result := false;
  minOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.MINUTE_IDX], aMinute, anyMin);
  if not minOk then
    exit;
  hourOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.HOUR_IDX], aHour, anyHour);
  if not hourOk then
    exit;
  dayOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.DAY_IDX], aDay, anyDay);
  if not dayOk then
    exit;
  monthOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.MONTH_IDX], aMonth, anyMonth);
  if not monthOk then
    exit;
  weekdayOk:= CheckValue(FDecodedSchedule.Strings[TccSchedulation.WEEKDAY_IDX], aWeekDay, anyWeekDay);
  if not weekdayOk then
    exit;
  yearOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.YEAR_IDX], aYear, anyYear);
  if not yearOk then
    exit;


  minOk := (not anyMin) or (anyMin and (aMinute =0));
  if (not anyHour) and (not minOk) then
    exit;

  hourOk := (not anyHour) or (anyHour and (aHour = 0));
  if (not anyDay) and ((not hourOk) or (not minOk)) then
    exit;

  dayOk := (not anyDay) or (anyDay and (aDay=1));
  if (not anyMonth) and ((not hourOk) or (not minOk) or (not dayOk)) then
    exit;

  monthOk := (not anyMonth) or (anyMonth and (aMonth=1));
  if (not anyYear) and ((not hourOk) or (not minOk) or (not dayOk) or (not monthOk)) then
    exit;

  Result := true;
end;

function TccScheduledTask.MustRun(const aDateTime: TDateTime): boolean;
var
  hour, minute, year, month, day, weekday : word;
begin
  ccDecodeDateTime(aDateTime, hour, minute, year, month, day, weekday);
  Result := Self.MustRun(minute, hour, day, month, year, weekday);
end;

end.
