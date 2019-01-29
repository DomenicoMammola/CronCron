unit ccCronTab;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Classes, contnrs;

// https://help.ubuntu.com/community/CronHowto

type

  TccTaskProcedure = procedure of Object;

  { TccCronTab }

  TccCronTab = class
  strict private
    FTasksList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask (const aScheduleString : String; const aTask : TccTaskProcedure);
  end;


  { FSchedulation }

  TccSchedulation = class
  strict private
    const ARRAY_SIZE = 5;
  public
    const MINUTE_IDX : integer = 0;
    const HOUR_IDX : integer = 1;
    const DAY_IDX : integer = 2;
    const MONTH_IDX : integer = 3;
    const YEAR_IDX : integer = 4;
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
    FTaskProcedure: TccTaskProcedure;
    FScheduleString : String;
    FDecodedSchedule : TccSchedulation;
    procedure SetScheduleString(AValue: String);
    function CheckValue (const aString : String; const aValue : word; out aAnyValueAllowed : boolean): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function MustRun (const aMinute, aHour, aDay, aMonth, aYear : word) : boolean;

    property TaskProcedure : TccTaskProcedure read FTaskProcedure write FTaskProcedure;
    property ScheduleString : String read FScheduleString write SetScheduleString;
  end;


implementation

uses
  sysutils, strutils;


procedure RaiseWrongScheduleStringException (const aValue : String);
begin
  raise Exception.Create('Wrong format of cron schedule string: ' + aValue);
end;

{ TccSchedulation }

constructor TccSchedulation.Create;
begin
  SetLength(Strings, ARRAY_SIZE - 1);
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
        inc(cur);
      end;
    end;
  end;
  if cur < ARRAY_SIZE then
    RaiseWrongScheduleStringException(aScheduleString);
end;

{ TccCronTab }

constructor TccCronTab.Create;
begin
  FTasksList := TObjectList.Create(true);
end;

destructor TccCronTab.Destroy;
begin
  FTasksList.Free;
  inherited Destroy;
end;

procedure TccCronTab.AddTask(const aScheduleString: String; const aTask: TccTaskProcedure);
begin

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
  FTaskProcedure := nil;
  FScheduleString:= '';
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
      begin
        Result := true;
        exit;
      end;
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
      begin
        Result := true;
        exit;
      end;
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
        begin
          Result := true;
          exit;
        end;
      end;
    finally
      list.Free;
    end;
  end;
end;

function TccScheduledTask.MustRun(const aMinute, aHour, aDay, aMonth, aYear: word) : boolean;
var
  minOk, hourOk, dayOk, monthOk : boolean;
  anyMin, anyHour, anyDay, anyMonth, anyYear : boolean;
  goOn : boolean;
begin
  Result := false;
  minOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.MINUTE_IDX], aMinute, anyMin);
  if not minOk then
    exit;
  goOn := ((not anyMin) or (aMinute=0));
  hourOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.HOUR_IDX], aHour, anyHour) and goOn;
  if not hourOk then
    exit;
  goOn := goOn and ((not anyHour) or (aHour = 0));
  dayOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.DAY_IDX], aDay, anyDay) and goOn;
  if not dayOk then
    exit;
  goOn := goOn and ((not anyDay) or (aDay = 1));
  monthOk := CheckValue(FDecodedSchedule.Strings[TccSchedulation.MONTH_IDX], aMonth, anyMonth) and goOn;
  if not monthOk then
    exit;
  goOn := goOn and ((not anyMonth) or (aMonth = 1));
  Result := CheckValue(FDecodedSchedule.Strings[TccSchedulation.YEAR_IDX], aYear, anyYear) and goOn;
end;

end.
