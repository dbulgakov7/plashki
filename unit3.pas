// Copyright (c) 2022 Dmitry Bulgakov, Yaroslava Bulgakova, Yaroslav Turovskiy
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, IniFiles, RegExpr, SQLite3Conn, SQLDB,
  DB, DateUtils;

const
  pathINI: String = 'plashki.ini';
  db_filename = 'plashki.db3';

type
  TIntegerArray = Array of Integer; // Массив переменной длины из целых чисел
  TExperimentParams = record // Параметры плашек и форм
    Form_Color: LongInt;  // Цвет главной формы
    Plashka_Active_Color: LongInt; // Цвет активной плашки
    Plashka_Passive_Color: LongInt;  // Цвет пассивной плашки
    Plashka_Target_BevelColor: LongInt; // Цвет рамки активной плашки
    Plashka_Target_BevelWidth: LongInt; // Толщина рамки активной плашки
    Target_Plashka_Num: LongInt; // Номер целевой плашки (которую обведем в рамку)
    Plashki_Count: LongInt; // Количество плашек на форме
    Every_Plashka_Flash_Count: LongInt; // Сколько раз должна мигнуть каждая плашка, кол-во раз
    Flash_Light_Delay: Double; // Сколько времени горит активная плашка, сек
    Flash_Pause_Delay: Double; // Сколько времени плашки не горят, сек
    Subject_FIO: String; // ФИО испытуемого
  end;


  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    conn: TSQLite3Connection;
    ds1: TDataSource;
    sql_temp: TSQLQuery;
    trans: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ConnectToSQLite();
    function seans_insert(): LongInt;
    function event_insert(seans_id: LongInt; flash_num: LongInt; plashka_num: LongInt; is_plashka_target: LongInt; ts: TDateTime): LongInt;
    procedure seans_update_end(is_seans_completely: LongInt);
  private

  public
  end;

var
  DataModule1: TDataModule1;
  ep: TExperimentParams; // Параметры эксперимента
  Flash_Queue: String; // Очередь мигания
  sequence_array: TIntegerArray; // Последовательность мигания
  sequence_length: LongInt; // Длина последовательности мигания
  is_begin_exp: Boolean;
  current_seans_id: LongInt; // Идентификатор текущего сеанса

// Опишем в разделе interface эти процедуры и функции, чтобы их
// можно было вызывать из других модулей
procedure seq_init(pl_cnt: LongInt; mignet_cnt: LongInt);
procedure seq_mix();
function seq_to_str(): String;
function str_is_int(str: String): Boolean;
function str_is_float(str: String): Boolean;

implementation

{$R *.lfm}

procedure TDataModule1.ConnectToSQLite();
var
  seans_sql, event_sql: string;
begin
  conn.DatabaseName := db_filename;
  trans.StartTransaction; // opens Connect, EInOutError if SQLite not installed
  seans_sql := 'CREATE TABLE if not exists seans (id INTEGER PRIMARY KEY AUTOINCREMENT,'
              +'begin_seans TEXT,'
              +'fio TEXT, plashki_count integer, target_plashka_num integer, flash_pause_delay float,'
              +'flash_light_delay float, every_plashka_flash_count integer,'
              +'end_seans TEXT, '
              +'is_seans_completely integer,'
              +'flash_queue_json TEXT, colors_json TEXT'
              +');';
  event_sql := 'CREATE TABLE if not exists event ('
              +'id INTEGER PRIMARY KEY AUTOINCREMENT,'
              +'seans_id INTEGER, '
              +'flash_num INTEGER, '
              +'datetime TEXT,'
              +'plashka_num INTEGER, is_plashka_target INTEGER,'
              +'CONSTRAINT EVENT_FK FOREIGN KEY (seans_id) REFERENCES seans(id) ON DELETE CASCADE ON UPDATE CASCADE);';
  conn.ExecuteDirect(seans_sql);
  conn.ExecuteDirect(event_sql);
  trans.Commit;
end;

function TDataModule1.seans_insert(): LongInt;
begin
  sql_temp.SQL.Text := 'INSERT INTO seans (begin_seans, fio, plashki_count, target_plashka_num, flash_pause_delay, flash_light_delay, every_plashka_flash_count, flash_queue_json, colors_json)'
      +'values (:begin_seans, :fio, :plashki_count, :target_plashka_num, :flash_pause_delay, :flash_light_delay, :every_plashka_flash_count, :flash_queue_json, :colors_json)';
  sql_temp.Params.ParamByName('begin_seans').AsString := FormatDateTime('YYYY-MM-DD hh:nn:ss.zzz', Now());
  sql_temp.Params.ParamByName('fio').AsString := ep.Subject_FIO;
  sql_temp.Params.ParamByName('plashki_count').AsInteger := ep.Plashki_Count;
  sql_temp.Params.ParamByName('target_plashka_num').AsInteger := ep.Target_Plashka_Num;
  sql_temp.Params.ParamByName('flash_pause_delay').AsFloat := ep.Flash_Pause_Delay;
  sql_temp.Params.ParamByName('flash_light_delay').AsFloat := ep.Flash_Light_Delay;
  sql_temp.Params.ParamByName('every_plashka_flash_count').AsFloat := ep.Every_Plashka_Flash_Count;
  sql_temp.Params.ParamByName('flash_queue_json').AsString := '[' + Flash_Queue + ']';
  sql_temp.Params.ParamByName('colors_json').AsString := '{' +
    '"Form_Color": ' + IntToStr(ep.Form_Color) + ',' +
    ' "Plashka_Active_Color": ' + IntToStr(ep.Plashka_Active_Color) + ',' +
    ' "Plashka_Passive_Color": ' + IntToStr(ep.Plashka_Passive_Color) + ',' +
    ' "Plashka_Target_BevelColor": ' + IntToStr(ep.Plashka_Target_BevelColor) + ',' +
    ' "Plashka_Target_BevelWidth": ' + IntToStr(ep.Plashka_Target_BevelWidth) + '}';
  sql_temp.ExecSQL;
  trans.Commit;
  current_seans_id := conn.GetInsertID;
  Result := current_seans_id;
end;

function TDataModule1.event_insert(seans_id: LongInt; flash_num: LongInt; plashka_num: LongInt; is_plashka_target: LongInt; ts: TDateTime): LongInt;
begin
  sql_temp.SQL.Text := 'INSERT INTO event (seans_id, flash_num, plashka_num, is_plashka_target, datetime)'
      +'values (:seans_id, :flash_num, :plashka_num, :is_plashka_target, :ts)';
  sql_temp.Params.ParamByName('seans_id').AsInteger := seans_id;
  sql_temp.Params.ParamByName('flash_num').AsInteger := flash_num;
  sql_temp.Params.ParamByName('plashka_num').AsInteger := plashka_num;
  sql_temp.Params.ParamByName('is_plashka_target').AsInteger := is_plashka_target;
  sql_temp.Params.ParamByName('ts').AsString := FormatDateTime('YYYY-MM-DD hh:nn:ss.zzz', ts);
  sql_temp.ExecSQL;
  trans.Commit;
  Result := conn.GetInsertID;
end;

procedure TDataModule1.seans_update_end(is_seans_completely: LongInt);
begin
  sql_temp.SQL.Text := 'UPDATE seans '
      +'set end_seans = :end_seans, '
      +'is_seans_completely = :is_seans_completely WHERE id = :id';
  sql_temp.Params.ParamByName('end_seans').AsString := FormatDateTime('YYYY-MM-DD hh:nn:ss.zzz', Now());
  sql_temp.Params.ParamByName('is_seans_completely').AsInteger := is_seans_completely;
  sql_temp.Params.ParamByName('id').AsInteger := current_seans_id;
  sql_temp.ExecSQL;
  trans.Commit;
end;

function str_is_int(str: String): Boolean;
// Проверить, что строка является корректным Int
var
  RegexObjI: TRegExpr;
begin
  RegexObjI := TRegExpr.Create;
  RegexObjI.Expression := '^[0-9]+$';
  if RegexObjI.Exec(str)
    then Result := True
    else Result := False;
  RegexObjI.Free;
end;


function str_is_float(str: String): Boolean;
// Проверить, что строка является корректным Float
var
  RegexObjF: TRegExpr;
  RegexObjI: TRegExpr;
begin
  RegexObjF := TRegExpr.Create;
  RegexObjF.Expression := '^[0-9]+,?[0-9]+$';
  RegexObjI := TRegExpr.Create;
  RegexObjI.Expression := '^[0-9]+$';
  if RegexObjF.Exec(str) or RegexObjI.Exec(str)
    then Result := True
    else Result := False;
  RegexObjI.Free;
  RegexObjF.Free;
end;


procedure read_config_from_ini();
// Прочитать параметры из ini-файла
var
  sIniFile: TIniFile;
begin
  if FileExists(pathINI) then
    begin
      sIniFile := TIniFile.Create(pathINI);
      try
        // Прочитаем параметры
        ep.Form_Color := sIniFile.ReadInteger('Colors', 'Form_Color', clSilver);
        ep.Plashka_Active_Color := sIniFile.ReadInteger('Colors', 'Plashka_Active_Color', clWhite);
        ep.Plashka_Passive_Color := sIniFile.ReadInteger('Colors', 'Plashka_Passive_Color', clGray);
        ep.Plashka_Target_BevelColor := sIniFile.ReadInteger('Colors', 'Plashka_Target_BevelColor', clGreen);
        ep.Plashka_Target_BevelWidth := sIniFile.ReadInteger('Colors', 'Plashka_Target_BevelWidth', 4);
        ep.Target_Plashka_Num := sIniFile.ReadInteger('Flash', 'Target_Plashka_Num', 1);
        ep.Plashki_Count := sIniFile.ReadInteger('Sequence', 'Plashki_Count', 8);
        ep.Every_Plashka_Flash_Count := sIniFile.ReadInteger('Sequence', 'Every_Plashka_Flash_Count', 1);
        ep.Flash_Light_Delay := sIniFile.ReadFloat('Sequence', 'Flash_Light_Delay', 1.0);
        ep.Flash_Pause_Delay := sIniFile.ReadFloat('Sequence', 'Flash_Pause_Delay', 2.0);
        Flash_Queue := sIniFile.ReadString('Sequence', 'Flash_Queue', '1,2,3');
        ep.Subject_FIO := sIniFile.ReadString('Subject', 'FIO', '-');
      finally
        sIniFile.Free;
      end;
  end;
  DataModule1.ConnectToSQLite();
end;

procedure save_config_to_ini();
// Записать параметры в ini-файл
var
  sIniFile: TIniFile;
begin
  if FileExists(pathINI) then
    begin
      sIniFile := TIniFile.Create(pathINI);
      try
        // Запишем в INI строки
        sIniFile.WriteInteger('Colors', 'Form_Color', ep.Form_Color);
        sIniFile.WriteInteger('Colors', 'Plashka_Active_Color', ep.Plashka_Active_Color);
        sIniFile.WriteInteger('Colors', 'Plashka_Passive_Color', ep.Plashka_Passive_Color);
        sIniFile.WriteInteger('Colors', 'Plashka_Target_BevelColor', ep.Plashka_Target_BevelColor);
        sIniFile.WriteInteger('Colors', 'Plashka_Target_BevelWidth', ep.Plashka_Target_BevelWidth);
        sIniFile.WriteInteger('Flash', 'Target_Plashka_Num', ep.Target_Plashka_Num);
        sIniFile.WriteInteger('Sequence', 'Plashki_Count', ep.Plashki_Count);
        sIniFile.WriteInteger('Sequence', 'Every_Plashka_Flash_Count', ep.Every_Plashka_Flash_Count);
        sIniFile.WriteFloat('Sequence', 'Flash_Light_Delay', ep.Flash_Light_Delay);
        sIniFile.WriteFloat('Sequence', 'Flash_Pause_Delay', ep.Flash_Pause_Delay);
        sIniFile.WriteString('Sequence', 'Flash_Queue', seq_to_str());
        sIniFile.WriteString('Subject', 'FIO', ep.Subject_FIO);
      finally
        sIniFile.Free;
      end;
    end;
end;


function seq_to_str(): String;
// Преобразуем последовательность в строку
var
  i: LongInt;
begin
  Result := '';
  for i := 0 to sequence_length-1 do
    begin
      Result := Result + IntToStr(sequence_array[i]);
      if i < sequence_length-1
        then Result := Result + ',';
    end;
end;


procedure seq_init(pl_cnt: LongInt; mignet_cnt: LongInt);
// Создадим последовательность из подряд идущих плашек: 1,2,3,1,2,3
var
  i, j, seq_num: LongInt;
begin
  sequence_length := pl_cnt * mignet_cnt;
  SetLength(sequence_array, sequence_length);
  for i := 1 to mignet_cnt do
    for j := 1 to pl_cnt do
      begin
        seq_num := (i-1) * pl_cnt + j - 1;
        sequence_array[seq_num] := j;
      end;
  Flash_Queue := seq_to_str();
end;


procedure seq_mix();
// Перемешаем последовательность, не оставив никакой элемент на своем месте
var
   i, r, h : Integer;
begin
  Randomize;
  for i := sequence_length-1 downto 0 do begin
    r := Random(i) + 1; // выбираем случайный элемент
    if r <> i then
      begin
        // меняем элементы друг с другом
        h := sequence_array[i];
        sequence_array[i] := sequence_array[r];
        sequence_array[r] := h;
      end;
  end;
  Flash_Queue := seq_to_str();
end;

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  read_config_from_ini();
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  save_config_to_ini();
end;

end.

