// Copyright (c) 2022 Dmitry Bulgakov, Yaroslava Bulgakova, Yaroslav Turovskiy
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

const
   n = 1000;  // Максимальное число миганий
   PlashkaHeight = 139;
   PlashkaWidth = 395;
   Plashki8Left: array[1..8] of longint = (16, 432, 848, 1264, 16, 432, 848, 1264);
   Plashki8Top: array[1..8] of longint = (16, 16, 16, 16, 176, 176, 176, 176);
   Plashki6Left: array[1..6] of longint = (16, 432, 848, 16, 432, 848);
   Plashki6Top: array[1..6] of longint = (16, 16, 16, 176, 176, 176);

type
   TIntegerArray = Array of Integer;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    TimerOnFirstTime: TTimer;
    TimerOn: TTimer; // Зажигает плашку
    TimerOff: TTimer; // Гасит плашку
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerOnFirstTimeTimer(Sender: TObject);
    procedure TimerOnTimer(Sender: TObject);
    procedure TimerOffTimer(Sender: TObject);
    procedure Show68Plashek(Sender: TObject; PlashkiCount: LongInt);
  private

  public

  end;

var
  Form1: TForm1;
  mas: TIntegerArray; // Последовательность мигания плашек
  s: String;

  seq: TIntegerArray;
  seq_N: LongInt;
  seq_str: TStringList;

  NowOn: boolean; // Сейчас активная плашка горит
  NowOff: boolean; // Сейчас активная плашка не горит
  now_flash_index: Integer; // Порядковый номер в очереди плашек
  act_num: Integer; // Порядковый номер активной плашки
  GameOver: boolean; // Эксперимент закончился
  now_ts: TDateTime; // Текущее время

implementation

uses Unit2, Unit3;

{$R *.lfm}

{ TForm1 }


procedure set_passive_panel(panel_num: Int64);
// Сделать пассивной панель
begin
  if panel_num in [1,2,3,4,5,6,7,8]
    then (Form1.FindComponent('Panel' + IntToStr(panel_num)) as TPanel).Color := ep.Plashka_Passive_Color;
end;


procedure set_active_panel(panel_num: Int64);
// Сделать активной панель
begin
  if panel_num in [1,2,3,4,5,6,7,8]
    then (Form1.FindComponent('Panel' + IntToStr(panel_num)) as TPanel).Color := ep.Plashka_Active_Color;
end;


procedure refresh_panels();
var
  i: LongInt;
begin
  for i := 1 to 8 do
    (Form1.FindComponent('Panel' + IntToStr(i)) as TPanel).Refresh;
end;


procedure set_target_panel(panel_num: Int64);
// Подсветить целевую плашку, все остальные пригасить
var
  i: LongInt;
  Panel: TPanel;
begin
  // У всех плашек уберем рамки
  for i := 1 to 8 do begin
    Panel := Form1.FindComponent('Panel' + IntToStr(i)) as TPanel;
    Panel.BevelColor := clDefault;
    Panel.BevelWidth := 1;
  end;
  // Установим рамку для целевой плашки
  // Если panel_num = 0, то ни у какой плашки рамка не установится
  if panel_num in [1,2,3,4,5,6,7,8] then begin
    Panel := Form1.FindComponent('Panel' + IntToStr(panel_num)) as TPanel;
    Panel.BevelColor := ep.Plashka_Target_BevelColor;
    Panel.BevelWidth := ep.Plashka_Target_BevelWidth;
  end;
  refresh_panels();
end;


procedure set_visual_elements_color();
var
  i: LongInt;
begin
  Form1.Color := ep.Form_Color;
  for i := 1 to 8 do
    set_passive_panel(i);
  set_target_panel(0);
  refresh_panels();
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  seq_str := TStringList.Create;
  seq_str.Delimiter := ',';
  refresh_panels();
  is_begin_exp := false;
  Form2.ShowModal;
  if is_begin_exp then begin // После выхода из Form2 (is_begin_exp = True)
    Show68Plashek(Sender, ep.Plashki_Count);
    set_target_panel(ep.Target_Plashka_Num);
    ShowMessage('Эксперимент начинается.');
    DataModule1.seans_insert(Now);
    seq_str.DelimitedText := Flash_Queue; // Сформируем массив очереди из цифр на основе строки с разделителями
    now_flash_index := 0;
    // Начинаем с того, что все плашки погашены
    GameOver := False;
    Button1.Enabled := False;
    Button2.Enabled := True;
    TimerOn.Interval := 0; // Выключим таймер горения
    TimerOnFirstTime.Interval := 0; // Выключим таймер горения первого раза выключен
    TimerOff.Interval := Round((ep.Flash_Light_Delay + ep.Flash_Pause_Delay) * 1000); // Взведем таймер паузы
  end;

end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  // Отключим таймеры
  TimerOff.Interval := 0;
  TimerOn.Interval := 0;
  now_flash_index := 0;
  GameOver := true; // Игра окончена
  ShowMessage('Эксперимент остановлен.');
  DataModule1.seans_update_end(0, Now);
  Button1.Enabled := True;
  Button2.Enabled := False;
  set_visual_elements_color();
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Button2.Enabled := False;
  Show68Plashek(Sender, ep.Plashki_Count);
  set_visual_elements_color();
end;


procedure TForm1.TimerOffTimer(Sender: TObject);
// Сработал таймер паузы - время паузы вышло
var
  is_plashka_target: LongInt;
begin
  now_ts := Now;
  if not GameOver then begin
    if now_flash_index = 0 then // Если пока никакая плашка не горела
      TimerOnFirstTime.Interval := Round(ep.Flash_Light_Delay * 1000); // Взведем таймер горения первого раза
    act_num := StrToInt64(seq_str[now_flash_index]);  // Укажем, какая плашка сейчас активна
    if act_num = ep.Target_Plashka_Num
      then is_plashka_target := 1
      else is_plashka_target := 0;
    set_active_panel(act_num); // Включим активную плашку
    refresh_panels(); // Перерисуем панели
    DataModule1.event_insert(current_seans_id, now_flash_index+1, act_num, is_plashka_target, now_ts);
  end else begin // GameOver = True
    // Отключим таймеры
    TimerOff.Interval := 0;
    TimerOn.Interval := 0;
    TimerOnFirstTime.Interval := 0; // Выключим таймер горения первого раза
    ShowMessage('Эксперимент закончен.');
    DataModule1.seans_update_end(1, Now);
    Button1.Enabled := True;
    Button2.Enabled := False;
    set_visual_elements_color();
  end;
end;


procedure TForm1.TimerOnTimer(Sender: TObject);
// Сработал таймер горения - время горения вышло
begin
  set_passive_panel(act_num); // Выключим активную плашку
  refresh_panels(); // Перерисуем панели
  if (now_flash_index < seq_str.Count-1) and not GameOver // Если еще не дошли до заключительной плашки
    then now_flash_index := now_flash_index+1 // Укажем, что теперь переходим на следующую плашку
    else GameOver := True; // Игра окончена
end;


procedure TForm1.TimerOnFirstTimeTimer(Sender: TObject);
// Сработал таймер горения первого раза - время горения первого раза вышло
begin
  // Взведем таймер горения - он будет по длительность как таймер паузы, но будет срабатывать с зазором -
  // на время первого горения плашки
  TimerOn.Interval := Round((ep.Flash_Light_Delay + ep.Flash_Pause_Delay) * 1000); // Взведем таймер горения
  TimerOnFirstTime.Interval := 0; // Выключим таймер горения первого раза
  set_passive_panel(act_num); // Выключим активную плашку
  refresh_panels(); // Перерисуем панели
  if (now_flash_index < seq_str.Count-1) and not GameOver // Если еще не дошли до заключительной плашки
    then now_flash_index := now_flash_index+1 // Укажем, что теперь переходим на следующую плашку
    else GameOver := True; // Игра окончена
end;


procedure TForm1.Show68Plashek(Sender: TObject; PlashkiCount: LongInt);
// Отобразить 8 плашек, задать их размер и местоположение
var
  i: LongInt;
  Panel: TPanel;
begin
  if PlashkiCount = 8 then begin
    for i := 1 to 8 do begin
      Panel := FindComponent('Panel' + IntToStr(i)) as TPanel;
      Panel.Width := PlashkaWidth;
      Panel.Height := PlashkaHeight;
      Panel.Left := Plashki8Left[i];
      Panel.Top := Plashki8Top[i];
      Panel.Visible := True;
    end;
  end
  else if PlashkiCount = 6 then begin
    for i := 1 to 6 do begin
      Panel := FindComponent('Panel' + IntToStr(i)) as TPanel;
      Panel.Width := PlashkaWidth;
      Panel.Height := PlashkaHeight;
      Panel.Left := Plashki6Left[i];
      Panel.Top := Plashki6Top[i];
      Panel.Visible := True;
    end;
    Panel7.Visible := False;
    Panel8.Visible := False;
  end;
end;

end.

