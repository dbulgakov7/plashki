// Copyright (c) 2022 Dmitry Bulgakov, Yaroslava Bulgakova, Yaroslav Turovskiy
// 
// This software is released under the MIT License.
// https://opensource.org/licenses/MIT

unit Unit2;

{$mode objfpc}{$H+}

interface

uses
      Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    RadioGroup68: TRadioGroup;
    rb6: TRadioButton;
    rb8: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure LabeledEdit4Change(Sender: TObject);
    function get_pl_cnt(): LongInt;
    procedure rb6Change(Sender: TObject);
    procedure rb8Change(Sender: TObject);
    procedure set_rb_pl_cnt(pl_cnt: LongInt);
  private

  public

  end;

var
      Form2: TForm2;

implementation

uses Unit3;

{$R *.lfm}

function TForm2.get_pl_cnt(): LongInt;
begin
  if rb6.Checked then Result := 6;
  if rb8.Checked then Result := 8;
end;


procedure TForm2.set_rb_pl_cnt(pl_cnt: LongInt);
begin
  if pl_cnt = 6 then rb6.Checked := True;
  if pl_cnt = 8 then rb8.Checked := True;
end;

function all_nums_is_ok(): Boolean;
// Проверить, что все числа введены корректно
begin
  if str_is_int(Form2.LabeledEdit2.Text)
    and str_is_float(Form2.LabeledEdit3.Text)
    and str_is_float(Form2.LabeledEdit4.Text)
    and (Form2.rb6.Checked or Form2.rb8.Checked)
  then Result := True
  else Result := False;
end;

procedure calc_all_time();
var
  pl_cnt: LongInt; // Кол-во плашек, шт.
  mignet_cnt: LongInt; // Мигнет каждая, раз
  pause_time: Extended; // Пазуа между миганиями, с
  active_time: Extended; // Время подсветки активной плашки, c
  all_time: Extended; // Общее время
begin
  pl_cnt := Form2.get_pl_cnt();
  mignet_cnt := StrToInt(Form2.LabeledEdit2.Text);
  pause_time := StrToFloat(Form2.LabeledEdit3.Text);
  active_time := StrToFloat(Form2.LabeledEdit4.Text);
  all_time := pl_cnt * mignet_cnt * (pause_time + active_time);
  Form2.LabeledEdit5.Text := FloatToStr(all_time);
end;


{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
// Отмена
begin
  is_begin_exp := False;
  Form2.Close;
end;

procedure TForm2.Button3Click(Sender: TObject);
// Старт
begin
  if all_nums_is_ok() then begin
    calc_all_time();
    ep.Plashki_Count := Form2.get_pl_cnt();
    ep.Every_Plashka_Flash_Count := StrToInt(Form2.LabeledEdit2.Text);
    ep.Flash_Pause_Delay := StrToFloat(Form2.LabeledEdit3.Text);
    ep.Flash_Light_Delay := StrToFloat(Form2.LabeledEdit4.Text);
    seq_init(ep.Plashki_Count, ep.Every_Plashka_Flash_Count);
    seq_mix();
    ep.Target_Plashka_Num := Random(6)+1;
    is_begin_exp := True;
    Form2.Close;
  end
  else
    ShowMessage('Исправьте параметры эксперимента.');
end;


procedure TForm2.FormCreate(Sender: TObject);
begin
  set_rb_pl_cnt(ep.Plashki_Count);
  LabeledEdit1.Text := ep.Subject_FIO;
  LabeledEdit2.Text := IntToStr(ep.Every_Plashka_Flash_Count);
  LabeledEdit3.Text := FloatToStr(ep.Flash_Pause_Delay);
  LabeledEdit4.Text := FloatToStr(ep.Flash_Light_Delay);
end;

procedure TForm2.LabeledEdit1Change(Sender: TObject);
begin
  ep.Subject_FIO := LabeledEdit1.Text;
end;


procedure TForm2.LabeledEdit2Change(Sender: TObject);
begin
  if str_is_int(Form2.LabeledEdit2.Text)
    then Form2.LabeledEdit2.Font.Color := clDefault
    else Form2.LabeledEdit2.Font.Color := clRed;
  if all_nums_is_ok()
    then calc_all_time()
    else LabeledEdit5.Text := '-';
end;

procedure TForm2.LabeledEdit3Change(Sender: TObject);
begin
  if str_is_float(Form2.LabeledEdit3.Text)
    then Form2.LabeledEdit3.Font.Color := clDefault
    else Form2.LabeledEdit3.Font.Color := clRed;
  if all_nums_is_ok()
    then calc_all_time()
    else LabeledEdit5.Text := '-';
end;

procedure TForm2.LabeledEdit4Change(Sender: TObject);
begin
  if str_is_float(Form2.LabeledEdit4.Text)
    then Form2.LabeledEdit4.Font.Color := clDefault
    else Form2.LabeledEdit4.Font.Color := clRed;
  if all_nums_is_ok()
    then calc_all_time()
    else LabeledEdit5.Text := '-';
end;

procedure TForm2.rb6Change(Sender: TObject);
begin
  if all_nums_is_ok()
    then calc_all_time()
    else LabeledEdit5.Text := '-';
end;

procedure TForm2.rb8Change(Sender: TObject);
begin
  if all_nums_is_ok()
    then calc_all_time()
    else LabeledEdit5.Text := '-';
end;


end.

