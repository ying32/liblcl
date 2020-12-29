//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uGoForm;

{$mode objfpc}{$H+}
{$I ExtDecl.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, LMessages, LCLType, fgl;

type
  // 消息过程定义
  // 重定一个，主要是为了修改相关默认
  TWndProcEvent = procedure(Sender: TObject; var TheMessage: TLMessage) of object;

  { TGoForm }

  TGoForm = class(TForm)
  private
    FOnWndProc: TWndProcEvent;
    FGoPtr: Pointer;
  protected
    procedure ProcessResource; override;
    procedure WndProc(var TheMessage: TLMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor CreateFromClassName(TheOwner: TComponent; const AClassName: string);
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    procedure ScaleForPPI(ANewPPI: Integer);
    procedure ScaleForCurrentDpi;

    procedure InheritedWndProc(var TheMessage: TLMessage);

    // 自定义一些
    procedure EnabledMaximize(AValue: Boolean);
    procedure EnabledMinimize(AValue: Boolean);
    procedure EnabledSystemMenu(AValue: Boolean);

    procedure ScreenCenter;
    procedure WorkAreaCenter;

    class function Create2(AOwner: TComponent; AInitScale: Boolean): TGoForm;

    property GoPtr: Pointer read FGoPtr write FGoPtr;
  published
    property OnWndProc: TWndProcEvent read FOnWndProc write FOnWndProc;
  end;

  TRequestCallCreateParams = function(goPtr: Pointer; var Params: TCreateParams): Pointer; extdecl;

var
  GRequestCallCreateParamsPtr: TRequestCallCreateParams = nil;

implementation

type
  TFormResouces = specialize TFPGMap<string, Pointer>;

var
  uFormRes: TFormResouces;


constructor TGoForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
end;

procedure TGoForm.ScaleForPPI(ANewPPI: Integer);
begin
  if ANewPPI < 30 then
    Exit;
  if ANewPPI <> PixelsPerInch then
  begin
    AutoAdjustLayout(lapAutoAdjustForDPI, PixelsPerInch, ANewPPI,
      MulDiv(Width, ANewPPI, PixelsPerInch),
      MulDiv(Height, ANewPPI, PixelsPerInch));
  end;
end;

procedure TGoForm.ScaleForCurrentDpi;
begin
  if not Scaled then
  begin
    Scaled := True;
    Exit;
  end;
  if PixelsPerInch <> Monitor.PixelsPerInch then
  begin
    AutoAdjustLayout(lapAutoAdjustForDPI, PixelsPerInch, Monitor.PixelsPerInch,
      MulDiv(Width, Monitor.PixelsPerInch, PixelsPerInch),
      MulDiv(Height, Monitor.PixelsPerInch, PixelsPerInch));
  end;
end;

procedure TGoForm.InheritedWndProc(var TheMessage: TLMessage);
begin
  inherited WndProc(TheMessage);
end;

procedure TGoForm.EnabledMaximize(AValue: Boolean);
begin
  if AValue then
  begin
    if not(biMaximize in BorderIcons) then
      BorderIcons := BorderIcons + [biMaximize]
  end else
  begin
    if biMaximize in BorderIcons then
      BorderIcons := BorderIcons - [biMaximize]
  end;
end;

procedure TGoForm.EnabledMinimize(AValue: Boolean);
begin
  if AValue then
  begin
    if not(biMinimize in BorderIcons) then
      BorderIcons := BorderIcons + [biMinimize]
  end else
  begin
    if biMinimize in BorderIcons then
      BorderIcons := BorderIcons - [biMinimize]
  end;
end;

procedure TGoForm.EnabledSystemMenu(AValue: Boolean);
begin
  if AValue then
  begin
    if not(biSystemMenu in BorderIcons) then
      BorderIcons := BorderIcons + [biSystemMenu]
  end else
  begin
    if biSystemMenu in BorderIcons then
      BorderIcons := BorderIcons - [biSystemMenu]
  end;
end;

procedure TGoForm.ScreenCenter;
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TGoForm.WorkAreaCenter;
begin
  Left := (Screen.WorkAreaWidth - Width) div 2;
  Top := (Screen.WorkAreaHeight - Height) div 2;
end;

class function TGoForm.Create2(AOwner: TComponent; AInitScale: Boolean): TGoForm;
begin
  Result := TGoForm.Create(AOwner);
end;

procedure TGoForm.ProcessResource;
begin
  Self.ClientHeight := 321;
  Self.ClientWidth := 678;
  // 没有使用窗口资源，不处理，处理就会报错的。
end;

procedure TGoForm.WndProc(var TheMessage: TLMessage);
begin
  if Assigned(FOnWndProc) then
    FOnWndProc(Self, TheMessage)
  else
    inherited WndProc(TheMessage);
end;

procedure TGoForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Assigned(GRequestCallCreateParamsPtr) then
    GRequestCallCreateParamsPtr(FGoPtr, Params);
end;

constructor TGoForm.CreateFromClassName(TheOwner: TComponent;
  const AClassName: string);
begin

  inherited Create(TheOwner);
end;

initialization

finalization

end.

