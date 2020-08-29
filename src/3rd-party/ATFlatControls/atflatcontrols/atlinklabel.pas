{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATLinkLabel;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
  {$define delphi}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
  {$endif}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, {$ifdef delphi}ShellAPI, Windows, Messages;{$endif}
  {$ifdef FPC}
  LclIntf;
  {$endif}

type
  { TATLabelLink }

  TATLabelLink = class(TLabel)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;

    FLink: string;
    FColorLinkNormal: TColor;
    FColorLinkMouseover: TColor;
    procedure SetLink(AValue: string);
    {$ifdef delphi}
    procedure CMMouseEnter(var msg: TMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage);
      message CM_MOUSELEAVE;
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Click; override;
    {$ifdef FPC}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    {$endif}
    {$ifdef delphi}
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    {$endif}
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property Link: string read FLink write SetLink;
    property ColorLinkNormal: TColor read FColorLinkNormal write FColorLinkNormal;
    property ColorLinkMouseover: TColor read FColorLinkMouseover write FColorLinkMouseover;
  end;

implementation

constructor TATLabelLink.Create(AOwner: TComponent);
begin
  inherited;
  Cursor:= crHandPoint;
  ShowHint:= true;
  ColorLinkNormal:= clBlue;
  ColorLinkMouseover:= clRed;
  Font.Color:= ColorLinkNormal;
end;

procedure TATLabelLink.SetLink(AValue: string);
begin
  if FLink=AValue then Exit;
  FLink:= AValue;
  Hint:= AValue;
end;

procedure TATLabelLink.Click;
begin
  if Link<>'' then
    {$ifdef FPC}
    OpenURL(Link);
    {$endif}
    {$ifdef delphi}
    ShellExecuteW(0, nil, PWideChar(Link), nil, nil, SW_SHOWNORMAL);
    {$endif}
  if Assigned(OnClick) then
    OnClick(Self);
end;

{$ifdef FPC}
procedure TATLabelLink.MouseEnter;
begin
  Font.Color:= ColorLinkMouseover;
  Font.Style:= Font.Style+[fsUnderline];
end;

procedure TATLabelLink.MouseLeave;
begin
  Font.Color:= ColorLinkNormal;
  Font.Style:= Font.Style-[fsUnderline];
end;
{$endif}

{$ifdef delphi}
procedure TATLabelLink.CMMouseEnter(var msg: TMessage);
begin
  DoMouseEnter;
end;

procedure TATLabelLink.CMMouseLeave(var msg: TMessage);
begin
  DoMouseLeave;
end;

procedure TATLabelLink.DoMouseEnter;
begin
  Invalidate;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TATLabelLink.DoMouseLeave;
begin
  Invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$endif}

end.

