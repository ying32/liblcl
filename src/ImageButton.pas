//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

///=======================================================
/// 功能：图像状态按钮
///
/// 注： 一般使推荐使用png图片，当然jpg、bmp也是可以的。
///      一张png图的排序为 Normal, Hover, Down, Disabled，其中
///      Disabled状态可忽略
///  Orientation 分为横向排列和纵向排列的图片
///=======================================================

unit ImageButton;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LMessages,
{$ELSE}
  Winapi.Messages,
{$ENDIF}
  Classes,
  SysUtils,
  Types,
  Controls,
  Graphics,
  Forms;

type

{$IFNDEF FPC}
  TLMessage = TMessage;
{$ENDIF}

  { TImageButton }

  TImageOrientation = (ioHorizontal, ioVertical);

  TImageButton = class(TGraphicControl)
  private type
    TButtonState = (bsNormal, bsHover, bsDown, bsDisabled);
  private
    FOrientation: TImageOrientation;
    FState: TButtonState;
    FPicture: TPicture;
    FImageCount: Integer;
    FCaption: string;
    FShowCaption: Boolean;
    FFont: TFont;
    FWordwarp: Boolean;
    FModalResult: TModalResult;
    FMouseLeave: Boolean;
    FCacheBmps: TImageList;

    procedure SetOrientation(AValue: TImageOrientation);
    procedure UpdateCacheBmps;

    procedure SetPicture(const Value: TPicture);
    procedure OnPictureChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure SetImageCount(const Value: Integer);
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure SetCaption(const Value: string);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetWordwarp(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure SetEnabled(Value: Boolean); override;

    procedure Paint; override;
    procedure ResetSize;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property Caption: string read FCaption write SetCaption;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property ImageCount: Integer read FImageCount write SetImageCount default 3;
    property Orientation: TImageOrientation read FOrientation write SetOrientation default ioHorizontal;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property ParentShowHint;
    property ParentFont;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property ShowHint;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property Visible;
    property Wordwarp: Boolean read FWordwarp write SetWordwarp;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation


function IfThen(ABool: Boolean; ATrue, AFalse: Integer): Integer; inline;
begin
  if ABool then Result := ATrue else Result := AFalse;
end;

{ TImageButton }

constructor TImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCacheBmps := TImageList.Create(Self);
  FPicture := TPicture.Create;
  FPicture.OnChange := {$IFDEF FPC}@{$ENDIF}OnPictureChanged;
  FFont := TFont.Create;
  FFont.OnChange := {$IFDEF FPC}@{$ENDIF}OnFontChanged;
  FState := bsNormal;
  FOrientation := ioHorizontal;
  FImageCount := 3;
  Width := 80;
  Height := 25;
  FShowCaption := False;
  FCaption := '';
  FModalResult := mrNone;
end;

destructor TImageButton.Destroy;
begin
  FCacheBmps.Free;
  FFont.Free;
  FPicture.Free;
  inherited;
end;

procedure TImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseLeave := False;
  if csDesigning in ComponentState then Exit;
  if not Enabled then Exit;
  FState := bsDown;
  Invalidate;
end;

procedure TImageButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if csDesigning in ComponentState then Exit;
  if not Enabled then Exit;
  if FState = bsDown then Exit;
  if FState <> bsHover then
  begin
    FState := bsHover;
    Invalidate;
  end;
end;

procedure TImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if not Enabled then Exit;
  if not FMouseLeave then
    FState := bsHover
  else FState := bsNormal;
  Invalidate;
end;

procedure TImageButton.OnFontChanged(Sender: TObject);
begin
  inherited Canvas.Font.Assign(Font);
  Invalidate;
end;

procedure TImageButton.OnPictureChanged(Sender: TObject);
begin
  FCacheBmps.Clear;
  UpdateCacheBmps;
  ResetSize;
  FState := bsNormal;  // 重设状态
  Invalidate;
end;

procedure TImageButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TImageButton.Paint;
{$IFDEF FPC}
  procedure DrawText;
  var
    LR: TRect;
    LStyle: TTextStyle;
  begin
    if FShowCaption then
    begin
      LR := ClientRect;
      inherited Canvas.Brush.Style := bsClear;
      LStyle.Alignment := taCenter;
      LStyle.Layout := tlCenter;
      LStyle.Wordbreak := FWordwarp;
      LStyle.SingleLine := not FWordwarp;
      Canvas.TextRect(LR, 0, 0, FCaption, LStyle);
    end;
  end;
{$ELSE}
  procedure DrawText;
  begin
    // 只用于在GenlibLcl工具中生成代码用
  end;
{$ENDIF}

var
  R: TRect;
begin
  inherited Paint;

  if not Visible then Exit;
  if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Self.Width-1, Self.Height-1);//????
    end;
  end;

  if FPicture.Graphic = nil then
  begin
    DrawText;
    Exit;
  end;
{$ifdef fpc}
  R := ClientRect;
  case FCacheBmps.Count of
    1: FCacheBmps.StretchDraw(Canvas, 0, R);
    2:
      begin
        if FState = bsNormal then
          FCacheBmps.StretchDraw(Canvas, 0, R)
        else
          FCacheBmps.StretchDraw(Canvas, 1, R)
      end;
    3:
      begin
        if FState = bsNormal then
          FCacheBmps.StretchDraw(Canvas, 0, R)
        else if FState = bsHover then
          FCacheBmps.StretchDraw(Canvas, 1, R)
        else
          FCacheBmps.StretchDraw(Canvas, 2, R)
      end;
    4,5:
      begin
        if FState = bsNormal then
          FCacheBmps.StretchDraw(Canvas, 0, R)
        else if FState = bsHover then
          FCacheBmps.StretchDraw(Canvas, 1, R)
        else if FState = bsDown then
          FCacheBmps.StretchDraw(Canvas, 2, R)
        else
          FCacheBmps.StretchDraw(Canvas, 3, R)
      end;
  end;
{$endif}
  DrawText;
end;

procedure TImageButton.ResetSize;
begin
  if AutoSize then
  begin
    if (FPicture.Graphic <> nil) and ((FPicture.Width > 0) and (FPicture.Height > 0)) then
    begin
      if Align in [alNone, alLeft, alRight] then
        Width := FCacheBmps.Width; //IfThen(FOrientation = ioHorizontal, FPicture.Width div FImageCount, FPicture.Heigh;
      if Align in [alNone, alTop, alBottom] then
        Height := FCacheBmps.Height;//FPicture.Height;
    end;
  end;
end;

procedure TImageButton.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  ResetSize;
end;

procedure TImageButton.Resize;
begin
  inherited;
  Invalidate;
end;


procedure TImageButton.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if FShowCaption then
      Invalidate;
  end;
end;

procedure TImageButton.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  if not Value then
    FState := bsDisabled
  else FState := bsNormal;
  Invalidate;
end;

procedure TImageButton.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TImageButton.SetImageCount(const Value: Integer);
begin
  if FImageCount <> Value then
  begin
    FImageCount := Value;
    if FImageCount <= 0 then
      FImageCount := 1;
    if FImageCount > 5 then
      FImageCount := 5; // 5状态没有，先弄个吧
    FState := bsNormal;  // 重设状态
    FCacheBmps.Clear;
    UpdateCacheBmps;
    ResetSize;
    Invalidate;
  end;
end;

procedure TImageButton.UpdateCacheBmps;
{$ifdef fpc}
  procedure ReAdd;
  begin
    if FCacheBmps.Count <> 0 then
      Exit;
    FCacheBmps.BeginUpdate;
    try
      FCacheBmps.AddSliced(FPicture.Bitmap, IfThen(FOrientation = ioHorizontal, FImageCount, 1), IfThen(FOrientation = ioHorizontal, 1, FImageCount));
    finally
      FCacheBmps.EndUpdate;
    end;
  end;
{$endif}

var
  LW, LH: Integer;
begin
  if FPicture.Graphic = nil then
  begin
    FCacheBmps.Clear;
    Exit;
  end;
  LW := IfThen(FOrientation = ioHorizontal, FPicture.Width div FImageCount, FPicture.Width);
  LH := IfThen(FOrientation = ioHorizontal, FPicture.Height, FPicture.Height div FImageCount);
  FCacheBmps.Width := LW;
  FCacheBmps.Height := LH;
{$ifdef fpc}
  ReAdd;
  //// 貌似是个bug
  if FCacheBmps.Count <> FImageCount then
  begin
    FCacheBmps.Clear;
    ReAdd;
  end;
{$endif}
end;

procedure TImageButton.SetOrientation(AValue: TImageOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  ResetSize;
  FCacheBmps.Clear;
  UpdateCacheBmps;
  Invalidate;
end;

procedure TImageButton.SetPicture(const Value: TPicture);
begin
  FCacheBmps.Clear;
  FPicture.Assign(Value);
end;

procedure TImageButton.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TImageButton.SetWordwarp(const Value: Boolean);
begin
  if FWordwarp <> Value then
  begin
    FWordwarp := Value;
    if FShowCaption then
      Invalidate;
  end;
end;

procedure TImageButton.CMMouseEnter(var Message: TLMessage);
begin
  FMouseLeave := False;
  if csDesigning in ComponentState then Exit;
  if not Enabled then Exit;
  FState := bsHover;
  Invalidate;
end;

procedure TImageButton.CMMouseLeave(var Message: TLMessage);
begin
  FMouseLeave := True;
  if csDesigning in ComponentState then Exit;
  if not Enabled then Exit;
  FState := bsNormal;
  Invalidate;
end;

end.
