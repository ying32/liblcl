{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtons;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls, Menus,
  Types, Math, Forms, ExtCtrls,
  {$ifdef FPC}
  LCLType,
  {$else}
  Windows, Messages,
  {$endif}
  ATFlatThemes,
  ATCanvasPrimitives;


type
  TATButtonKind = (
    abuTextOnly,
    abuIconOnly,
    abuTextIconHorz,
    abuTextIconVert,
    abuSeparatorHorz,
    abuSeparatorVert,
    abuTextChoice
    );

const
  cATButtonKindValues: array[TATButtonKind] of string = (
    'text',
    'icon',
    'text_icon_h',
    'text_icon_v',
    'sep_h',
    'sep_v',
    'text_choice'
    );

const
  cDefaultButtonPadding = 4;
  cDefaultButtonPaddingBig = 5;

type
  TATButtonArrowKind = (
    abakArrowDown,
    abakArrowLeft,
    abakArrowRight,
    abakCross
    );

type
  { TATButton }

  TATButton = class(TCustomControl)
  private
    {$ifndef FPC}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    {$endif}

    FPressed: boolean;
    FOver: boolean;
    FChecked: boolean;
    FCheckable: boolean;
    FFocusable: boolean;
    FPicture: TPicture;
    FImages: TImageList;
    FImageIndex: integer;
    FArrow: boolean;
    FArrowKind: TATButtonArrowKind;
    FArrowAlign: TAlignment;
    FFlat: boolean;
    FKind: TATButtonKind;
    FBoldBorder: boolean;
    FBoldFont: boolean;
    FDataString: string;
    FDataString2: string;
    FDataString3: string;
    FItems: TStringList;
    FItemsShort: TStringList;
    FItemIndex: integer;
    FPopup: TPopupMenu;
    FPadding: integer;
    FPaddingBig: integer;
    FTheme: PATFlatTheme;
    FWidthInitial: integer;
    FTextOverlay: string;
    FTextAlign: TAlignment;
    FShowShortItems: boolean;

    {$ifndef FPC}
    procedure CMMouseEnter(var msg: TMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage);
      message CM_MOUSELEAVE;
    {$endif}

    procedure DoChoiceClick(Sender: TObject);
    function GetIconHeight: integer;
    function GetIconWidth: integer;
    function GetTextItem(AIndex: integer; const ADefault: string): string;
    function IsPressed: boolean;
    procedure PaintBorder(C: TCanvas; R: TRect; AColor: TColor; AWidth: integer);
    procedure PaintIcon(C: TCanvas; AX, AY: integer);
    procedure PaintArrow(C: TCanvas; AX, AY: integer; AColorBg, AColorArrow: TColor);
    procedure PaintTo(C: TCanvas);
    procedure SetBoldFont(AValue: boolean);
    procedure SetChecked(AValue: boolean);
    procedure SetFlat(AValue: boolean);
    procedure SetFocusable(AValue: boolean);
    procedure SetImageIndex(AValue: integer);
    procedure SetImages(AValue: TImageList);
    procedure SetItemIndex(AValue: integer);
    procedure SetKind(AValue: TATButtonKind);
    procedure SetBoldBorder(AValue: boolean);
    procedure SetTextOverlay(const AValue: string);
    procedure SetTheme(AValue: PATFlatTheme);
    procedure ShowChoiceMenu;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$ifdef FPC}
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure TextChanged; override;
    {$else}
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    {$endif}

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure SetAutoSize(AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function CanFocus: boolean; override;
    property DataString: string read FDataString write FDataString;
    property DataString2: string read FDataString2 write FDataString2;
    property DataString3: string read FDataString3 write FDataString3;
    function GetTextSize(C: TCanvas; const S: string): TSize;
    property Items: TStringList read FItems;
    property ItemsShort: TStringList read FItemsShort;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Theme: PATFlatTheme read FTheme write SetTheme;
    property WidthInitial: integer read FWidthInitial write FWidthInitial;
    property TextOverlay: string read FTextOverlay write SetTextOverlay;
    property ShowShortItems: boolean read FShowShortItems write FShowShortItems;
  published
    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderSpacing;
    {$endif}

    {$ifndef FPC}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$endif}

    property Caption;
    property TabStop;
    property TabOrder;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Checked: boolean read FChecked write SetChecked default false;
    property Checkable: boolean read FCheckable write FCheckable default false;
    property Images: TImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Focusable: boolean read FFocusable write SetFocusable default true;
    property Flat: boolean read FFlat write SetFlat default false;
    property Arrow: boolean read FArrow write FArrow default false;
    property ArrowKind: TATButtonArrowKind read FArrowKind write FArrowKind default abakArrowDown;
    property ArrowAlign: TAlignment read FArrowAlign write FArrowAlign default taRightJustify;
    property TextAlign: TAlignment read FTextAlign write FTextAlign default taLeftJustify;
    property Kind: TATButtonKind read FKind write SetKind default abuTextOnly;
    property BoldBorder: boolean read FBoldBorder write SetBoldBorder default false;
    property BoldFont: boolean read FBoldFont write SetBoldFont default false;
    property Picture: TPicture read FPicture write FPicture;
    property Padding: integer read FPadding write FPadding default cDefaultButtonPadding;
    property PaddingBig: integer read FPaddingBig write FPaddingBig default cDefaultButtonPaddingBig;
    property OnClick;
    property OnDblClick;
    property OnResize;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    {$ifdef FPC}
    property OnMouseEnter;
    property OnMouseLeave;
    {$endif}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

{ TATButton }

{$ifndef FPC}
procedure TATButton.CMMouseEnter(var msg: TMessage);
begin
  DoMouseEnter;
end;

procedure TATButton.CMMouseLeave(var msg: TMessage);
begin
  DoMouseLeave;
end;

procedure TATButton.DoMouseEnter;
begin
  FOver:= true;
  Invalidate;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TATButton.DoMouseLeave;
begin
  FOver:= false;
  Invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$endif}

procedure TATButton.SetChecked(AValue: boolean);
begin
  if FChecked=AValue then Exit;
  FChecked:= AValue;
  Invalidate;
end;

procedure TATButton.SetFlat(AValue: boolean);
begin
  if FFlat=AValue then Exit;
  FFlat:= AValue;
  Invalidate;
  if FFlat then
    Focusable:= false;
end;

procedure TATButton.SetFocusable(AValue: boolean);
begin
  if FFocusable=AValue then Exit;
  FFocusable:= AValue;
  TabStop:= AValue;
end;

procedure TATButton.SetImageIndex(AValue: integer);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:= AValue;
  Invalidate;
end;

procedure TATButton.SetImages(AValue: TImageList);
begin
  if FImages=AValue then Exit;
  FImages:= AValue;
  Invalidate;
end;

procedure TATButton.SetItemIndex(AValue: integer);
begin
  if FItemIndex=AValue then Exit;
  FItemIndex:= AValue;
  if FCheckable then
    FChecked:= FItemIndex>0;
end;

procedure TATButton.SetKind(AValue: TATButtonKind);
begin
  if FKind=AValue then Exit;
  FKind:= AValue;
  Invalidate;
end;

procedure TATButton.SetBoldBorder(AValue: boolean);
begin
  if FBoldBorder=AValue then Exit;
  FBoldBorder:= AValue;
  Invalidate;
end;

procedure TATButton.SetTextOverlay(const AValue: string);
begin
  if FTextOverlay=AValue then Exit;
  FTextOverlay:= AValue;
  Invalidate;
end;

procedure TATButton.SetTheme(AValue: PATFlatTheme);
begin
  if FTheme=AValue then Exit;
  FTheme:= AValue;
  Invalidate;
end;

procedure TATButton.Click;
begin
  if FKind=abuTextChoice then
  begin
    ShowChoiceMenu;
    exit
  end;

  inherited;
  if FCheckable then
    FChecked:= not FChecked;
  Invalidate;
end;

function TATButton.CanFocus: boolean;
begin
  Result:= FFocusable;
end;

function TATButton.IsPressed: boolean;
begin
  Result:= FPressed and FOver;
end;

procedure TATButton.PaintBorder(C: TCanvas; R: TRect; AColor: TColor; AWidth: integer);
var
  i: integer;
begin
  C.Brush.Style:= bsClear;
  C.Pen.Color:= ColorToRGB(AColor);
  C.Rectangle(R);

  for i:= 1 to AWidth-1 do
  begin
    InflateRect(R, -1, -1);
    C.Rectangle(R);
  end;

  C.Brush.Style:= bsSolid;
end;

procedure TATButton.Paint;
begin
  inherited;
  PaintTo(Canvas);
end;

type
  TControlCracker = class(TControl); //for Delphi

procedure TATButton.PaintTo(C: TCanvas);
var
  NWidth, NHeight: integer;
  NSize, NSizeArrow: integer;
  bUseBack, bUseBorder: boolean;
  NColorBg, NColor: TColor;
  TextSize: TSize;
  pnt1, pnt2: TPoint;
  RectAll, RectText: TRect;
  S: string;
begin
  NWidth:= ClientWidth;
  NHeight:= ClientHeight;
  RectAll:= ClientRect;

  if FArrow then
    NSizeArrow:= Theme^.DoScale(4*Theme^.ArrowSize)
  else
    NSizeArrow:= 0;

  if not Theme^.EnableColorBgOver then
    FOver:= false;

  bUseBack:=
    (not FFlat)
    or FChecked
    or (FOver and not (FKind in [abuSeparatorHorz, abuSeparatorVert]));

  bUseBorder:= bUseBack
    or (FKind=abuTextChoice);

  if bUseBack then
  begin
    if not Enabled then
      NColorBg:= Theme^.ColorBgDisabled
    else
    if FChecked then
      NColorBg:= Theme^.ColorBgChecked
    else
    if FOver then
      NColorBg:= Theme^.ColorBgOver
    else
      NColorBg:= Theme^.ColorBgPassive;

    NColorBg:= ColorToRGB(NColorBg);
    C.Brush.Color:= NColorBg;
    C.FillRect(RectAll);
  end
  else
  begin
    {$ifdef FPC}
    //Flat style - don't paint any background (FPC)
    NColorBg:= clNone;
    {$else}
    //Flat style - don't paint any background (delphi)
    if Parent <> nil then
    if Parent is TControl then
    begin
      Self.Canvas.Brush.Color:= TControlCracker(Parent).Color;
      Self.Canvas.FillRect(Self.Canvas.ClipRect);
    end;
    {$endif}
  end;

  if bUseBorder then
  begin
    if FOver then
      NColor:= Theme^.ColorBorderOver
    else
    if Focused then
      NColor:= Theme^.ColorBorderFocused
    else
      NColor:= Theme^.ColorBorderPassive;

    NSize:= 1;
    if IsPressed then
      NSize:= Theme^.PressedBorderWidth
    else
    if BoldBorder then
      NSize:= Theme^.BoldBorderWidth
    else
    if Kind=abuTextChoice then
      NSize:= Theme^.ChoiceBorderWidth
    else
    if FOver then
      NSize:= Theme^.MouseoverBorderWidth;

    PaintBorder(C, RectAll, NColor, NSize);
  end;

  C.Font.Name:= Theme^.FontName;
  C.Font.Color:= ColorToRGB(IfThen(Enabled, Theme^.ColorFont, Theme^.ColorFontDisabled));
  C.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);

  if BoldFont then
    C.Font.Style:= [fsBold]
  else
    C.Font.Style:= Theme^.FontStyles;
  C.Brush.Style:= bsClear;

  case FKind of
    abuIconOnly:
      begin
        pnt1.x:= (NWidth-GetIconWidth) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftX) -
          IfThen(Arrow, Padding);
        pnt1.y:= (NHeight-GetIconHeight) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);
        PaintIcon(C, pnt1.x, pnt1.y);
      end;

    abuTextOnly:
      begin
        TextSize:= GetTextSize(C, Caption);
        pnt1.x:= (NWidth-TextSize.cx-NSizeArrow) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftX);
        pnt1.y:= (NHeight-TextSize.cy) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);
        C.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextIconHorz:
      begin
        TextSize:= GetTextSize(C, Caption);
        pnt1.x:= FPadding +
          IfThen(IsPressed, Theme^.PressedCaptionShiftX);
        pnt1.y:= (NHeight-GetIconHeight) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);
        PaintIcon(C, pnt1.x, pnt1.y);

        Inc(pnt1.x, GetIconWidth+FPadding);
        pnt1.y:= (NHeight-TextSize.cy) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);
        C.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextIconVert:
      begin
        TextSize:= GetTextSize(C, Caption);
        pnt1.x:= (NWidth-GetIconWidth-NSizeArrow) div 2+
          IfThen(IsPressed, Theme^.PressedCaptionShiftX);
        pnt1.y:= FPadding +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);
        PaintIcon(C, pnt1.x, pnt1.y);

        Inc(pnt1.y, GetIconHeight+FPadding);
        pnt1.x:= (NWidth-TextSize.cx-NSizeArrow) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftX);
        C.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextChoice:
      begin
        S:= GetTextItem(FItemIndex, '?');
        TextSize:= C.TextExtent(S);

        RectText.Top:= 0;
        RectText.Bottom:= NHeight;
        RectText.Left:= IfThen(IsPressed, Theme^.PressedCaptionShiftX);
        RectText.Right:= NWidth;

        if FArrowAlign=taLeftJustify then
          Inc(RectText.Left, NSizeArrow)
        else
          Dec(RectText.Right, NSizeArrow);

        case FTextAlign of
          taLeftJustify:
            pnt1.x:= RectText.Left+FPadding;
          taCenter:
            pnt1.x:= (RectText.Width-TextSize.cx) div 2;
          taRightJustify:
            pnt1.x:= RectText.Right-FPadding-TextSize.cx;
        end;

        pnt1.y:= (NHeight-TextSize.cy) div 2 +
          IfThen(IsPressed, Theme^.PressedCaptionShiftY);

        C.TextOut(pnt1.x, pnt1.y, S);
      end;

    abuSeparatorVert:
      begin
        for NSize:= 0 to Theme^.DoScale(1)-1 do
        begin
          pnt1:= Point(Theme^.SeparatorOffset, NHeight div 2+NSize);
          pnt2:= Point(NWidth-Theme^.SeparatorOffset+NSize, NHeight div 2+NSize);
          CanvasLine(C, pnt1, pnt2, Theme^.ColorSeparators);
        end;
      end;

    abuSeparatorHorz:
      begin
        for NSize:= 0 to Theme^.DoScale(1)-1 do
        begin
          pnt1:= Point(NWidth div 2+NSize, Theme^.SeparatorOffset);
          pnt2:= Point(NWidth div 2+NSize, NHeight-Theme^.SeparatorOffset);
          CanvasLine(C, pnt1, pnt2, Theme^.ColorSeparators);
        end;
      end;
  end;

  if FArrow then
  begin
    case FArrowAlign of
      taLeftJustify:
        pnt1.x:= NSizeArrow;
      taRightJustify:
        pnt1.x:= NWidth-NSizeArrow;
      taCenter:
        pnt1.x:= (NWidth-NSizeArrow div 4) div 2;
    end;

    pnt1.y:= NHeight div 2 +
      IfThen(IsPressed, Theme^.PressedCaptionShiftY);

    PaintArrow(C, pnt1.x, pnt1.y, clNone{NColorBg}, Theme^.ColorArrows);
  end;

  if FTextOverlay<>'' then
  begin
    TextSize:= C.TextExtent(FTextOverlay);
    C.Brush.Color:= Theme^.ColorBgOverlay;
    C.Font.Color:= Theme^.ColorFontOverlay;

    case Theme^.TextOverlayPosition of
      bopLeftTop:
        begin
          pnt1.x:= 0;
          pnt1.y:= 0;
        end;
      bopRightTop:
        begin
          pnt1.x:= NWidth-TextSize.cx;
          pnt1.y:= 0;
        end;
      bopLeftBottom:
        begin
          pnt1.x:= 0;
          pnt1.y:= NHeight-TextSize.cy;
        end;
      bopRightBottom:
        begin
          pnt1.x:= NWidth-TextSize.cx;
          pnt1.y:= NHeight-TextSize.cy;
        end;
    end;

    C.TextOut(pnt1.x, pnt1.y, FTextOverlay);
  end;
end;

procedure TATButton.PaintIcon(C: TCanvas; AX, AY: integer);
begin
  if Assigned(FImages) and (FImageIndex>=0) and (FImageIndex<FImages.Count) then
    FImages.Draw(C, AX, AY, FImageIndex)
  else
  if Assigned(FPicture) then
    C.Draw(AX, AY, FPicture.Graphic);
end;

procedure TATButton.PaintArrow(C: TCanvas; AX, AY: integer; AColorBg, AColorArrow: TColor);
var
  NSize: integer;
  R: TRect;
begin
  NSize:= Theme^.DoScale(Theme^.ArrowSize);
  R:= Rect(
    AX-NSize*2-1,
    AY-NSize*2-1,
    AX+NSize*2+2,
    AY+NSize*2+1);

  if AColorBg<>clNone then
  begin
    C.Brush.Color:= AColorBg;
    C.FillRect(R);
  end;

  case FArrowKind of
    abakArrowDown:
      CanvasPaintTriangleDown(C, AColorArrow, Point(AX, AY), NSize);
    abakArrowLeft:
      CanvasPaintTriangleLeft(C, AColorArrow, Point(AX, AY), NSize);
    abakArrowRight:
      CanvasPaintTriangleRight(C, AColorArrow, Point(AX, AY), NSize);
    abakCross:
      begin
        NSize:= (R.Right-R.Left - Theme^.DoScale(FTheme^.XMarkWidth - FTheme^.XMarkOffsetLeft - FTheme^.XMarkOffsetRight)) div 2;
        CanvasPaintXMark(C, R, AColorArrow,
          NSize,
          NSize,
          Theme^.DoScale(FTheme^.XMarkLineWidth));
      end;
  end;
end;

procedure TATButton.SetBoldFont(AValue: boolean);
begin
  if FBoldFont=AValue then Exit;
  FBoldFont:= AValue;
  AutoSize:= AutoSize;
  Invalidate;
end;

function TATButton.GetIconWidth: integer;
begin
  if Assigned(FImages) then
    Result:= FImages.Width
  else
  if Assigned(FPicture) then
    Result:= FPicture.Width
  else
    Result:= 0;
end;

function TATButton.GetIconHeight: integer;
begin
  if Assigned(FImages) then
    Result:= FImages.Height
  else
  if Assigned(FPicture) then
    Result:= FPicture.Height
  else
    Result:= 0;
end;

procedure TATButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  bOver: boolean;
begin
  inherited;

  bOver:= PtInRect(ClientRect, Point(X, Y));
  if bOver<>FOver then
  begin
    FOver:= bOver;
    Invalidate;
  end;
end;

{$ifdef FPC}
procedure TATButton.MouseLeave;
begin
  inherited;
  FOver:= false;
  Invalidate;
end;

procedure TATButton.MouseEnter;
begin
  inherited;
  FOver:= true;
  Invalidate;
end;
{$endif}

procedure TATButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Shift=[ssLeft] then
  begin
    FPressed:= true;
    if FFocusable then
      SetFocus;
  end;

  Invalidate;
end;

procedure TATButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FPressed:= false;
  Invalidate;
end;

procedure TATButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Key=VK_SPACE) or (Key=VK_RETURN)) and (Shift=[]) then
  begin
    Click;
    Key:= 0;
  end;
end;

procedure TATButton.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TATButton.DoExit;
begin
  inherited;
  Invalidate;
end;

{$ifdef FPC}
procedure TATButton.TextChanged;
begin
  inherited;
  Invalidate; //paint caption
end;
{$endif}

procedure TATButton.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TATButton.SetAutoSize(AValue: boolean);
var
  C: TCanvas;
  NText, NIcon, NGap: integer;
begin
  inherited;
  if not AValue then exit;

  C:= Canvas;
  C.Font.Name:= Theme^.FontName;
  C.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);
  C.Font.Style:= [];

  //if FBoldFont then
  //  C.Font.Style:= [fsBold]
  //else
  //  C.Font.Style:= [];

  NText:= C.TextWidth(Caption);
  NIcon:= GetIconWidth;
  NGap:= Theme^.GapForAutoSize;

  case FKind of
    abuTextOnly:
      Width:= NText+NGap;
    abuIconOnly:
      Width:= NIcon+NGap;
    abuTextIconHorz:
      Width:= NText+NGap+NIcon+NGap;
    abuTextIconVert:
      Width:= Max(NIcon, NText)+NGap;
  end;
end;

constructor TATButton.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque]
    -[csDoubleClicks {$ifdef FPC}, csTripleClicks{$endif}];

  TabStop:= true;
  Width:= 100;
  Height:= 25;

  Caption:= 'Button';
  FPicture:= TPicture.Create;
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FFocusable:= true;
  FFlat:= false;
  FImages:= nil;
  FImageIndex:= -1;
  FKind:= abuTextOnly;
  FBoldBorder:= false;
  FArrow:= false;
  FArrowKind:= abakArrowDown;
  FArrowAlign:= taRightJustify;
  FTextAlign:= taLeftJustify;
  FPadding:= cDefaultButtonPadding;
  FPaddingBig:= cDefaultButtonPaddingBig;
  FItems:= TStringList.Create;
  FItemsShort:= TStringList.Create;
  FItemIndex:= -1;
  FTheme:= @ATFlatTheme;
  FWidthInitial:= 0;
end;

destructor TATButton.Destroy;
begin
  FItemsShort.Free;
  FItems.Free;
  FPicture.Free;

  inherited;
end;

function TATButton.GetTextSize(C: TCanvas; const S: string): TSize;
begin
  Result.cx:= 0;
  Result.cy:= 0;
  if S='' then exit;

  if BoldFont then
    C.Font.Style:= [fsBold]
  else
    C.Font.Style:= Theme^.FontStyles;

  Result:= C.TextExtent(S);
end;

procedure TATButton.DoChoiceClick(Sender: TObject);
begin
  ItemIndex:= (Sender as TComponent).Tag;
  Invalidate;
  inherited Click;
end;

procedure TATButton.ShowChoiceMenu;
var
  mi: TMenuItem;
  i: integer;
  P: TPoint;
begin
  if not Assigned(FPopup) then
    FPopup:= TPopupMenu.Create(Self);

  FPopup.Items.Clear;
  for i:= 0 to FItems.Count-1 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= FItems[i];
    mi.Tag:= i;
    mi.RadioItem:= true;
    mi.Checked:= i=FItemIndex;

    mi.OnClick:= DoChoiceClick;

    FPopup.Items.Add(mi);
  end;

  P:= ClientToScreen(Point(0, Height));
  FPopup.PopUp(P.X, P.Y);
end;

function TATButton.GetTextItem(AIndex: integer; const ADefault: string): string;
begin
  Result:= ADefault;
  if FShowShortItems then
  begin
    if (AIndex>=0) and (AIndex<FItemsShort.Count) then
      Result:= FItemsShort[AIndex];
  end
  else
  begin
    if (AIndex>=0) and (AIndex<FItems.Count) then
      Result:= FItems[AIndex];
  end;
end;

end.

