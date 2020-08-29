{
ATScrollBar for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL

Features:
- fully supports owner-draw of all elements (arrows, backgnd, thumb, corner empty area)
- prop: border size
- prop: arrow mark size
- prop: size of corner empty area (for additional controls maybe)
- prop: kind of arrows (normal, both above, both below, no arrows)

Mouse usage:
- click and holding mouse on arrows
- click and holding mouse on page-up (area above thumb) / page-down (area below thumb)
- dragging of thumb
}

unit ATScrollBar;

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
{$endif}

interface

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Forms,
  ATCanvasPrimitives;

type
  TATScrollbarElemType = (
    aseArrowUp,
    aseArrowDown,
    aseArrowLeft,
    aseArrowRight,
    aseScrollThumbV,
    aseScrollThumbH,
    aseScrollAreaH,
    aseScrollAreaV,
    aseScrolledAreaH,
    aseScrolledAreaV,
    aseCorner
    );

type
  TATScrollbarArrowsStyle = (
    asaArrowsNormal,
    asaArrowsBelow,
    asaArrowsAbove,
    asaArrowsHidden
    );

type
  TATScrollbarDrawEvent = procedure (Sender: TObject; AType: TATScrollbarElemType;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  PATScrollbarTheme = ^TATScrollbarTheme;
  TATScrollbarTheme = record
    ColorBG: TColor;
    ColorBorder: TColor;
    ColorThumbBorder: TColor;
    ColorThumbFill: TColor;
    ColorThumbFillOver: TColor;
    ColorThumbFillPressed: TColor;
    ColorThumbDecor: TColor;
    ColorThumbDecor2: TColor;

    ColorArrowBorder: TColor;
    ColorArrowFill: TColor;
    ColorArrowFillOver: TColor;
    ColorArrowFillPressed: TColor;

    ColorArrowSign: TColor;
    ColorScrolled: TColor;

    InitialSize: integer;
    ScalePercents: integer;
    ArrowStyleH: TATScrollbarArrowsStyle;
    ArrowStyleV: TATScrollbarArrowsStyle;
    ArrowSize: integer;
    ArrowLengthPercents: integer;
    BorderSize: integer;
    TimerInterval: integer;
    DirectJumpOnClickPageUpDown: boolean;

    MinSizeToShowThumb: integer;
    ThumbMinSize: integer;
    ThumbMarkerOffset: integer;
    ThumbMarkerMinimalSize: integer;
    ThumbMarkerDecorSize: integer;
    ThumbMarkerDecorSpace: integer;
    ThumbMarkerDecorDouble: boolean;
  end;

var
  ATScrollbarTheme: TATScrollbarTheme;

type
  { TATScrollbar }

  TATScrollbar = class(TCustomControl)
  private
    FTimerMouseover: TTimer;

    {$ifndef FPC}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    {$endif}

    FKind: TScrollBarKind;
    FIndentCorner: Integer;
    FTheme: PATScrollbarTheme;

    FPos: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: Integer;
    FLargeChange: Integer;
    FPageSize: Integer;
    FDeltaOfThumb: Integer;

    //internal
    FRectMain: TRect; //area for scrolling
    FRectArrUp: TRect; //area for up or left arrow
    FRectArrDown: TRect; //area for down or right arrow
    FRectThumb: TRect; //area for scroll-thumb
    FRectCorner: TRect;
    FRectPageUp: TRect;
    FRectPageDown: TRect;

    FBitmap: TBitmap;
    FTimer: TTimer;
    FOnChange: TNotifyEvent;
    FOnOwnerDraw: TATScrollbarDrawEvent;

    //drag-drop
    FMouseDown: boolean;
    FMouseDragOffset: Integer;
    FMouseDownOnUp,
    FMouseDownOnDown,
    FMouseDownOnThumb,
    FMouseDownOnPageUp,
    FMouseDownOnPageDown: boolean;

    {$ifndef FPC}
    procedure CMMouseEnter(var msg: TMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage);
      message CM_MOUSELEAVE;
    {$endif}

    function EffectiveRectSize: integer;
    procedure TimerMouseoverTick(Sender: TObject);

    procedure DoPaintArrow(C: TCanvas; const R: TRect; AType: TATScrollbarElemType);
    procedure DoPaintThumb(C: TCanvas);
    procedure DoPaintBack(C: TCanvas);
    procedure DoPaintBackScrolled(C: TCanvas);
    procedure DoPaintTo(C: TCanvas);

    procedure DoPaintStd_Corner(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Back(C: TCanvas; const R: TRect);
    procedure DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Arrow(C: TCanvas; R: TRect; AType: TATScrollbarElemType);
    procedure DoPaintStd_Thumb(C: TCanvas; const R: TRect);

    function IsHorz: boolean; inline;
    function CoordToPos(X, Y: Integer): Integer;
    procedure DoUpdateThumbRect;
    procedure DoUpdateCornerRect;
    procedure DoUpdatePosOnDrag(X, Y: Integer);
    procedure DoScrollBy(NDelta: Integer);
    function PosToCoord(APos: Integer): Integer;
    function DoScale(AValue: integer): integer;

    procedure TimerTimer(Sender: TObject);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetPos(AValue: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPageSize(Value: Integer);
    function DoDrawEvent(AType: TATScrollbarElemType;
      ACanvas: TCanvas; const ARect: TRect): boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    property Theme: PATScrollbarTheme read FTheme write FTheme;
    procedure Update; reintroduce;

  protected
    {$ifdef FPC}
     procedure MouseLeave; override;
     procedure MouseEnter; override;
    {$endif}
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifndef FPC}
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    {$endif}
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    {$ifndef FPC}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {$endif}

    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderSpacing;
    {$endif}
    property Constraints;
    property Enabled;
    property DoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Position: Integer read FPos write SetPos default 0;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property SmallChange: Integer read FSmallChange write FSmallChange default 1;
    property LargeChange: Integer read FLargeChange write FLargeChange default 0;
    property PageSize: Integer read FPageSize write SetPageSize default 20;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property IndentCorner: Integer read FIndentCorner write FIndentCorner default 0;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseUp;
    property OnOwnerDraw: TATScrollbarDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
    property OnContextPopup;
    property OnResize;
  end;

implementation

uses
  SysUtils, Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

{ TATScrollbar }

procedure TATScrollbar.TimerMouseoverTick(Sender: TObject);
//timer is workaround for LCL issue, where MouseLeave not called
//if mouse leaves app window area (at least on Linux)
{$ifdef FPC}
var
  Pnt: TPoint;
{$endif}
begin
  {$ifdef FPC}
  Pnt:= ScreenToClient(Mouse.CursorPos);
  if not PtInRect(ClientRect, Pnt) then
    MouseLeave;
  {$endif}
end;

{$ifdef FPC}
procedure TATScrollbar.MouseLeave;
begin
  inherited;
  FTimerMouseover.Enabled:= false;
  //FOver:= false;
  Invalidate;
end;

procedure TATScrollbar.MouseEnter;
begin
  inherited;
  //FOver:= true;
  Invalidate;
  FTimerMouseover.Enabled:= true;
end;
{$endif}

constructor TATScrollbar.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  {$ifdef FPC}
  BorderStyle:= bsNone;
  {$endif}
  ControlStyle:= ControlStyle+[csOpaque];

  FKind:= sbHorizontal;
  FIndentCorner:= 0;

  FTheme:= @ATScrollbarTheme;
  Width:= 200;
  Height:= FTheme.InitialSize;
  Color:= FTheme^.ColorBG;

  DoubleBuffered:= IsDoubleBufferedNeeded;

  FMin:= 0;
  FMax:= 100;
  FSmallChange:= 1;
  FLargeChange:= 0;
  FPageSize:= 20;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 600;
  FBitmap.Height:= 50;

  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= false;
  FTimer.Interval:= 100;
  FTimer.OnTimer:= TimerTimer;

  FTimerMouseover:= TTimer.Create(Self);
  FTimerMouseover.Enabled:= false;
  FTimerMouseover.Interval:= 1000;
  FTimerMouseover.OnTimer:= TimerMouseoverTick;

  FMouseDown:= false;
  FMouseDragOffset:= 0;
end;

destructor TATScrollbar.Destroy;
begin
  FTimer.Enabled:= false;
  FreeAndNil(FTimer);
  FreeAndNil(FBitmap);
  inherited;
end;

function TATScrollbar.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATScrollbar.Update;
begin
  if IsHorz then
    Height:= DoScale(FTheme^.InitialSize)
  else
    Width:= DoScale(FTheme^.InitialSize);

  Invalidate;
end;

function TATScrollbar.DoScale(AValue: integer): integer;
begin
  Result:= AValue * FTheme^.ScalePercents div 100;
end;

procedure TATScrollbar.Paint;
begin
  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
    begin
      DoPaintTo(FBitmap.Canvas);
      Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
    end;
  end
  else
    DoPaintTo(Canvas);
end;

procedure TATScrollbar.DoPaintTo(C: TCanvas);
var
  FSize: Integer;
begin
  FRectMain:= ClientRect;
  FRectArrUp:= Rect(0, 0, 0, 0);
  FRectArrDown:= Rect(0, 0, 0, 0);

  DoUpdateCornerRect;
  if not IsRectEmpty(FRectCorner) then
    if DoDrawEvent(aseCorner, C, FRectCorner) then
      DoPaintStd_Corner(C, FRectCorner);

  C.Brush.Color:= ColorToRGB(FTheme^.ColorBorder);
  C.FillRect(FRectMain);

  InflateRect(FRectMain,
    -DoScale(FTheme^.BorderSize),
    -DoScale(FTheme^.BorderSize)
    );

  if IsHorz then
  begin
    //horz kind
    FSize:= Math.Min(
      FRectMain.Height * FTheme^.ArrowLengthPercents div 100,
      FRectMain.Width div 2
      );
    case FTheme^.ArrowStyleH of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Left, FSize);
          Dec(FRectMain.Right, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Left+FSize, FRectMain.Top, FRectMain.Left+2*FSize, FRectMain.Bottom);
          Inc(FRectMain.Left, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          FRectArrUp:= Rect(FRectMain.Right-2*FSize, FRectMain.Top, FRectMain.Right-FSize, FRectMain.Bottom);
          Dec(FRectMain.Right, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowLeft);
    DoPaintArrow(C, FRectArrDown, aseArrowRight);
  end
  else
  begin
    //vertical kind
    FSize:= Math.Min(
      FRectMain.Width * FTheme^.ArrowLengthPercents div 100,
      FRectMain.Height div 2
      );
    case FTheme^.ArrowStyleV of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Top, FSize);
          Dec(FRectMain.Bottom, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Bottom-2*FSize, FRectMain.Right, FRectMain.Bottom-FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Dec(FRectMain.Bottom, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Top+FSize, FRectMain.Right, FRectMain.Top+2*FSize);
          Inc(FRectMain.Top, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowUp);
    DoPaintArrow(C, FRectArrDown, aseArrowDown);
  end;

  DoPaintBack(C);
  DoUpdateThumbRect;
  DoPaintBackScrolled(C);
  DoPaintThumb(C);
end;

procedure TATScrollbar.DoPaintBack(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsHorz then Typ:= aseScrollAreaH else Typ:= aseScrollAreaV;
  if DoDrawEvent(Typ, C, FRectMain) then
    DoPaintStd_Back(C, FRectMain);
end;

procedure TATScrollbar.DoPaintBackScrolled(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsHorz then Typ:= aseScrolledAreaH else Typ:= aseScrolledAreaV;

  if FMouseDown and FMouseDownOnPageUp then
    if DoDrawEvent(Typ, C, FRectPageUp) then
      DoPaintStd_BackScrolled(C, FRectPageUp);

  if FMouseDown and FMouseDownOnPageDown then
    if DoDrawEvent(Typ, C, FRectPageDown) then
      DoPaintStd_BackScrolled(C, FRectPageDown);
end;


procedure TATScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ScrollVal: integer;
begin
  inherited;

  FMouseDown:= Button=mbLeft;
  FMouseDownOnThumb:= PtInRect(FRectThumb, Point(X, Y));
  FMouseDownOnUp:= PtInRect(FRectArrUp, Point(X, Y));
  FMouseDownOnDown:= PtInRect(FRectArrDown, Point(X, Y));
  FMouseDownOnPageUp:= PtInRect(FRectPageUp, Point(X, Y));
  FMouseDownOnPageDown:= PtInRect(FRectPageDown, Point(X, Y));

  Invalidate;

  if IsHorz then
    FMouseDragOffset:= X-FRectThumb.Left
  else
    FMouseDragOffset:= Y-FRectThumb.Top;

  if FMouseDown then
  begin
    FTimer.Interval:= FTheme^.TimerInterval;

    if FMouseDownOnUp then
    begin
      DoScrollBy(-FSmallChange);
      FTimer.Enabled:= true;
    end
    else
    if FMouseDownOnDown then
    begin
      DoScrollBy(FSmallChange);
      FTimer.Enabled:= true;
    end
    else
    if FMouseDownOnPageUp or FMouseDownOnPageDown then
    begin
      if FTheme^.DirectJumpOnClickPageUpDown then
      begin
        Position:= Math.Min(FMax-FPageSize,
                   Math.Max(FMin,
                   CoordToPos(X, Y)));
      end
      else
      begin
        if FLargeChange>0 then
          ScrollVal:= FLargeChange
        else
          ScrollVal:= FPageSize;

        if FMouseDownOnPageUp then
          DoScrollBy(-ScrollVal)
        else
          DoScrollBy(ScrollVal);

        FTimer.Enabled:= true;
      end;
    end;
  end;
end;

procedure TATScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  FMouseDown:= false;
  FMouseDownOnThumb:= false;

  FMouseDownOnUp:= false;
  FMouseDownOnDown:= false;

  FTimer.Enabled:= false;
  Invalidate;
end;

procedure _BitmapResize(b: TBitmap; X, Y: integer); inline;
begin
  {$ifdef fpc}
  b.SetSize(X, Y);
  b.FreeImage; //recommended, else seen black bitmap on bigsize
  {$else}
  b.Width:= X;
  b.Height:= Y;
  {$endif}
end;

procedure TATScrollbar.Resize;
const
  cStep = 50; //resize bitmap by N pixels step
var
  SizeX, SizeY: integer;
begin
  inherited;

  //ATSynEdit has the same code
  if Assigned(FBitmap) then
  begin
    SizeX:= (Width div cStep + 1)*cStep;
    SizeY:= (Height div cStep + 1)*cStep;
    if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
      _BitmapResize(FBitmap, SizeX, SizeY);
  end;

  Invalidate;
end;

//needed to remove flickering on resize and mouse-over
{$ifdef windows}
procedure TATScrollbar.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

{$ifndef FPC}
procedure TATScrollbar.CMMouseEnter(var msg: TMessage);
begin
  DoMouseEnter;
end;

procedure TATScrollbar.CMMouseLeave(var msg: TMessage);
begin
  DoMouseLeave;
end;

procedure TATScrollbar.DoMouseEnter;
begin
  Invalidate;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TATScrollbar.DoMouseLeave;
begin
  Invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$endif}

procedure TATScrollbar.Click;
begin
  inherited;
end;

function TATScrollbar.DoDrawEvent(AType: TATScrollbarElemType;
  ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnOwnerDraw) then
    FOnOwnerDraw(Self, AType, ACanvas, ARect, Result);
end;

procedure TATScrollbar.SetKind(AValue: TScrollBarKind);
begin
  if AValue=FKind then Exit;
  FKind:= AValue;

  if IsHorz then
  begin
    Width:= 200;
    Height:= FTheme^.InitialSize;
  end
  else
  begin
    Height:= 200;
    Width:= FTheme^.InitialSize;
  end;
end;

procedure TATScrollbar.DoPaintArrow(C: TCanvas; const R: TRect;
  AType: TATScrollbarElemType);
begin
  if IsRectEmpty(R) then exit;
  if DoDrawEvent(AType, C, R) then
    DoPaintStd_Arrow(C, R, AType);
end;    

procedure TATScrollbar.DoPaintStd_Arrow(C: TCanvas; R: TRect;
  AType: TATScrollbarElemType);
var
  P: TPoint;
  NSize: Integer;
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowBorder);
  C.FillRect(R);

  InflateRect(R, -1, -1);
  C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowFill);
  C.FillRect(R);

  if (FMouseDownOnUp and (AType in [aseArrowUp, aseArrowLeft])) or
    (FMouseDownOnDown and (AType in [aseArrowDown, aseArrowRight])) then
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowFillPressed);
    C.FillRect(R);
  end
  else
  begin
    P:= Mouse.CursorPos;
    P:= ScreenToClient(P);
    if PtInRect(R,P) then
    begin
      C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowFillOver);
      C.FillRect(R);
    end;
  end;

  P:= CenterPoint(R);
  NSize:= DoScale(FTheme^.ArrowSize);

  case AType of
    aseArrowUp:
      CanvasPaintTriangleUp(C, FTheme^.ColorArrowSign, P, NSize);
    aseArrowDown:
      CanvasPaintTriangleDown(C, FTheme^.ColorArrowSign, P, NSize);
    aseArrowLeft:
      CanvasPaintTriangleLeft(C, FTheme^.ColorArrowSign, P, NSize);
    aseArrowRight:
      CanvasPaintTriangleRight(C, FTheme^.ColorArrowSign, P, NSize);
    else
      Exit;
  end;
end;

function TATScrollbar.IsHorz: boolean;
begin
  Result:= FKind=sbHorizontal;
end;

function TATScrollbar.EffectiveRectSize: integer;
begin
  if IsHorz then
    Result:= FRectMain.Width
  else
    Result:= FRectMain.Height;

  if FDeltaOfThumb<0 then
    Inc(Result, FDeltaOfThumb);

  if Result<1 then
    Result:= 1;
end;

function TATScrollbar.PosToCoord(APos: Integer): Integer;
var
  N0: Integer;
begin
  if IsHorz then
  begin
    N0:= FRectMain.Left;
  end
  else
  begin
    N0:= FRectMain.Top;
  end;
  Result:= N0 + (APos-FMin) * EffectiveRectSize div Math.Max(1, FMax-FMin);
end;

procedure TATScrollbar.DoUpdateThumbRect;
var
  R: TRect;
  NMin: integer;
begin
  FRectThumb:= Rect(0, 0, 0, 0);
  FRectPageUp:= Rect(0, 0, 0, 0);
  FRectPageDown:= Rect(0, 0, 0, 0);
  NMin:= FTheme^.ThumbMinSize;

  if IsHorz then
  begin
    if FRectMain.Width<FTheme^.MinSizeToShowThumb then Exit;
    R.Top:= FRectMain.Top;
    R.Bottom:= FRectMain.Bottom;
    R.Left:= PosToCoord(FPos);
    R.Right:= PosToCoord(FPos+FPageSize);
    FDeltaOfThumb:= R.Right-R.Left-NMin;
    R.Left:= Math.Min(R.Left, FRectMain.Right-NMin);
    R.Right:= Math.Max(R.Right, R.Left+NMin);
    R.Right:= Math.Min(R.Right, FRectMain.Right);
  end
  else
  begin
    if FRectMain.Height<FTheme^.MinSizeToShowThumb then Exit;
    R.Left:= FRectMain.Left;
    R.Right:= FRectMain.Right;
    R.Top:= PosToCoord(FPos);
    R.Bottom:= PosToCoord(FPos+FPageSize);
    FDeltaOfThumb:= R.Bottom-R.Top-NMin;
    R.Top:= Math.Min(R.Top, FRectMain.Bottom-NMin);
    R.Bottom:= Math.Max(R.Bottom, R.Top+NMin);
    R.Bottom:= Math.Min(R.Bottom, FRectMain.Bottom);
  end;
  FRectThumb:= R;

  if IsHorz then
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectThumb.Left, FRectMain.Bottom);
    FRectPageDown:= Rect(FRectThumb.Right, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
  end
  else
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectThumb.Top);
    FRectPageDown:= Rect(FRectMain.Left, FRectThumb.Bottom, FRectMain.Right, FRectMain.Bottom);
  end;
end;

procedure TATScrollbar.DoPaintThumb(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsRectEmpty(FRectThumb) then Exit;
  if IsHorz then
    Typ:= aseScrollThumbH
  else
    Typ:= aseScrollThumbV;

  if DoDrawEvent(Typ, C, FRectThumb) then
    DoPaintStd_Thumb(C, FRectThumb);
end;

procedure TATScrollbar.DoPaintStd_Thumb(C: TCanvas; const R: TRect);
var
  P: TPoint;
  NOffset, i, DecorSpace: integer;
begin
  C.Brush.Color:= ColorToRGB(FTheme^.ColorThumbFill);

  if FMouseDownOnThumb then
    C.Brush.Color:= ColorToRGB(FTheme^.ColorThumbFillPressed)
  else
  begin
    P := Mouse.CursorPos;
    P := ScreenToClient(P);
    if PtInRect(R,P) then
      C.Brush.Color:= ColorToRGB(FTheme^.ColorThumbFillOver);
  end;

  C.Pen.Color:= ColorToRGB(FTheme^.ColorThumbBorder);
  C.Rectangle(R);

  NOffset:= FTheme^.ThumbMarkerOffset;

  P:= CenterPoint(R);
  if FTheme^.ThumbMarkerDecorDouble then
  begin
    if IsHorz then Inc(P.X) else Inc(P.Y);
  end;

  C.Pen.Color:= ColorToRGB(FTheme^.ColorThumbDecor);
  DecorSpace := FTheme^.ThumbMarkerDecorSPace;

  if IsHorz then
  begin
    if R.Width>FTheme^.ThumbMarkerMinimalSize then
    begin
      for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
      begin
        C.MoveTo(P.X-DecorSpace*i, R.Top+NOffset);
        C.LineTo(P.X-DecorSpace*i, R.Bottom-NOffset);
        if i>0 then
        begin
          C.MoveTo(P.X+DecorSpace*i, R.Top+NOffset);
          C.LineTo(P.X+DecorSpace*i, R.Bottom-NOffset);
        end;
      end;
    end;

    if FTheme^.ThumbMarkerDecorDouble then
    begin
      C.Pen.Color:= ColorToRGB(FTheme^.ColorThumbDecor2);
      for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
      begin
        C.MoveTo((P.X-DecorSpace*i) -1, R.Top+NOffset);
        C.LineTo((P.X-DecorSpace*i) -1, R.Bottom-NOffset);
        if i>0 then
        begin
          C.MoveTo((P.X+DecorSpace*i) -1, R.Top+NOffset);
          C.LineTo((P.X+DecorSpace*i) -1, R.Bottom-NOffset);
        end;
      end;
    end;

  end
  else
  begin
    if R.Height>FTheme^.ThumbMarkerMinimalSize then
    begin
      for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
      begin
        C.MoveTo(R.Left+NOffset, P.Y-DecorSpace*i);
        C.LineTo(R.Right-NOffset, P.Y-DecorSpace*i);
        if i>0 then
        begin
          C.MoveTo(R.Left+NOffset, P.Y+DecorSpace*i);
          C.LineTo(R.Right-NOffset, P.Y+DecorSpace*i);
        end;
      end;

      if FTheme^.ThumbMarkerDecorDouble then
      begin
        C.Pen.Color:= ColorToRGB(FTheme^.ColorThumbDecor2);
        for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
        begin
          C.MoveTo(R.Left+NOffset, (P.Y-DecorSpace*i) -1);
          C.LineTo(R.Right-NOffset, (P.Y-DecorSpace*i) -1);
          if i>0 then
          begin
            C.MoveTo(R.Left+NOffset, (P.Y+DecorSpace*i) -1);
            C.LineTo(R.Right-NOffset, (P.Y+DecorSpace*i) -1);
          end;
        end;
      end;

    end;
  end;
end;


procedure TATScrollbar.SetMax(Value: Integer);
begin
  if FMax<>Value then
  begin
    FMax:= Value;
    FPos:= Math.Min(FPos, FMax);
    Invalidate;
  end;
end;

procedure TATScrollbar.SetMin(Value: Integer);
begin
  if FMin<>Value then
  begin
    FMin:= Value;
    FPos:= Math.Max(FPos, FMin);
    Invalidate;
  end;
end;

procedure TATScrollbar.SetPageSize(Value: Integer);
begin
  if FPageSize<>Value then
  begin
    FPageSize:= Value;
    Invalidate;
  end;
end;

procedure TATScrollbar.SetPos(AValue: Integer);
begin
  if AValue>FMax then
    AValue:= FMax;
  if AValue<FMin then
    AValue:= FMin;

  if FPos<>AValue then
  begin
    FPos:= AValue;

    {$ifdef windows}
    Repaint; //only Invalidate is not ok, it delays painting on big files
    {$else}
    Invalidate;
    {$endif}

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TATScrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Invalidate;

  if FMouseDownOnThumb then
  begin
    DoUpdatePosOnDrag(X, Y);
  end;
end;

function TATScrollbar.CoordToPos(X, Y: Integer): Integer;
begin
  if IsHorz then
    Result:= FMin + (X-FRectMain.Left) * (FMax-FMin) div EffectiveRectSize
  else
    Result:= FMin + (Y-FRectMain.Top) * (FMax-FMin) div EffectiveRectSize;
end;

procedure TATScrollbar.DoUpdatePosOnDrag(X, Y: Integer);
var
  N: Integer;
begin
  N:= CoordToPos(
    X-FMouseDragOffset,
    Y-FMouseDragOffset);
  N:= Math.Max(N, FMin);
  N:= Math.Min(N, FMax-FPageSize);
  SetPos(N);
end;

procedure TATScrollbar.DoScrollBy(NDelta: Integer);
var
  N: Integer;
begin
  N:= FPos;
  Inc(N, NDelta);
  if (NDelta>0) then
    N:= Math.Min(N, FMax-FPageSize);
  SetPos(N);
end;

procedure TATScrollbar.TimerTimer(Sender: TObject);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  P:= ScreenToClient(P);

  if FMouseDownOnDown and PtInRect(FRectArrDown, P) then
    DoScrollBy(FSmallChange)
  else
  if FMouseDownOnUp and PtInRect(FRectArrUp, P) then
    DoScrollBy(-FSmallChange)
  else
  if FMouseDownOnPageDown and PtInRect(FRectPageDown, P) then
    DoScrollBy(FPageSize)
  else
  if FMouseDownOnPageUp and PtInRect(FRectPageUp, P) then
    DoScrollBy(-FPageSize);
end;

procedure TATScrollbar.DoPaintStd_Corner(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorBG);
  C.FillRect(R);
end;

procedure TATScrollbar.DoPaintStd_Back(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorBG);
  C.FillRect(R);
end;

procedure TATScrollbar.DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorScrolled);
  C.FillRect(R);
end;

procedure TATScrollbar.DoUpdateCornerRect;
var
  w, h, Delta: integer;
begin
  w:= Width;
  h:= Height;
  FRectCorner:= Rect(0, 0, 0, 0);

  if IsHorz then
    Delta:= FIndentCorner * h div 100
  else
    Delta:= FIndentCorner * w div 100;

  if IsHorz then
  begin
    if Delta>0 then
    begin
      FRectCorner:= Rect(w-Delta, 0, w, h);
      Dec(FRectMain.Right, Delta);
    end
    else
    if Delta<0 then
    begin
      FRectCorner:= Rect(0, 0, Abs(Delta), h);
      Inc(FRectMain.Left, Abs(Delta));
    end;
  end
  else
  begin
    if Delta>0 then
    begin
      FRectCorner:= Rect(0, h-Delta, w, h);
      Dec(FRectMain.Bottom, Delta);
    end
    else
    if Delta<0 then
    begin
      FRectCorner:= Rect(0, 0, w, Abs(Delta));
      Inc(FRectMain.Top, Abs(Delta));
    end;
  end;
end;

initialization

  with ATScrollbarTheme do
  begin
    ColorBG:= $d0d0d0;
    ColorBorder:= clLtGray;
    ColorThumbBorder:= $808080;
    ColorThumbFill:= $c0c0c0;
    ColorThumbFillOver:= $d0d0d0;
    ColorThumbFillPressed:= $e0c0c0;
    ColorThumbDecor:= ColorThumbBorder;
    ColorThumbDecor2:= clWhite;

    ColorArrowBorder:= $808080;
    ColorArrowFill:= $c0c0c0;
    ColorArrowFillOver:= $d0d0d0;
    ColorArrowFillPressed:= $e0a0a0;

    ColorArrowSign:= $404040;
    ColorScrolled:= $d0b0b0;

    InitialSize:= 16;
    ScalePercents:= 100;
    ArrowStyleH:= asaArrowsNormal;
    ArrowStyleV:= asaArrowsNormal;
    ArrowSize:= 2;
    ArrowLengthPercents:= 100;
    BorderSize:= 0;
    TimerInterval:= 200;
    DirectJumpOnClickPageUpDown:= false;

    MinSizeToShowThumb:= 10;
    ThumbMinSize:= 8;
    ThumbMarkerOffset:= 4;
    ThumbMarkerMinimalSize:= 20;
    ThumbMarkerDecorSize:= 2;
    ThumbMarkerDecorSpace:= 2;
    ThumbMarkerDecorDouble:= false;
  end;

end.
