{
Copyright (C) Alexey Torgashin, uvviewsoft.com
Written for Lazarus LCL
License: MPL 2.0 or LGPL or any license which LCL can use
}

unit ATGauge;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Math, Types,
  {$ifdef FPC}
  InterfaceBase,
  LCLType, LCLIntf,
  {$endif}
  ATFlatThemes;

type
  TATGaugeKind = (
    gkText,
    gkHorizontalBar,
    gkVerticalBar,
    gkPie,
    gkNeedle,
    gkHalfPie
    );

type
  { TATGauge }

  TATGauge = class(TGraphicControl)
  private
    FDoubleBuffered: boolean;
    FBitmap: TBitmap;
    {$ifdef FPC}
    FBorderStyle: TBorderStyle;
    {$endif}
    FKind: TATGaugeKind;
    FMinValue: integer;
    FMaxValue: integer;
    FProgress: integer;
    FShowText: boolean;
    FShowTextInverted: boolean;
    FTheme: PATFlatTheme;
    procedure DoPaintTextInverted(C: TCanvas; r: TRect; const Str: string);
    procedure DoPaintTextUsual(C: TCanvas; r: TRect; const Str: string);
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function GetPercentDone: integer;
    function GetPartDoneFloat: Double;
    {$ifdef FPC}
    procedure SetBorderStyle(AValue: TBorderStyle);
    {$endif}
    procedure SetKind(AValue: TATGaugeKind);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetProgress(AValue: integer);
    procedure SetShowText(AValue: boolean);
    procedure SetShowTextInverted(AValue: boolean);
  protected
    procedure Paint; override;
    {$ifdef FPC}
    procedure DoOnResize; override;
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress(AValue: integer);
    property PercentDone: integer read GetPercentDone;
    property Theme: PATFlatTheme read FTheme write FTheme;
  published
    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderSpacing;
    {$endif}
    property Color;
    property Constraints;
    property DoubleBuffered: boolean read FDoubleBuffered write FDoubleBuffered;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Kind: TATGaugeKind read FKind write SetKind default gkHorizontalBar;
    property Progress: integer read FProgress write SetProgress default 0;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property ShowText: boolean read FShowText write SetShowText default true;
    property ShowTextInverted: boolean read FShowTextInverted write SetShowTextInverted default false;
    property OnClick;
    property OnDblClick;
    property OnResize;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= true;
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$endif}
end;

{ TATGauge }

procedure TATGauge.DoPaintTextUsual(C: TCanvas; r: TRect; const Str: string);
var
  StrSize: TSize;
begin
  StrSize:= C.TextExtent(Str);
  C.Font.Name:= Theme^.FontName;
  C.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);
  C.Brush.Style:= bsClear;
  C.TextOut(
    (r.Left+r.Right-StrSize.cx) div 2,
    (r.Top+r.Bottom-StrSize.cy) div 2,
    Str);
  C.Brush.Style:= bsSolid;
end;

procedure TATGauge.DoPaintTextInverted(C: TCanvas; r: TRect; const Str: string);
const
  ColorEmpty = clBlack;
var
  StrSize: TSize;
  Bmp: TBitmap;
  Pnt: TPoint;
begin
  StrSize:= C.TextExtent(Str);

  Bmp:= TBitmap.Create;
  try
    Bmp.PixelFormat:= pf24bit;
    Bmp.SetSize(StrSize.cx, StrSize.cy);
    Bmp.Transparent:= true;
    Bmp.TransparentColor:= ColorEmpty;

    Bmp.Canvas.Brush.Color:= ColorEmpty;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    Bmp.Canvas.Font.Name:= Theme^.FontName;
    Bmp.Canvas.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);
    Bmp.Canvas.Font.Color:= Theme^.ColorFont;
    Bmp.Canvas.Font.Quality:= fqNonAntialiased; //antialias
    {$ifdef FPC}
    Bmp.Canvas.AntialiasingMode:= amOn; //antialias
    {$endif}
    Bmp.Canvas.TextOut(0, 0, Str);

    Pnt.X:= (r.Left+r.Right-StrSize.cx) div 2;
    Pnt.Y:= (r.Top+r.Bottom-StrSize.cy) div 2;

    C.CopyMode:= cmSrcInvert;
    C.CopyRect(
      Rect(Pnt.X, Pnt.Y, Pnt.X+StrSize.cx, Pnt.Y+StrSize.cy),
      Bmp.Canvas,
      Rect(0, 0, StrSize.cx, StrSize.cy));
    C.CopyMode:= cmSrcCopy;
  finally
    Bmp.Free;
  end;
end;

procedure TATGauge.DoPaintTo(C: TCanvas; r: TRect);
  //
  procedure DoFillBG(AColor: TColor);
  begin
    C.Pen.Color:= AColor;
    C.Brush.Color:= AColor;
    C.FillRect(r);
  end;
  //
var
  NSize: integer;
  Alfa: double;
  Str: string;
  r2: TRect;
begin
  case FKind of
    gkText:
      begin
        DoFillBG(Theme^.ColorBgPassive);
      end;

    gkHorizontalBar:
      begin
        DoFillBG(Theme^.ColorBgPassive);

        C.Brush.Color:= Theme^.ColorBgOver;
        NSize:= Round((r.Right-r.Left) * GetPartDoneFloat);
        C.FillRect(Rect(r.Left, r.Top, r.Left+NSize, r.Bottom));
      end;

    gkVerticalBar:
      begin
        DoFillBG(Theme^.ColorBgPassive);

        C.Brush.Color:= Theme^.ColorBgOver;
        NSize:= Round((r.Bottom-r.Top) * GetPartDoneFloat);
        C.FillRect(Rect(r.Left, r.Bottom-NSize, r.Right, r.Bottom));
      end;

    gkNeedle,
    gkHalfPie:
      begin
        DoFillBG(Color);



        {$ifdef FPC}
        if FBorderStyle<>bsNone then NSize:= 1 else NSize:= 0;
        {$else}
        NSize:= 0;
        {$endif}
        r2:= Rect(
          r.Left+NSize, r.Top+NSize,
          r.Right-1-NSize, r.Bottom-1+(r.Bottom-r.Top)-NSize);

        C.Pen.Color:= Theme^.ColorBgOver;
        C.Brush.Color:= Theme^.ColorBgPassive;
        C.Pie(r2.Left, r2.Top, r2.Right, r2.Bottom,
          r.Right, r.Bottom,
          r.Left, r.Bottom);

        if FKind=gkHalfPie then
          C.Brush.Color:= Theme^.ColorBgOver;

        Alfa:= pi*GetPartDoneFloat;
        C.Pie(r2.Left, r2.Top, r2.Right, r2.Bottom,
          r2.Left-Round(1000*cos(Alfa)),
          r2.Bottom-Round(1000*sin(Alfa)),
          r2.Left,
          r2.Bottom);

        C.Pen.Color:= Theme^.ColorBgOver;
        C.MoveTo(r2.Left, r.Bottom-1-NSize);
        C.LineTo(r2.Right, r.Bottom-1-NSize);
      end;

    gkPie:
      begin
        DoFillBG(Color);

        {$ifdef FPC}
        if FBorderStyle<>bsNone then NSize:= 1 else NSize:= 0;
        {$else}
        NSize:= 0;
        {$endif}
        r2:= Rect(r.Left+NSize, r.Top+NSize, r.Right-NSize, r.Bottom-NSize);

        C.Pen.Color:= Theme^.ColorBgOver;
        C.Brush.Color:= Theme^.ColorBgPassive;
        C.Ellipse(r2);

        if FProgress>FMinValue then
        begin
          Alfa:= 2*pi*GetPartDoneFloat;
          C.Pen.Color:= Theme^.ColorBgOver;
          C.Brush.Color:= Theme^.ColorBgOver;

          C.Pie(r2.Left, r2.Top, r2.Right, r2.Bottom,
            (r2.Left+r2.Right) div 2 + Round(1000*sin(Alfa)),
            (r2.Top+r2.Bottom) div 2 - Round(1000*cos(Alfa)),
            (r2.Left+r2.Right) div 2,
            r.Top
           );
        end;
      end;
  end;

  //paint text
  if FShowText then
  begin
    Str:= IntToStr(PercentDone)+'%';
    if FShowTextInverted then
      DoPaintTextInverted(C, r, Str)
    else
      DoPaintTextUsual(C, r, Str);
  end;

  //paint border
  {$ifdef FPC}
  if FBorderStyle<>bsNone then
  begin
    C.Pen.Color:= Theme^.ColorBorderPassive;
    C.Brush.Style:= bsClear;
    C.Rectangle(r);
    C.Brush.Style:= bsSolid;
  end;
  {$endif}

end;

function TATGauge.GetPartDoneFloat: Double;
begin
  Result:= (FProgress-FMinValue) / (FMaxValue-FMinValue);
end;

function TATGauge.GetPercentDone: integer;
begin
  Result:= Round(100 * GetPartDoneFloat);
end;

{$ifdef FPC}
procedure TATGauge.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  Invalidate;
end;
{$endif}

procedure TATGauge.SetKind(AValue: TATGaugeKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
  Invalidate;
end;

procedure TATGauge.SetMaxValue(AValue: integer);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=Max(FMinValue+1, AValue);
  FProgress:=Min(FProgress, FMaxValue);
  Invalidate;
end;

procedure TATGauge.SetMinValue(AValue: integer);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=Min(FMaxValue-1, AValue);
  FProgress:=Max(FProgress, FMinValue);
  Invalidate;
end;

procedure TATGauge.SetProgress(AValue: integer);
begin
  if FProgress=AValue then Exit;
  FProgress:=Max(FMinValue, Min(FMaxValue, AValue));
  Invalidate;
end;

procedure TATGauge.SetShowText(AValue: boolean);
begin
  if FShowText=AValue then Exit;
  FShowText:=AValue;
  Invalidate;
end;

procedure TATGauge.SetShowTextInverted(AValue: boolean);
begin
  if FShowTextInverted=AValue then Exit;
  FShowTextInverted:=AValue;
  Invalidate;
end;

procedure TATGauge.Paint;
var
  R: TRect;
begin
  inherited;

  R:= ClientRect;
  if DoubleBuffered then
  begin
    FBitmap.Canvas.Font.Name:= Theme^.FontName;
    FBitmap.Canvas.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);
    FBitmap.Canvas.Font.Color:= Theme^.ColorFont;
    DoPaintTo(FBitmap.Canvas, R);
    Canvas.CopyRect(R, FBitmap.Canvas, R);
  end
  else
    DoPaintTo(Canvas, R);
end;


constructor TATGauge.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque {$ifdef FPC}, csNoFocus{$endif}]
    -[csDoubleClicks {$ifdef FPC},csTripleClicks{$endif}];

  Width:= 150;
  Height:= 50;
  Color:= clBtnFace;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.SetSize(500, 80);

  FDoubleBuffered:= IsDoubleBufferedNeeded;
  FKind:= gkHorizontalBar;
  {$ifdef FPC}
  FBorderStyle:= bsSingle;
  {$endif}

  FMinValue:= 0;
  FMaxValue:= 100;
  FProgress:= 0;
  FShowText:= true;
  FShowTextInverted:= false;
  FTheme:= @ATFlatTheme;
end;

destructor TATGauge.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATGauge.AddProgress(AValue: integer);
begin
  Progress:= Progress+AValue;
end;

{$ifdef FPC}
procedure TATGauge.DoOnResize;
const
  cResizeBitmapStep = 100;
var
  SizeX, SizeY: integer;
begin
  inherited;

  if DoubleBuffered then
  if Assigned(FBitmap) then
  begin
    SizeX:= (Width div cResizeBitmapStep + 1)*cResizeBitmapStep;
    SizeY:= (Height div cResizeBitmapStep + 1)*cResizeBitmapStep;
    if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
    begin
      FBitmap.SetSize(SizeX, SizeY);
      FBitmap.FreeImage; //recommended, else seen black bitmap on bigsize
    end;
  end;

  Invalidate;
end;
{$endif}


end.

