{
ATStatusBar component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL
}

unit ATStatusBar;

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows,
  Messages,
  {$endif}
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls,
  ATFlatThemes;

type

  { TATStatusData }

  TATStatusData = class(TCollectionItem)
  private
    FWidth: integer;
    FAlign: TAlignment;
    FCaption: TCaption;
    FHint: string;
    FImageIndex: integer;
    FAutoSize: boolean;
    FAutoStretch: boolean;
    FColorFont: TColor; //integer;
    FColorBack: TColor; //integer;
    FFontName: string;
    FFontSize: integer;
    FTag: Int64;

  public
    constructor Create(ACollection: TCollection); override;
  published
    property Width: integer read FWidth write FWidth;
    property Align: TAlignment read FAlign write FAlign default taLeftJustify;
    property Caption: TCaption read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property ImageIndex: integer read FImageIndex write FImageIndex default -1;
    property AutoSize: boolean read FAutoSize write FAutoSize default false;
    property AutoStretch: boolean read FAutoStretch write FAutoStretch default false;
    property ColorFont: TColor read FColorFont write FColorFont default clNone;
    property ColorBack: TColor read FColorBack write FColorBack default clNone;
    property FontName: string read FFontName write FFontName;
    property FontSize: integer read FFontSize write FFontSize default 0;
    property Tag: Int64 read FTag write FTag default 0;
  end;

type
  TATStatusClickEvent = procedure (Sender: TObject; AIndex: integer) of object;
  TATStatusDrawEvent = procedure (Sender: TObject; AIndex: integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

const
  cDefaultStatusbarPadding = 2;
  cDefaultStatusbarColorBack = clBtnFace;
  cDefaultStatusbarColorBorderTop = clGray;
  cDefaultStatusbarColorBorderR = clGray;
  cDefaultStatusbarColorBorderL = clNone;
  cDefaultStatusbarColorBorderU = clNone;
  cDefaultStatusbarColorBorderD = clNone;

type
  { TATStatus }

  TATStatus = class(TCustomControl)
  private
    FColorBorderTop: TColor;
    FColorBorderR: TColor;
    FColorBorderL: TColor;
    FColorBorderU: TColor;
    FColorBorderD: TColor;
    FHeightInitial: integer;
    FPadding: integer;
    FClickedIndex: integer;
    FPrevPanelMouseOver: integer;

    FItems: TCollection;
    FBitmap: TBitmap;
    FImages: TImageList;
    FTheme: PATFlatTheme;

    FOnPanelClick: TATStatusClickEvent;
    FOnPanelDrawBefore: TATStatusDrawEvent;
    FOnPanelDrawAfter: TATStatusDrawEvent;

    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintPanelTo(C: TCanvas; ARect: TRect; AData: TATStatusData);
    function DoDrawBefore(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoDrawAfter(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function GetCaption(AIndex: integer): TCaption;
    function GetHint(AIndex: integer): string;
    procedure SetCaption(AIndex: integer; const AValue: TCaption);
    procedure SetHint(AIndex: integer; const AValue: string); reintroduce;
    procedure UpdateCanvasFont(C: TCanvas; D: TATStatusData);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    function GetPanelRect(AIndex: integer): TRect;
    function GetPanelAt(X, Y: integer): integer;
    function GetPanelData(AIndex: integer): TATStatusData;
    function PanelCount: integer;
    function IsIndexOk(AIndex: integer): boolean;
    procedure AddPanel(
      APanelIndex: integer;
      AWidth: integer;
      AAlign: TAlignment;
      const ACaption: TCaption= '';
      AImageIndex: integer= - 1;
      ATag: IntPtr= 0;
      AAutoSize: boolean= false;
      AAutoStretch: boolean= false;
      AFontColor: TColor=clNone);
    procedure DeletePanel(AIndex: integer);
    procedure DeletePanels;
    property Theme: PATFlatTheme read FTheme write FTheme;
    property Captions[AIndex: integer]: TCaption read GetCaption write SetCaption;
    property Hints[AIndex: integer]: string read GetHint write SetHint;
    procedure DoPanelStretch(AIndex: integer);
    procedure DoPanelAutoWidth(C: TCanvas; AIndex: integer);
    function FindPanel(ATag: IntPtr): integer;
    property HeightInitial: integer read FHeightInitial write FHeightInitial;
    procedure Invalidate; override;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}

  published
    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderSpacing;
    {$endif}
    property DoubleBuffered;
    property Enabled;
    property Visible;
    property Color default cDefaultStatusbarColorBack;
    property ColorBorderTop: TColor read FColorBorderTop write FColorBorderTop default cDefaultStatusbarColorBorderTop;
    property ColorBorderR: TColor read FColorBorderR write FColorBorderR default cDefaultStatusbarColorBorderR;
    property ColorBorderL: TColor read FColorBorderL write FColorBorderL default cDefaultStatusbarColorBorderL;
    property ColorBorderU: TColor read FColorBorderU write FColorBorderU default cDefaultStatusbarColorBorderU;
    property ColorBorderD: TColor read FColorBorderD write FColorBorderD default cDefaultStatusbarColorBorderD;
    property Padding: integer read FPadding write FPadding default cDefaultStatusbarPadding;
    property Panels: TCollection read FItems write FItems;
    property Images: TImageList read FImages write FImages;
    property ShowHint;
    property ParentShowHint;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnResize;
    property OnPanelClick: TATStatusClickEvent read FOnPanelClick write FOnPanelClick;
    property OnPanelDrawBefore: TATStatusDrawEvent read FOnPanelDrawBefore write FOnPanelDrawBefore;
    property OnPanelDrawAfter: TATStatusDrawEvent read FOnPanelDrawAfter write FOnPanelDrawAfter;
  end;

implementation

uses
  SysUtils, Forms, Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

{ TATStatusData }

constructor TATStatusData.Create(ACollection: TCollection);
begin
  inherited;
  FAlign:= taLeftJustify;
  FImageIndex:= -1;
  FAutoSize:= false;
  FAutoStretch:= false;
  FWidth:= 100;
  FColorFont:= clNone;
  FColorBack:= clNone;
  FFontName:= '';
  FFontSize:= 0;
  FTag:= 0;
end;

{ TATStatus }

function TATStatus.IsIndexOk(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FItems.Count);
end;

function TATStatus.PanelCount: integer;
begin
  Result:= FItems.Count;
end;

constructor TATStatus.Create(AOnwer: TComponent);
begin
  inherited;

  Align:= alBottom;
  Caption:= '';
  {$ifdef FPC}
  BorderStyle:= bsNone;
  {$endif}
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;

  Width:= 400;
  Height:= 24;

  FTheme:= @ATFlatTheme;
  FHeightInitial:= Height;
  FPadding:= cDefaultStatusbarPadding;

  Color:= cDefaultStatusbarColorBack;
  FColorBorderTop:= cDefaultStatusbarColorBorderTop;
  FColorBorderR:= cDefaultStatusbarColorBorderR;
  FColorBorderL:= cDefaultStatusbarColorBorderL;
  FColorBorderU:= cDefaultStatusbarColorBorderU;
  FColorBorderD:= cDefaultStatusbarColorBorderD;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FItems:= TCollection.Create(TATStatusData);
end;

destructor TATStatus.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);

  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATStatus.Paint;
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

procedure TATStatus.UpdateCanvasFont(C: TCanvas; D: TATStatusData);
begin
  if D.FontName<>'' then
    C.Font.Name:= D.FontName
  else
    C.Font.Name:= Theme^.FontName;

  if D.FontSize>0 then
    C.Font.Size:= Theme^.DoScaleFont(D.FontSize)
  else
    C.Font.Size:= Theme^.DoScaleFont(Theme^.FontSize);

  if D.ColorFont<>clNone then
    C.Font.Color:= ColorToRGB(D.ColorFont)
  else
    C.Font.Color:= ColorToRGB(Theme^.ColorFont);
end;

procedure TATStatus.DoPaintPanelTo(C: TCanvas; ARect: TRect; AData: TATStatusData);
var
  RectText: TRect;
  PosIcon: TPoint;
  TextSize: TSize;
  NOffsetLeft, NPad: integer;
begin
  if AData.ColorBack<>clNone then
    C.Brush.Color:= ColorToRGB(AData.ColorBack)
  else
    C.Brush.Color:= ColorToRGB(Color);
  C.FillRect(ARect);

  NPad:= Theme^.DoScale(FPadding);
  RectText:= Rect(ARect.Left+NPad, ARect.Top, ARect.Right-NPad, ARect.Bottom);

  if Assigned(FImages) then
    if AData.ImageIndex>=0 then
    begin
      if AData.Caption='' then
        case AData.Align of
          taLeftJustify:
            PosIcon.x:= ARect.Left+NPad;
          taRightJustify:
            PosIcon.x:= (ARect.Right-FImages.Width-NPad);
          taCenter:
            PosIcon.x:= (ARect.Left+ARect.Right-FImages.Width) div 2
        end
      else
        PosIcon.x:= ARect.Left+NPad;
      PosIcon.y:= (ARect.Top+ARect.Bottom-FImages.Height) div 2;

      FImages.Draw(C, PosIcon.x, PosIcon.y, AData.ImageIndex);
      Inc(RectText.Left, FImages.Width);
    end;

  if AData.Caption<>'' then
  begin
    C.FillRect(RectText);
    UpdateCanvasFont(C, AData);
    TextSize:= C.TextExtent(AData.Caption);

    case AData.Align of
      taLeftJustify:
        NOffsetLeft:= 0;
      taRightJustify:
        NOffsetLeft:= RectText.Right-RectText.Left-TextSize.cx;
      taCenter:
        NOffsetLeft:= (RectText.Right-RectText.Left-TextSize.cx) div 2;
    end;

    ExtTextOut(C.Handle,
      RectText.Left+NOffsetLeft,
      (ARect.Top+ARect.Bottom-TextSize.cy) div 2+1,
      ETO_CLIPPED+ETO_OPAQUE,
      @RectText,
      PChar(AData.Caption),
      Length(AData.Caption),
      nil);
  end;

  if FColorBorderR<>clNone then
  begin
    C.Pen.Color:= ColorToRGB(FColorBorderR);
    C.MoveTo(ARect.Right, ARect.Top);
    C.LineTo(ARect.Right, ARect.Bottom);
  end;

  if FColorBorderL<>clNone then
  begin
    C.Pen.Color:= ColorToRGB(FColorBorderL);
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Left, ARect.Bottom);
  end;

  if FColorBorderU<>clNone then
  begin
    C.Pen.Color:= ColorToRGB(FColorBorderU);
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Right, ARect.Top);
  end;

  if FColorBorderD<>clNone then
  begin
    C.Pen.Color:= ColorToRGB(FColorBorderD);
    C.MoveTo(ARect.Left, ARect.Bottom-1);
    C.LineTo(ARect.Right, ARect.Bottom-1);
  end;  
end;

function TATStatus.GetPanelRect(AIndex: integer): TRect;
var
  Data: TATStatusData;
  NSize, i: integer;
begin
  Result.Left:= 0;
  Result.Right:= -1;
  Result.Top:= 1;
  Result.Bottom:= ClientHeight;

  if IsIndexOk(AIndex) then
    for i:= 0 to PanelCount-1 do
    begin
      Data:= GetPanelData(i);
      Result.Left:= Result.Right + 1;

      NSize:= Data.Width;
      if not Data.AutoSize and not Data.AutoStretch then
        NSize:= Theme^.DoScale(NSize);

      Result.Right:= Result.Left + NSize - 1;
      if AIndex=i then Exit;
    end;
end;

procedure TATStatus.DoPaintTo(C: TCanvas);
var
  PanelRect: TRect;
  D: TATStatusData;
  i: integer;
begin
  C.Brush.Color:= ColorToRGB(Color);
  C.FillRect(ClientRect);

  //consider AutoSize
  for i:= 0 to PanelCount-1 do
  begin
    D:= GetPanelData(i);
    if Assigned(D) and D.AutoSize then
      DoPanelAutoWidth(C, i);
  end;

  //consider AutoStretch
  for i:= 0 to PanelCount-1 do
  begin
    D:= GetPanelData(i);
    if Assigned(D) and not D.AutoSize and D.AutoStretch then
    begin
      DoPanelStretch(i);
      Break; //allowed for single panel
    end;
  end;

  //paint panels
  for i:= 0 to PanelCount-1 do
  begin
    PanelRect:= GetPanelRect(i);
    if DoDrawBefore(i, C, PanelRect) then
    begin
      DoPaintPanelTo(C, PanelRect, TATStatusData(FItems.Items[i]));
      DoDrawAfter(i, C, PanelRect);
    end;  
  end;

  C.Pen.Color:= ColorToRGB(FColorBorderTop);
  C.MoveTo(0, 0);
  C.LineTo(ClientWidth, 0);
end;


function TATStatus.GetPanelAt(X, Y: integer): integer;
var
  i: integer;
  Pnt: TPoint;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  for i:= 0 to PanelCount-1 do
    if PtInRect(GetPanelRect(i), Pnt) then exit(i);
end;

procedure TATStatus.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  FClickedIndex:= GetPanelAt(X, Y);
end;

function TATStatus.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATStatus.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, Width);
    FBitmap.Height:= Max(FBitmap.Height, Height);
  end;
  Invalidate;
end;

procedure TATStatus.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NPanel: integer;
begin
  inherited;

  NPanel:= GetPanelAt(X, Y);
  if NPanel<0 then
  begin
    Hint:= '';
    Application.HideHint;
    exit;
  end;

  Hint:= Hints[NPanel];

  if NPanel<>FPrevPanelMouseOver then
  begin
    FPrevPanelMouseOver:= NPanel;
    Application.HideHint;
    if ShowHint and (Hint<>'') then
      Application.ActivateHint(ClientToScreen(Point(X, Y)));
  end;
end;


procedure TATStatus.AddPanel(
  APanelIndex: integer;
  AWidth: integer;
  AAlign: TAlignment;
  const ACaption: TCaption='';
  AImageIndex: integer=-1;
  ATag: IntPtr= 0;
  AAutoSize: boolean=false;
  AAutoStretch: boolean=false;
  AFontColor: TColor=clNone);
var
  Data: TATStatusData;
begin
  if APanelIndex<0 then
    Data:= FItems.Add as TATStatusData
  else
    Data:= FItems.Insert(APanelIndex) as TATStatusData;

  Data.Width:= AWidth;;
  Data.Align:= AAlign;
  Data.Caption:= ACaption;
  Data.ColorFont:= AFontColor;
  Data.ImageIndex:= AImageIndex;
  Data.AutoSize:= AAutoSize;
  Data.AutoStretch:= AAutoStretch;
  Data.Tag:= ATag;
  Invalidate;
end;

procedure TATStatus.DeletePanel(AIndex: integer);
begin
  if IsIndexOk(AIndex) then
  begin
    FItems.Delete(AIndex);
    Invalidate;
  end;
end;

procedure TATStatus.DeletePanels;
begin
  while PanelCount>0 do
    DeletePanel(PanelCount-1);
end;

function TATStatus.GetPanelData(AIndex: integer): TATStatusData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATStatusData(FItems.Items[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATStatus.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATStatus.Click;
begin
  inherited;
  if Assigned(FOnPanelClick) then
    FOnPanelClick(Self, FClickedIndex);
end;

function TATStatus.DoDrawBefore(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawBefore) then
    FOnPanelDrawBefore(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.DoDrawAfter(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawAfter) then
    FOnPanelDrawAfter(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.GetCaption(AIndex: integer): TCaption;
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
    Result:= D.Caption
  else
    Result:= '';
end;

procedure TATStatus.SetCaption(AIndex: integer; const AValue: TCaption);
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
  begin
    D.Caption:= AValue;
    Invalidate;
  end;
end;

function TATStatus.GetHint(AIndex: integer): string;
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
    Result:= D.Hint
  else
    Result:= '';
end;

procedure TATStatus.SetHint(AIndex: integer; const AValue: string);
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
  begin
    D.Hint:= AValue;
    Invalidate;
  end;
end;

procedure TATStatus.DoPanelStretch(AIndex: integer);
var
  NSize, NCell, i: integer;
  D: TATStatusData;
begin
  if not IsIndexOk(AIndex) then exit;

  NSize:= 0;
  for i:= 0 to PanelCount-1 do
    if i<>AIndex then
    begin
      D:= GetPanelData(i);
      NCell:= D.Width;
      if not D.AutoSize then
        NCell:= Theme^.DoScale(NCell);
      if Assigned(D) then
        Inc(NSize, NCell);
    end;

  D:= GetPanelData(AIndex);
  if Assigned(D) then
    D.Width:= Max(0, Width-NSize);
end;


procedure TATStatus.DoPanelAutoWidth(C: TCanvas; AIndex: integer);
var
  NSize, NPad: integer;
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
  begin
    NPad:= Theme^.DoScale(FPadding);
    NSize:= NPad*2+2;
    if D.ImageIndex>=0 then
      Inc(NSize, Images.Width+NPad);

    if D.Caption<>'' then
    begin
      UpdateCanvasFont(C, D);
      Inc(NSize, C.TextWidth(D.Caption));
    end;

    D.Width:= NSize;
  end;
end;


function TATStatus.FindPanel(ATag: IntPtr): integer;
var
  D: TATStatusData;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to PanelCount-1 do
  begin
    D:= GetPanelData(i);
    if Assigned(D) and (D.Tag=ATag) then exit(i);
  end;
end;

procedure TATStatus.Invalidate;
begin
  if FHeightInitial>0 then
    Height:= Theme^.DoScale(FHeightInitial);
  inherited Invalidate;
end;


end.
