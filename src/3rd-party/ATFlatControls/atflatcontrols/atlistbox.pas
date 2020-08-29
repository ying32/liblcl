{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATListbox;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  {$ifdef FPC}
  LMessages,
  {$else}
  Messages, Windows, System.UITypes,
  {$endif}
  StrUtils,
  ATScrollBar,
  ATFlatThemes,
  ATCanvasPrimitives;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect) of object;
  TATListboxCalcWidth = function (Sender: TObject; C: TCanvas): integer of object;

type
  TATIntArray = array of integer;

type
  TATListboxShowX = (
    albsxNone,
    albsxAllItems,
    albsxHotItem
    );

  TATListboxScrollStyle = (
    alssHide,
    alssShow,
    alssAuto
    );

type
  { TATListboxItemProp }

  TATListboxItemProp = class
  public
    Tag: Int64;
    Modified: boolean;
    DataText: string;
    constructor Create(const ATag: Int64; AModified: boolean; const ADataText: string);
  end;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FTheme: PATFlatTheme;
    FThemedScrollbar: boolean;
    FThemedFont: boolean;
    FScrollbar: TATScrollbar;
    FScrollbarHorz: TATScrollbar;
    FScrollStyleHorz: TATListboxScrollStyle;
    FScrollStyleVert: TATListboxScrollStyle;
    FOwnerDrawn: boolean;
    FVirtualMode: boolean;
    FVirtualItemCount: integer;
    FItemIndex: integer;
    FItemHeightPercents: integer;
    FItemHeight: integer;
    FItemHeightIsFixed: boolean;
    FItemTop: integer;
    FScrollHorz: integer;
    FBitmap: Graphics.TBitmap;
    FCanGetFocus: boolean;
    FList: TStringList;
    FHotTrack: boolean;
    FHotTrackIndex: integer;
    FIndentLeft: integer;
    FIndentTop: integer;
    FIndentForX: integer;
    FColumnSep: char;
    FColumnSizes: TATIntArray;
    FColumnWidths: TATIntArray;
    FShowX: TATListboxShowX;
    FMaxWidth: integer;
    FOnDrawItem: TATListboxDrawItemEvent;
    FOnCalcScrollWidth: TATListboxCalcWidth;
    FOnClickX: TNotifyEvent;
    FOnChangeSel: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FShowOsBarVert: boolean;
    FShowOsBarHorz: boolean;
    procedure SetShowOsBarVert(AValue: boolean);
    procedure SetShowOsBarHorz(AValue: boolean);
    property ShowOsBarVert: boolean read FShowOsBarVert write SetShowOsBarVert;
    property ShowOsBarHorz: boolean read FShowOsBarHorz write SetShowOsBarHorz;
    procedure DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
    procedure DoPaintTo(C: TCanvas; r: TRect);
    procedure DoPaintX(C: TCanvas; const R: TRect; ACircle: boolean);
    function GetMaxWidth(C: TCanvas): integer;
    function GetOnDrawScrollbar: TATScrollbarDrawEvent;
    function ItemBottom: integer;
    procedure ScrollbarChange(Sender: TObject);
    procedure ScrollbarHorzChange(Sender: TObject);
    procedure SetCanBeFocused(AValue: boolean);
    procedure SetItemHeightPercents(AValue: integer);
    procedure SetOnDrawScrollbar(AValue: TATScrollbarDrawEvent);
    procedure SetScrollHorz(AValue: integer);
    procedure SetVirtualItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure SetItemHeight(AValue: integer);
    procedure SetThemedScrollbar(AValue: boolean);
    procedure UpdateColumnWidths;
    procedure UpdateFromScrollbarMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMVScroll{$endif});
    procedure UpdateFromScrollbarHorzMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMHScroll{$endif});
    {$ifndef FPC}
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure UpdateScrollbars(C: TCanvas);
    function GetVisibleItems: integer;
    function GetItemHeightDefault: integer;
    function GetColumnWidth(AIndex: integer): integer;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    function CurrentFontName: string;
    function CurrentFontSize: integer;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure Resize; override;
    {$ifdef FPC}
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure LMHScroll(var Msg: TLMHScroll); message LM_HSCROLL;
    procedure MouseLeave; override;
    {$else}
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure ChangedSelection; virtual;
    procedure Scrolled; virtual;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FList;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property ItemHeightDefault: integer read GetItemHeightDefault;
    function ItemCount: integer;
    function IsIndexValid(AValue: integer): boolean;
    property ScrollHorz: integer read FScrollHorz write SetScrollHorz;
    property HotTrackIndex: integer read FHotTrackIndex;
    property VirtualItemCount: integer read FVirtualItemCount write SetVirtualItemCount;
    property VisibleItems: integer read GetVisibleItems;
    function GetItemIndexAt(Pnt: TPoint): integer;
    property Theme: PATFlatTheme read FTheme write FTheme;
    property ThemedScrollbar: boolean read FThemedScrollbar write SetThemedScrollbar;
    property ThemedFont: boolean read FThemedFont write FThemedFont;
    property Scrollbar: TATScrollbar read FScrollbar;
    property ScrollbarHorz: TATScrollbar read FScrollbarHorz;
    property ColumnSeparator: char read FColumnSep write FColumnSep;
    property ColumnSizes: TATIntArray read FColumnSizes write FColumnSizes;
    property ColumnWidth[AIndex: integer]: integer read GetColumnWidth;
    {$ifdef FPC}
    function CanFocus: boolean; override;
    function CanSetFocus: boolean; override;
    {$endif}
    function ClientHeight: integer;
    function ClientWidth: integer;
    procedure Invalidate; override;
    procedure UpdateItemHeight;
  published
    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderStyle;
    property BorderSpacing;
    {$endif}
    property CanGetFocus: boolean read FCanGetFocus write SetCanBeFocused default false;
    property DoubleBuffered stored false;
    property Enabled;
    property HotTrack: boolean read FHotTrack write FHotTrack default false;
    property IndentLeft: integer read FIndentLeft write FIndentLeft default 4;
    property IndentTop: integer read FIndentTop write FIndentTop default 2;
    property ItemHeightPercents: integer read FItemHeightPercents write SetItemHeightPercents default 100;
    property OwnerDrawn: boolean read FOwnerDrawn write FOwnerDrawn default false;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollStyleHorz: TATListboxScrollStyle read FScrollStyleHorz write FScrollStyleHorz default alssAuto;
    property ScrollStyleVert: TATListboxScrollStyle read FScrollStyleVert write FScrollStyleVert default alssShow;
    property ShowHint;
    property ShowXMark: TATListboxShowX read FShowX write FShowX default albsxNone;
    property VirtualMode: boolean read FVirtualMode write FVirtualMode default true;
    property Visible;
    property OnClick;
    property OnClickXMark: TNotifyEvent read FOnClickX write FOnClickX;
    property OnDblClick;
    property OnContextPopup;
    property OnChangedSel: TNotifyEvent read FOnChangeSel write FOnChangeSel;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnCalcScrollWidth: TATListboxCalcWidth read FOnCalcScrollWidth write FOnCalcScrollWidth;
    property OnDrawScrollbar: TATScrollbarDrawEvent read GetOnDrawScrollbar write SetOnDrawScrollbar;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnResize;
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

uses
  {$ifdef FPC}
  Types,
  InterfaceBase,
  LCLType, LCLIntf,
  {$endif}
  Math;

type
  TATStringSeparator = record
  private
    FSep: char;
    FStr: string;
    FPos: integer;
  public
    procedure Init(const AStr: string; ASep: char);
    function GetItemStr(out AValue: string): boolean;
  end;

procedure TATStringSeparator.Init(const AStr: string; ASep: char);
begin
  FStr:= AStr;
  FSep:= ASep;
  FPos:= 1;
end;

function TATStringSeparator.GetItemStr(out AValue: string): boolean;
var
  N: integer;
begin
  if FPos>Length(FStr) then
  begin
    AValue:= '';
    exit(false);
  end;
  N:= PosEx(FSep, FStr, FPos);
  if N=0 then
    N:= Length(FStr)+1;
  AValue:= Copy(FStr, FPos, N-FPos);
  FPos:= N+1;
  Result:= true;
end;

function IsDoubleBufferedNeeded: boolean;
begin
  Result := true;
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$endif}
end;

{ TATListboxItemProp }

constructor TATListboxItemProp.Create(const ATag: Int64; AModified: boolean;
  const ADataText: string);
begin
  Tag:= ATag;
  Modified:= AModified;
  DataText:= ADataText;
end;

{ TATListbox }

function TATListbox.GetVisibleItems: integer;
begin
  Result:= ClientHeight div FItemHeight;
end;

function TATListbox.IsIndexValid(AValue: integer): boolean;
begin
  Result:= (AValue>=0) and (AValue<ItemCount);
end;

function TATListbox.GetItemHeightDefault: integer;
begin
  Result:= FTheme^.DoScaleFont(CurrentFontSize) * 18 div 10 + 2;
  Result:= Result * Screen.PixelsPerInch div 96;
end;

procedure TATListbox.UpdateItemHeight;
begin
  if not FItemHeightIsFixed then
    FItemHeight:= GetItemHeightDefault * FItemHeightPercents div 100;
end;

procedure TATListbox.ChangedSelection;
begin
  if Assigned(FOnChangeSel) then
    FOnChangeSel(Self);
end;

procedure TATListbox.Scrolled;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATListbox.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  //must select item under mouse cursor
  ItemIndex:= GetItemIndexAt(MousePos);

  inherited;
end;

function TATListbox.GetMaxWidth(C: TCanvas): integer;
var
  i: integer;
begin
  Result:= 0;

  if FVirtualMode then
  begin
    if Assigned(FOnCalcScrollWidth) then
      Result:= FOnCalcScrollWidth(Self, C);
    exit;
  end;

  for i:= 0 to ItemCount-1 do
    Result:= Max(Result, C.TextWidth(Items[i]));
  Inc(Result, FIndentLeft+2);
end;

procedure TATListbox.UpdateScrollbars(C: TCanvas);
var
  NeedVertBar, NeedHorzBar: boolean;
  si: TScrollInfo;
begin
  if FScrollStyleHorz in [alssShow, alssAuto] then
    FMaxWidth:= GetMaxWidth(C)
  else
    FMaxWidth:= 10;

  case FScrollStyleVert of
    alssAuto:
      NeedVertBar:= ItemCount*ItemHeight>Height; //not ClientHeight
    alssShow:
      NeedVertBar:= true;
    alssHide:
      NeedVertBar:= false;
  end;

  case FScrollStyleHorz of
    alssAuto:
      NeedHorzBar:= FMaxWidth>Width; //not ClientWidth
    alssShow:
      NeedHorzBar:= true;
    alssHide:
      NeedHorzBar:= false;
  end;

  FScrollbar.Visible:=     FThemedScrollbar and NeedVertBar;
  FScrollbarHorz.Visible:= FThemedScrollbar and NeedHorzBar;
  ShowOsBarVert:= not FThemedScrollbar and NeedVertBar;
  ShowOsBarHorz:= not FThemedScrollbar and NeedHorzBar;

  if FThemedScrollbar then
  begin
    if FScrollbar.Visible then
    begin
      FScrollbar.Min:= 0;
      FScrollbar.Max:= ItemCount;
      FScrollbar.PageSize:= VisibleItems;
      FScrollbar.Position:= ItemTop;
      FScrollbar.Update;
    end;

    if FScrollbarHorz.Visible then
    begin
      if FScrollbar.Visible then
        FScrollbarHorz.IndentCorner:= 100
      else
        FScrollbarHorz.IndentCorner:= 0;

      FScrollbarHorz.Min:= 0;
      FScrollbarHorz.Max:= FMaxWidth;
      FScrollbarHorz.PageSize:= ClientWidth;
      FScrollbarHorz.Position:= ScrollHorz;
      FScrollbarHorz.Update;
    end;
  end
  else
  begin
    FillChar(si{%H-}, SizeOf(si), 0);
    si.cbSize:= SizeOf(si);
    si.fMask:= SIF_ALL or SIF_DISABLENOSCROLL;
    si.nMin:= 0;

    if ShowOsBarVert then
    begin
      si.nMax:= ItemCount;
      si.nPage:= GetVisibleItems;
      si.nPos:= FItemTop;
      SetScrollInfo(Handle, SB_VERT, si, True);
    end;

    if ShowOsBarHorz then
    begin
      si.nMax:= FMaxWidth;
      si.nPage:= ClientWidth;
      si.nPos:= ScrollHorz;
      SetScrollInfo(Handle, SB_HORZ, si, True);
    end;
  end;
end;

function TATListbox.ItemCount: integer;
begin
  if FVirtualMode then
    Result:= FVirtualItemCount
  else
    Result:= Items.Count;
end;


procedure TATListbox.DoPaintTo(C: TCanvas; r: TRect);
var
  Index: integer;
  bPaintX, bCircle: boolean;
  RectX: TRect;
begin
  C.Font.Name:= CurrentFontName;
  C.Font.Size:= FTheme^.DoScaleFont(CurrentFontSize);

  C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListbox);
  C.FillRect(r);

  FIndentForX:= 0;
  if FShowX<>albsxNone then
    Inc(FIndentForX, FTheme^.DoScale(FTheme^.XMarkWidth));

  for Index:= FItemTop to ItemCount-1 do
  begin
    r.Top:= (Index-FItemTop)*FItemHeight;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= ClientWidth;
    if r.Top>=ClientHeight then Break;

    if FOwnerDrawn then
    begin
      if Assigned(FOnDrawItem) then
        FOnDrawItem(Self, C, Index, r);
    end
    else
    begin
      DoDefaultDrawItem(C, Index, r);
    end;

    bCircle:=
      (Index>=0) and (Index<FList.Count) and
      (FList.Objects[Index] is TATListboxItemProp) and
      TATListboxItemProp(FList.Objects[Index]).Modified;

    case FShowX of
      albsxNone:
        bPaintX:= false;
      albsxAllItems:
        bPaintX:= true;
      albsxHotItem:
        bPaintX:= bCircle or (FHotTrack and (Index=FHotTrackIndex));
    end;

    if bPaintX then
    begin
      RectX:= Rect(r.Left, r.Top, r.Left+FIndentForX, r.Bottom);
      DoPaintX(C, RectX, bCircle and (Index<>FHotTrackIndex));
    end;
  end;
end;

procedure TATListbox.DoPaintX(C: TCanvas; const R: TRect; ACircle: boolean);
var
  P: TPoint;
  NColor: TColor;
begin
  NColor:= FTheme^.ColorArrows;
  if FHotTrack then
  begin
    P:= ScreenToClient(Mouse.CursorPos);
    if PtInRect(R, P) then
      NColor:= FTheme^.ColorArrowsOver;
  end;

  if ACircle then
    CanvasPaintCircleMark(C, R, NColor,
      FTheme^.DoScale(FTheme^.XMarkOffsetLeft),
      FTheme^.DoScale(FTheme^.XMarkOffsetRight)
    )
  else
    CanvasPaintXMark(C, R, NColor,
      FTheme^.DoScale(FTheme^.XMarkOffsetLeft),
      FTheme^.DoScale(FTheme^.XMarkOffsetRight),
      FTheme^.DoScale(FTheme^.XMarkLineWidth)
      );
end;

function TATListbox.GetOnDrawScrollbar: TATScrollbarDrawEvent;
begin
  Result:= FScrollbar.OnOwnerDraw;
end;

function TATListbox.GetColumnWidth(AIndex: integer): integer;
begin
  if (AIndex>=0) and (AIndex<Length(FColumnSizes)) then
    Result:= FColumnWidths[AIndex]
  else
    Result:= 0;
end;

procedure TATListbox.UpdateColumnWidths;
var
  NTotalWidth, NAutoSized, NSize, NFixedSize, i: integer;
begin
  NTotalWidth:= ClientWidth;
  NAutoSized:= 0;
  NFixedSize:= 0;

  SetLength(FColumnWidths, Length(FColumnSizes));

  //set width of fixed columns
  for i:= 0 to Length(FColumnSizes)-1 do
  begin
    NSize:= FColumnSizes[i];

    //auto-sized?
    if NSize=0 then
      Inc(NAutoSized)
    else
    //in percents?
    if NSize<0 then
      NSize:= NTotalWidth * -NSize div 100;

    Inc(NFixedSize, NSize);
    FColumnWidths[i]:= NSize;
  end;

  //set width of auto-sized columns
  for i:= 0 to Length(FColumnSizes)-1 do
  begin
    if FColumnSizes[i]=0 then
      FColumnWidths[i]:= Max(0, NTotalWidth-NFixedSize) div NAutoSized;
  end;
end;

procedure TATListbox.SetShowOsBarVert(AValue: boolean);
begin
  if FShowOsBarVert=AValue then Exit;
  FShowOsBarVert:= AValue;
  ShowScrollBar(Handle, SB_Vert, AValue);
end;

procedure TATListbox.SetShowOsBarHorz(AValue: boolean);
begin
  if FShowOsBarHorz=AValue then Exit;
  FShowOsBarHorz:= AValue;
  ShowScrollBar(Handle, SB_Horz, AValue);
end;

procedure TATListbox.DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
var
  Sep: TATStringSeparator;
  SLine, SItem: string;
  NIndentLeft,
  NColOffset, NColWidth, NAllWidth, i: integer;
begin
  if AIndex=FItemIndex then
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListboxSel);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListboxSel);
  end
  else
  if FHotTrack and (AIndex=FHotTrackIndex) then
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListboxHottrack);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListbox);
  end
  else
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListbox);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListbox);
  end;
  C.FillRect(R);

  if (AIndex>=0) and (AIndex<FList.Count) then
    SLine:= FList[AIndex]
  else
    SLine:= '('+IntToStr(AIndex)+')';

  NIndentLeft:= FIndentLeft+FIndentForX;

  if Length(FColumnSizes)=0 then
  begin
    C.TextOut(
      R.Left+NIndentLeft-ScrollHorz,
      R.Top+FIndentTop,
      SLine);
  end
  else
  begin
    NAllWidth:= ClientWidth;
    NColOffset:= R.Left+FIndentLeft-ScrollHorz;
    C.Pen.Color:= Theme^.ColorSeparators;
    Sep.Init(SLine, FColumnSep);

    for i:= 0 to Length(FColumnSizes)-1 do
    begin
      NColWidth:= FColumnWidths[i];
      Sep.GetItemStr(SItem);

      C.FillRect(
        Rect(NColOffset,
        R.Top,
        NAllWidth,
        R.Bottom)
        );
      C.TextOut(
        NColOffset+1+IfThen(i=0, NIndentLeft),
        R.Top+FIndentTop,
        SItem
        );

      Inc(NColOffset, NColWidth);
      {$ifdef FPC}
      C.Line(NColOffset-1, R.Top, NColOffset-1, R.Bottom);
      {$else}
      C.MoveTo (NColOffset-1, R.Top);
      C.LineTo (NColOffset-1, R.Bottom);
      {$endif}
    end;
  end;
end;

procedure TATListbox.Paint;
var
  R: TRect;
  C: TCanvas;
begin
  inherited;

  if DoubleBuffered then
    C:= FBitmap.Canvas
  else
    C:= Canvas;

  UpdateItemHeight;
  UpdateScrollbars(C);
  UpdateColumnWidths;

  R:= Rect(0, 0, ClientWidth, ClientHeight);
  if DoubleBuffered then
  begin
    DoPaintTo(C, R);
    Canvas.CopyRect(R, C, R);
  end
  else
    DoPaintTo(C, R);
end;

procedure TATListbox.Click;
var
  Pnt: TPoint;
begin
  if FCanGetFocus then
    {$ifdef FPC}
    LCLIntf.SetFocus(Handle);
    {$else}
    SetFocus;
    {$endif}

  Pnt:= ScreenToClient(Mouse.CursorPos);

  if FShowX<>albsxNone then
    if Pnt.X<=FIndentForX then
      if Assigned(FOnClickX) then
      begin
        FOnClickX(Self);
        exit;
      end;

  inherited; //OnClick must be after ItemIndex set
end;

procedure _BitmapResize(b: Graphics.TBitmap; X, Y: integer); inline;
begin
  {$ifdef fpc}
  b.SetSize(X, Y);
  b.FreeImage; //recommended, else seen black bitmap on bigsize
  {$else}
  b.Width:= X;
  b.Height:= Y;
  {$endif}
end;

procedure TATListbox.Resize;
const
  cStep = 200; //resize bitmap by N pixels step
var
  SizeX, SizeY: integer;
begin
  inherited;

  //ATSynEdit has the same code
  if DoubleBuffered then
    if Assigned(FBitmap) then
    begin
      SizeX:= (Width div cStep + 1)*cStep;
      SizeY:= (Height div cStep + 1)*cStep;
      if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
        _BitmapResize(FBitmap, SizeX, SizeY);
    end;

  Invalidate;
end;

function TATListbox.GetItemIndexAt(Pnt: TPoint): integer;
begin
  Result:= -1;
  if ItemCount=0 then exit;

  if (Pnt.X>=0) and (Pnt.X<ClientWidth) then
  begin
    Result:= Pnt.Y div FItemHeight + FItemTop;
    if Result>=ItemCount then
      Result:= -1;
  end;
end;

function TATListbox.ItemBottom: integer;
begin
  Result:= Min(ItemCount-1, FItemTop+GetVisibleItems-1);
end;

procedure TATListbox.ScrollbarChange(Sender: TObject);
begin
  ItemTop:= FScrollbar.Position;
end;

procedure TATListbox.ScrollbarHorzChange(Sender: TObject);
begin
  ScrollHorz:= FScrollbarHorz.Position;
end;

procedure TATListbox.SetCanBeFocused(AValue: boolean);
begin
  if FCanGetFocus=AValue then Exit;
  FCanGetFocus:= AValue;
  {$ifdef FPC}
  if AValue then
    ControlStyle:= ControlStyle-[csNoFocus]
  else
    ControlStyle:= ControlStyle+[csNoFocus];
  {$endif}
end;

procedure TATListbox.SetItemHeightPercents(AValue: integer);
begin
  if FItemHeightPercents=AValue then Exit;
  FItemHeightPercents:= AValue;
  FItemHeightIsFixed:= false;
end;

procedure TATListbox.SetOnDrawScrollbar(AValue: TATScrollbarDrawEvent);
begin
  FScrollbar.OnOwnerDraw:= AValue;
end;

procedure TATListbox.SetScrollHorz(AValue: integer);
begin
  if FScrollHorz=AValue then Exit;
  FScrollHorz:= AValue;
  FScrollbarHorz.Position:= AValue;
  Invalidate;
end;

procedure TATListbox.SetVirtualItemCount(AValue: integer);
begin
  if FVirtualItemCount=AValue then Exit;
  if AValue<0 then Exit;
  FVirtualItemCount:= AValue;
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemIndex(AValue: integer);
begin
  if FItemIndex=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemIndex:= AValue;

  UpdateItemHeight; //needed, ItemHeight may be not calculated yet

  //scroll if needed
  if FItemIndex=0 then
    FItemTop:= 0
  else
  if FItemIndex<FItemTop then
    FItemTop:= FItemIndex
  else
  if FItemIndex>ItemBottom then
    FItemTop:= Max(0, FItemIndex-GetVisibleItems+1);

  ChangedSelection;
  Invalidate;
end;

procedure TATListbox.SetItemTop(AValue: integer);
begin
  if FItemTop=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemTop:= Max(0, AValue);
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemHeight(AValue: integer);
begin
  if AValue=FItemHeight then exit;
  FItemHeight:= AValue;
  FItemHeightIsFixed:= true;
end;

procedure TATListbox.SetThemedScrollbar(AValue: boolean);
begin
  if FThemedScrollbar=AValue then Exit;
  FThemedScrollbar:= AValue;
  Invalidate;
end;


constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle+[csOpaque] {$ifdef FPC}-[csTripleClicks]{$endif};
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 180;
  Height:= 150;
  Font.Size:= 9;

  CanGetFocus:= false;
  FList:= TStringList.Create;
  FVirtualItemCount:= 0;
  FItemIndex:= 0;
  FItemHeightPercents:= 100;
  FItemHeight:= 17;
  FItemTop:= 0;
  FScrollStyleVert:= alssShow;
  FScrollStyleHorz:= alssAuto;
  FScrollHorz:= 0;
  FIndentLeft:= 4;
  FIndentTop:= 2;
  FOwnerDrawn:= false;
  FVirtualMode:= true;
  FHotTrack:= false;
  FColumnSep:= #9;
  SetLength(FColumnSizes, 0);
  SetLength(FColumnWidths, 0);
  FShowX:= albsxNone;

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.SetSize(800, 600);

  FTheme:= @ATFlatTheme;
  FThemedScrollbar:= true;
  FThemedFont:= true;

  FScrollbar:= TATScrollbar.Create(Self);
  FScrollbar.Parent:= Self;
  FScrollbar.Kind:= sbVertical;
  FScrollbar.Align:= alRight;
  FScrollbar.OnChange:= ScrollbarChange;

  FScrollbarHorz:= TATScrollbar.Create(Self);
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.IndentCorner:= 100;
  FScrollbarHorz.OnChange:= ScrollbarHorzChange;
end;

destructor TATListbox.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATListbox.UpdateFromScrollbarMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMVScroll{$endif});
var
  NMax: integer;
begin
  NMax:= Max(0, ItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:        FItemTop:= 0;
    SB_BOTTOM:     FItemTop:= NMax;

    SB_LINEUP:     FItemTop:= Max(0, FItemTop-1);
    SB_LINEDOWN:   FItemTop:= Min(NMax, FItemTop+1);

    SB_PAGEUP:     FItemTop:= Max(0, FItemTop-GetVisibleItems);
    SB_PAGEDOWN:   FItemTop:= Min(NMax, FItemTop+GetVisibleItems);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FItemTop:= Max(0, Msg.Pos);
  end;
end;

procedure TATListbox.UpdateFromScrollbarHorzMsg(const Msg: {$ifdef FPC}TLMScroll{$else}TWMHScroll{$endif});
var
  NMax: integer;
begin
  NMax:= Max(0, FMaxWidth-ClientWidth);

  case Msg.ScrollCode of
    SB_TOP:        FScrollHorz:= 0;
    SB_BOTTOM:     FScrollHorz:= NMax;

    SB_LINEUP:     FScrollHorz:= Max(0, FScrollHorz-1);
    SB_LINEDOWN:   FScrollHorz:= Min(NMax, FScrollHorz+1);

    SB_PAGEUP:     FScrollHorz:= Max(0, FScrollHorz-ClientWidth);
    SB_PAGEDOWN:   FScrollHorz:= Min(NMax, FScrollHorz+ClientWidth);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FScrollHorz:= Max(0, Msg.Pos);
  end;
end;

{$ifndef FPC}
procedure TATListbox.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TATListbox.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TATListbox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
  if Assigned(FScrollbar) then
  if not MouseInClient then
    FScrollbar.Refresh;
end;
{$endif}

{$ifdef FPC}
procedure TATListbox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

procedure TATListbox.LMHScroll(var Msg: TLMHScroll);
begin
  UpdateFromScrollbarHorzMsg(Msg);
  Invalidate;
end;
{$endif}

{$ifndef FPC}
procedure TATListbox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if (csCreating in ControlState) then exit;
  Invalidate;
end;

procedure TATListbox.WMVScroll(var Msg: TWMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

procedure TATListbox.WMHScroll(var Msg: TWMHScroll);
begin
  UpdateFromScrollbarHorzMsg(Msg);
  Invalidate;
end;

procedure TATListbox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
 inherited;
 Message.Result:= Message.Result or DLGC_WANTARROWS;
end;

procedure TATListbox.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin

 { Check the ShiftState, like delphi does while processing WMKeyDown }
 ShiftState := KeyDataToShiftState(Message.KeyData);
 DoKeyDown(Message.CharCode,ShiftState);

 inherited;

end;

{$endif}

{$ifdef FPC}
function TATListbox.CanFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

{$ifdef FPC}
function TATListbox.CanSetFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

function TATListbox.ClientHeight: integer;
begin
  Result:= inherited ClientHeight;
  if FScrollbarHorz.Visible then
    Dec(Result, FScrollbarHorz.Height);
end;

function TATListbox.ClientWidth: integer;
begin
  Result:= inherited ClientWidth;
  if FScrollbar.Visible then
    Dec(Result, FScrollbar.Width);
end;

function TATListbox.CurrentFontName: string;
begin
  if FThemedFont then
    Result:= FTheme^.FontName
  else
    Result:= Font.Name;
end;

function TATListbox.CurrentFontSize: integer;
begin
  if FThemedFont then
    Result:= FTheme^.FontSize
  else
    Result:= Font.Size;
end;

procedure TATListbox.Invalidate;
var
  R: TRect;
begin
  {$ifdef FPC}
  inherited Invalidate;
  {$else}
  // https://github.com/Alexey-T/ATFlatControls/issues/32
  if (Assigned(FScrollbar) and FScrollbar.Visible) or
  (Assigned(FScrollbarHorz) and FScrollbarHorz.Visible) then
  begin
    R:= Rect(0, 0, ClientWidth, ClientHeight);
    InvalidateRect(Handle, R, false);
  end
  else
    inherited Invalidate;
  {$endif}
end;

function TATListbox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if not ThemedScrollbar then
  begin
    Result:= inherited;
    exit
  end;

  Result:= true;
  if WheelDelta>0 then
    ItemTop:= Max(0, ItemTop-Mouse.WheelScrollLines)
  else
    ItemTop:= Max(0, Min(ItemCount-VisibleItems, ItemTop+Mouse.WheelScrollLines));
end;

procedure TATListbox.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (key=vk_up) then
  begin
    ItemIndex:= ItemIndex-1;
    key:= 0;
    Exit
  end;
  if (key=vk_down) then
  begin
    ItemIndex:= ItemIndex+1;
    key:= 0;
    Exit
  end;

  if (key=vk_prior) then
  begin
    ItemIndex:= Max(0, ItemIndex-(VisibleItems-1));
    key:= 0;
    Exit
  end;
  if (key=vk_next) then
  begin
    ItemIndex:= Min(ItemCount-1, ItemIndex+(VisibleItems-1));
    key:= 0;
    Exit
  end;

  if (key=vk_home) then
  begin
    ItemIndex:= 0;
    key:= 0;
    Exit
  end;
  if (key=vk_end) then
  begin
    ItemIndex:= ItemCount-1;
    key:= 0;
    Exit
  end;

  if (key=vk_return) then
  begin
    DblClick;
    key:= 0;
    Exit
  end;
end;

procedure TATListbox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  DoKeyDown(Key,Shift);
end;

procedure TATListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewIndex: integer;
begin
  inherited;

  if FHotTrack then
  begin
    NewIndex:= GetItemIndexAt(Point(X, Y));
    if (FHotTrackIndex<>NewIndex) or (FShowX<>albsxNone) then
    begin
      FHotTrackIndex:= NewIndex;
      Invalidate;
    end;
  end
  else
    FHotTrackIndex:= -1;
end;

procedure TATListbox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  ItemIndex:= GetItemIndexAt(Point(X, Y));
  inherited;
end;

{$ifdef fpc}
procedure TATListbox.MouseLeave;
begin
  inherited;
  if FHotTrack then
  begin
    FHotTrackIndex:= -1;
    Invalidate;
  end;
end;
{$endif}

initialization

end.

