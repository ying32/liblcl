//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uControlPatchs;

{$I ExtDecl.inc}
{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils,
{$I UseAll.inc};


type
  // 重定定义
  TUnixDateTime = type Int64; // LongInt Int64 改为LongInt。只精确到秒
  TPngImage = type TPortableNetworkGraphic;

  TJPEGPixelFormat = TPixelFormat;

  // 一些关于库的信息
  TStringEncoding = (seUnknown, seANSI, seUnicode, seUTF8);

  NaturalNumber = Integer;

  //TMargins = TControlBorderSpacing;

{$ifndef windows}
  PFNLVCOMPARE = Pointer;
  PFNTVCOMPARE = Pointer;
{$endif}



{$IFDEF WINDOWS}
 type
   TTrayIcon = class(ExtCtrls.TTrayIcon)
   public
     constructor Create(TheOwner: TComponent); override;
   end;

{$ENDIF}

type

  { TListView }

  TListView = class(ComCtrls.TListView)
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteSelected;
    function CustomSort(ASortProc: PFNLVCOMPARE; AOptionalParam: PtrInt): Boolean;
  end;

  { TTreeView }

  TTreeView = class(ComCtrls.TTreeView)
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteSelected;
    function CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt; ARecurse: LongBool): Boolean;
    function AlphaSort(ARecurse: Boolean): Boolean;
  end;

  { TColorBoxHelper }

  TColorBoxHelper = class helper for TColorBox
  public
    procedure DeleteSelected;
  end;

  { TTreeNodesHelper }

  TTreeNodesHelper = class helper for TTreeNodes
  public
    function CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt; ARecurse: Boolean): Boolean;
  end;


  { TTreeNodeHelper }

  TTreeNodeHelper = class helper for TTreeNode
  public
    function CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt; ARecurse: Boolean): Boolean;
  end;


  { TComboBoxHelper }

  TComboBoxHelper = class helper for TComboBox
  public
    procedure DeleteSelected;
  end;


  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);

  { TListHelper }

  TListHelper = class helper for TList
  public
    procedure Assign(ListA: TList; AOperator: TListAssignOp = laCopy; ListB: TList = nil);
  end;

  { TMonthCalendar }

  TMonthCalendar = class(TCalendar)
  private
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  public
    property Date: TDateTime read GetDate write SetDate;

    property DragCursor;
    property DragKind;
    property DragMode;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
  end;

  { TDragObjectHelper }

  TDragObjectHelper = class helper for TDragObject
  private
    function GetDragTarget: Pointer;
    procedure SetDragTarget(AValue: Pointer);
  public
    procedure Assign(Source: TDragObject);
    property DragTarget: Pointer read GetDragTarget write SetDragTarget;
  end;

  { TStatusPanelsHelper }

  TStatusPanelsHelper = class helper for TStatusPanels
  private
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { TApplicationHelper }

  TApplicationHelper = class helper for TApplication
  public class var
    // 定义运行过程指针
    FRunLoopReceivedPtr: Pointer;
  private
    procedure MyRunLoop;
  public
    procedure RestoreTopMosts;
    procedure MyRun;
    procedure SetRunLoopReceived(AProc: Pointer);
  end;


  { TFrame }

  //TFrame = class(Forms.TFrame)
  //private
  //  FOnDestroy: TNotifyEvent;
  //public
  //  constructor Create(TheOwner: TComponent); override;
  //  procedure BeforeDestruction; override;
  //public
  //  property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  //end;



  function ToUnixTime(ADateTime: TDateTime): TUnixDateTime; inline;
  function UnixToTime(ADateTime: TUnixDateTime): TDateTime; inline;
  function ToPChar(AStr: string): PChar; inline;

  procedure Application_SetRunLoopReceived(AObj: TApplication; AProc: Pointer); extdecl;

implementation



function ToUnixTime(ADateTime: TDateTime): TUnixDateTime; inline;
begin
  Result := DateTimeToUnix(LocalTimeToUniversal(ADateTime));
end;

function UnixToTime(ADateTime: TUnixDateTime): TDateTime; inline;
begin
  Result := UniversalTimeToLocal(UnixToDateTime(ADateTime));
end;

function ToPChar(AStr: string): PChar; inline;
begin
  Result := PChar(AStr);
end;

{ TFrame }

//constructor TFrame.Create(TheOwner: TComponent);
//begin
//  try
//    inherited Create(TheOwner);
//  except
//  end;
//end;
//
//procedure TFrame.BeforeDestruction;
//begin
//  if Assigned(FOnDestroy) then
//    FOnDestroy(Self);
//  inherited BeforeDestruction;
//end;

{ TMonthCalendar }

function TMonthCalendar.GetDate: TDateTime;
begin
  Result := DateTime;
end;

procedure TMonthCalendar.SetDate(AValue: TDateTime);
begin
  DateTime := AValue;
end;



{ TApplicationHelper }

//CLASSMETHOD:
procedure Application_SetRunLoopReceived(AObj: TApplication; AProc: Pointer); extdecl;
begin
  AObj.SetRunLoopReceived(AProc);
end;

procedure TApplicationHelper.RestoreTopMosts;
begin
  Self.RestoreStayOnTop(False);
end;

procedure TApplicationHelper.MyRunLoop;
type TRunLoopReceivedProc = function(data: Pointer): Pointer; extdecl; // 实际返回值未使用，主要是为了兼容些
begin
  repeat
    if TRunLoopReceivedProc(FRunLoopReceivedPtr)(Self) = nil then
    begin
      if CaptureExceptions then
        try
          HandleMessage;
        except
          HandleException(Self);
        end
      else
        HandleMessage;
    end;
  until Terminated;
end;

procedure TApplicationHelper.MyRun;
begin
  if Assigned(FRunLoopReceivedPtr) then
  begin
    if (MainForm <> nil) and ShowMainForm then
      MainForm.Show;
    WidgetSet.AppRun(@MyRunLoop);
  end else
    Run();
end;

procedure TApplicationHelper.SetRunLoopReceived(AProc: Pointer);
begin
  FRunLoopReceivedPtr := AProc;
end;

{ TStatusPanelsHelper }

function TStatusPanelsHelper.GetCapacity: Integer;
begin
  Result := 0;
end;

procedure TStatusPanelsHelper.SetCapacity(AValue: Integer);
begin

end;

{ TDragObjectHelper }

function TDragObjectHelper.GetDragTarget: Pointer;
begin
  Result := inherited DragTarget;
end;

procedure TDragObjectHelper.SetDragTarget(AValue: Pointer);
begin
  inherited DragTarget := TControl(AValue);
end;

procedure TDragObjectHelper.Assign(Source: TDragObject);
begin
  // no support
end;



{ TListHelper }

procedure TListHelper.Assign(ListA: TList; AOperator: TListAssignOp;
  ListB: TList);
begin
  // no support
end;

{ TComboBoxHelper }

procedure TComboBoxHelper.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;

{ TTreeNodeHelper }

function TTreeNodeHelper.CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt;
  ARecurse: Boolean): Boolean;
begin
  Result := False;
end;

{ TTreeNodesHelper }

function TTreeNodesHelper.CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt;
  ARecurse: Boolean): Boolean;
begin
  Result := False;
end;

{ TColorBoxHelper }

procedure TColorBoxHelper.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;

{$IFDEF WINDOWS}
constructor TTrayIcon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if not Application.Icon.Empty then
    Icon.Assign(Application.Icon);
end;

{$ENDIF}

{ TTreeView }

constructor TTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.ScrollBars := TScrollStyle.ssAutoBoth;
//  Self.ExpandSignType := tvestArrowFill;
//{$IFNDEF WINDOWS}
//  Self.TreeLinePenStyle := TPenStyle.psSolid;
//{$ENDIF}
end;

procedure TTreeView.DeleteSelected;
var
  LNode: TTreeNode;
begin
  LNode := Selected;
  if LNode <> nil then
     LNode.Delete;
end;

function TTreeView.CustomSort(SortProc: PFNTVCOMPARE; Data: NativeInt;
  ARecurse: LongBool): Boolean;
begin
  // no support
  Result := inherited CustomSort(nil);
end;

function TTreeView.AlphaSort(ARecurse: Boolean): Boolean;
begin
  Result := inherited AlphaSort();
end;


{ TListView }

constructor TListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.ScrollBars := TScrollStyle.ssAutoBoth;
end;

procedure TListView.DeleteSelected;
begin
  if ItemIndex <> -1 then
    Items.Delete(ItemIndex);
end;

// 兼容版，因为内部Lazarus2.0起后有个兼容版本，但实际使用中并不兼容。
// TLVCompare = function(Item1, Item2: TListItem; AOptionalParam: PtrInt): Integer stdcall;
function DefaultListViewSortProc(Item1, Item2: TListItem; AOptionalParam: PtrInt): Integer stdcall;
begin
  with Item1 do
    if Assigned(TListView(ListView).OnCompare) then
      TListView(ListView).OnCompare(ListView, Item1, Item2, AOptionalParam, Result)
    else Result := CompareText(Item1.Caption, Item2.Caption);
end;



function TListView.CustomSort(ASortProc: PFNLVCOMPARE; AOptionalParam: PtrInt): Boolean;
begin
  //if not Assigned(ASortProc) then ASortProc := @DefaultListViewSortProc;
  Result := inherited CustomSort(@DefaultListViewSortProc, AOptionalParam);
end;

end.

