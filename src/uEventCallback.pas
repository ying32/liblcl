//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uEventCallback;

{$mode objfpc}{$H+}

{$I ExtDecl.inc}

interface

uses
  Controls,
  Forms,
  ComCtrls,
  Menus,
  ExtCtrls,
  Classes,
  SysUtils,
  Graphics,
  StdCtrls,
  LCLType,
  LMessages,
  Grids,
  uLinkLabel,
  fgl;

const
  // call最长参数数，与导出的MySyscall一致，暂定为12个
  CALL_MAX_PARAM = 12;

type
  // 基础事件回调函数
  TEventCallbackPtr = function(f: Pointer; args: Pointer; argcout: NativeInt): Pointer; extdecl;
  // 清除事件回调函数
  TRemoveEventCallPtr = function(f: Pointer): Pointer; extdecl;
  // 特殊的消息事件回调
  TMessageCallbackPtr = function(f: Pointer; msg: Pointer): Pointer; extdecl;
  // 线程同步回调
  TThreadSyncCallbackPtr = function: Pointer; extdecl;

var
  GEventCallbackPtr: TEventCallbackPtr;
  GRemoveEventCallbackPtr: TRemoveEventCallPtr;
  GMessageCallbackPtr: TMessageCallbackPtr;
  GThreadSyncCallbackPtr: TThreadSyncCallbackPtr;


type

  // 新的方式，不再使用id+map来查找，这样效率更高，且不会出错

  TEventObjectList = specialize TFPGMapObject<Pointer, TObject>;

  { TLCLEventBase }

  TLCLEventBase = class
  private class var
    FEventObjectsLock: TRtlCriticalSection;
    FEventObjects: TEventObjectList;
    // thread sync
    FThreadEvtId: NativeUInt;
  public
    class constructor Create;
    class destructor Destroy;
  private
    // 保存传入的，一个标识吧
    FHostDataPtr: Pointer;
    function GetDataPtr: Pointer;
    // 不单独使用的，配置其他使用
    class procedure CallRemoveEvent(AMethod: TMethod);
  protected
    procedure SendEvent(AArgs: array of const);
    function CheckDataPtr: Boolean;
  public
    constructor Create(AHostDataPtr: Pointer);
    destructor Destroy; override;
    property DataPtr: Pointer read GetDataPtr;

    // 移除事件中的对象
    class procedure Remove(AMethod: TMethod);
    class function CheckAndUpdate(AMethod: TMethod; AHostDataPtr: Pointer): Boolean;
  end;

  { TLCLEvent }

  TLCLEvent = class(TLCLEventBase)
  public
    //------------------新方式-------------------------------
    procedure OnTExceptionEvent(Sender: TObject; E: Exception);

    procedure OnTNotifyEvent(Sender: TObject);

    function OnTHelpEvent(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    procedure OnTShortCutEvent(var Msg: TLMKey; var Handled: Boolean);
    procedure OnTAlignPositionEvent(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
    procedure OnTCloseEvent(Sender: TObject; var Action: TCloseAction);
    procedure OnTCloseQueryEvent(Sender: TObject; var CanClose: Boolean);
    procedure OnTContextPopupEvent(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure OnTDockDropEvent(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure OnTDragDropEvent(Sender, Source: TObject; X, Y: Integer);
    procedure OnTDragOverEvent(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnTEndDragEvent(Sender, Target: TObject; X, Y: Integer);
    procedure OnTGetSiteInfoEvent(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure OnTKeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnTKeyPressEvent(Sender: TObject; var Key: Char);
    procedure OnTMouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnTMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnTMouseWheelEvent(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnTMouseWheelUpDownEvent(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnTStartDockEvent(Sender: TObject; var DragObject: TDragDockObject);
    procedure OnTUnDockEvent(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
    procedure OnTMenuChangeEvent(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    procedure OnTDrawItemEvent(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OnTMeasureItemEvent(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure OnTSysLinkEvent(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure OnTUDChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure OnTUDClickEvent(Sender: TObject; Button: TUDBtnType);
    procedure OnTLVAdvancedCustomDrawEvent(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OnTLVAdvancedCustomDrawItemEvent(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OnTLVAdvancedCustomDrawSubItemEvent(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OnTLVChangeEvent(Sender: TObject; AItem: TListItem; Change: TItemChange);
    procedure OnTLVColumnClickEvent(Sender: TObject; Column: TListColumn);
    procedure OnTLVCompareEvent(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure OnTLVCustomDrawEvent(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure OnTLVCustomDrawItemEvent(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure OnTLVCustomDrawSubItemEvent(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure OnTLVDataEvent(Sender: TObject; Item: TListItem);
    procedure OnTLVDataFindEvent(Sender: TObject; Find: TItemFind; const FindString: string; const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean; var Index: Integer);
    procedure OnTLVDataHintEvent(Sender: TObject; StartIndex, EndIndex: Integer);
    procedure OnTLVDeletedEvent(Sender: TObject; Item: TListItem);
    procedure OnTLVEditedEvent(Sender: TObject; Item: TListItem; var S: string);
    procedure OnTLVEditingEvent(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure OnTLVSelectItemEvent(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure OnTLVCheckedItemEvent(Sender: TObject; Item: TListItem);
    procedure OnTLVDrawItemEvent(Sender: TCustomListView; AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);

    procedure OnTTVExpandedEvent(Sender: TObject; Node: TTreeNode);
    procedure OnTTVAdvancedCustomDrawEvent(Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OnTTVAdvancedCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure OnTTVChangedEvent(Sender: TObject; ANode: TTreeNode);
    procedure OnTTVChangingEvent(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure OnTTVCollapsingEvent(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure OnTTVCompareEvent(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);
    procedure OnTTVCustomDrawEvent(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean);
    procedure OnTTVCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure OnTTVEditedEvent(Sender: TObject; Node: TTreeNode; var S: string);
    procedure OnTTVEditingEvent(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure OnTTVExpandingEvent(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure OnTMenuMeasureItemEvent(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure OnTTabChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure OnTWebTitleChangeEvent(Sender: TObject; const Text: string);
    procedure OnTWebJSExternalEvent(Sender: TObject; const Afunc: string; const AArgs: string; var ARetval: string);

    procedure OnTDrawCellEvent(Sender: TObject; ACol, ARow: Longint; ARect: TRect; State: TGridDrawState);
    procedure OnTGetEditEvent(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure OnTSetEditEvent(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure OnTSelectCellEvent(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnTSectionNotifyEvent(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    procedure OnTSectionTrackEvent(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    procedure OnTSectionDragEvent(Sender: TObject; FromSection, ToSection: THeaderSection; var AllowDrag: Boolean);
    procedure OnTTaskDlgClickEvent(Sender: TObject; ModalResult: TModalResult; var CanClose: Boolean);
    procedure OnTCheckGroupClicked(Sender: TObject; Index: integer);

    procedure OnTConstrainedResizeEvent(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
    procedure OnTDropFilesEvent(Sender: TObject; const AFileNames: array of string);


    //TStringGrid
    procedure OnTOnSelectEvent(Sender: TObject; aCol, aRow: Integer);
    procedure OnTToggledCheckboxEvent(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure OnTGridOperationEvent(Sender: TObject; IsColumn:Boolean; sIndex, tIndex: Integer);
    procedure OnTOnCompareCells(Sender: TObject; ACol, ARow, BCol,BRow: Integer; var Result: integer);
    procedure OnTGetCellHintEvent(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure OnTGetCheckboxStateEvent(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure OnTSetCheckboxStateEvent(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    procedure OnTHdrEvent(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure OnTHeaderSizingEvent(sender: TObject; const IsColumn: boolean; const aIndex, aSize: Integer);
    procedure OnTSelectEditorEvent(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure OnTUserCheckboxBitmapEvent(Sender: TObject; const aCol, aRow: Integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
    procedure OnTValidateEntryEvent(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure OnTOnPrepareCanvasEvent(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    //--------------------------------------------------------------------------

    procedure OnTAcceptFileNameEvent(Sender: TObject; var Value: String);
    procedure OnTCheckItemChange(Sender: TObject; AIndex: Integer);

    procedure OnTUTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure OnTMenuDrawItemEvent(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);

    procedure OnTWndProcEvent(Sender: TObject; var TheMessage: TLMessage);
  public
    // thread sync
    class procedure ThreadProc;
    class property ThreadEvtId: NativeUInt read FThreadEvtId write FThreadEvtId;
  end;



implementation




{ TLCLEventBase }

class constructor TLCLEventBase.Create;
begin
  InitCriticalSection(FEventObjectsLock);
  FEventObjects := TEventObjectList.Create();
end;

class destructor TLCLEventBase.Destroy;
begin
  System.EnterCriticalSection(FEventObjectsLock);
  try
    FEventObjects.Clear;
    FreeAndNil(FEventObjects);
  finally
    System.LeaveCriticalSection(FEventObjectsLock);
    DoneCriticalsection(FEventObjectsLock);
  end;
end;

function TLCLEventBase.CheckDataPtr: Boolean;
begin
  Result := FHostDataPtr <> nil;
end;

function TLCLEventBase.GetDataPtr: Pointer;
begin
  Result := FHostDataPtr;
end;

class procedure TLCLEventBase.CallRemoveEvent(AMethod: TMethod);
begin
  // 通知宿主程序，某个事件要清除了，解除引用。
  if Assigned(GRemoveEventCallbackPtr) then
    GRemoveEventCallbackPtr(TLCLEventBase(AMethod.Data).FHostDataPtr);
end;

procedure TLCLEventBase.SendEvent(AArgs: array of const);
var
  LParams: array[0..CALL_MAX_PARAM-1] of Pointer;
  LArgLen: Integer;
  LV: TVarRec;
  I: Integer;
begin
  if Assigned(GEventCallbackPtr) and CheckDataPtr then
  begin
    LArgLen := Length(AArgs);
    if LArgLen <= Length(LParams) then
    begin
      for I := 0 to LArgLen - 1 do
      begin
        LV := AArgs[I];
        case LV.VType of
          vtInteger       : LParams[I] := Pointer(LV.VInteger);
          vtBoolean       : LParams[I] := Pointer(Byte(LV.VBoolean));
          vtChar          : LParams[I] := Pointer(Ord(LV.VChar));
          vtExtended      : LParams[I] := LV.VExtended;

          vtString        : LParams[I] := {$IFDEF MSWINDOWS}LV.VString{$ELSE}LV.VAnsiString{$ENDIF};

          vtPointer       : LParams[I] := LV.VPointer;
          vtPChar         : LParams[I] := LV.VPChar;
          vtObject        : LParams[I] := LV.VObject;
          vtClass         : LParams[I] := LV.VClass;
          vtWideChar      : LParams[I] := Pointer(Ord(LV.VWideChar));
          vtPWideChar     : LParams[I] := LV.VPWideChar;
          vtAnsiString    : LParams[I] := LV.VAnsiString;
//          vtCurrency      = 12;
//          vtVariant       = 13;
          vtInterface     : LParams[I] := LV.VInterface;
          vtWideString    : LParams[I] := LV.VWideString;
          vtInt64         : LParams[I] := LV.VInt64;
          vtUnicodeString : LParams[I] := LV.VUnicodeString;
        end;
      end;

      // 这里要替换为 Pointer
      GEventCallbackPtr(DataPtr, @LParams[0], LArgLen);
    end;
  end;
end;

constructor TLCLEventBase.Create(AHostDataPtr: Pointer);
begin
  inherited Create;
  FHostDataPtr := AHostDataPtr;
  // 并自己添加到对象列表，以便整个程序结束后清理对象
  System.EnterCriticalSection(FEventObjectsLock);
  try
    FEventObjects.AddOrSetData(Pointer(Self), Self);
  finally
    System.LeaveCriticalSection(FEventObjectsLock);
  end;
end;

destructor TLCLEventBase.Destroy;
begin
  inherited Destroy;
end;

class procedure TLCLEventBase.Remove(AMethod: TMethod);
begin
  System.EnterCriticalSection(FEventObjectsLock);
  try
    // 这是一个class，找到后会删除并Free掉这个对象
    if AMethod.Data <> nil then
    begin
      CallRemoveEvent(AMethod);
      FEventObjects.Remove(AMethod.Data);
    end;
  finally
    System.LeaveCriticalSection(FEventObjectsLock);
  end;
end;

class function TLCLEventBase.CheckAndUpdate(AMethod: TMethod; AHostDataPtr: Pointer): Boolean;
begin
  Result := False;
  System.EnterCriticalSection(FEventObjectsLock);
   try
     if AMethod.Data <> nil then
     begin
       // 只有当要替换的数据不同时，则更新
       if TLCLEventBase(AMethod.Data).FHostDataPtr <> AHostDataPtr then
       begin
         CallRemoveEvent(AMethod);
         TLCLEventBase(AMethod.Data).FHostDataPtr := AHostDataPtr;
         Result := True;
       end;
     end;
   finally
     System.LeaveCriticalSection(FEventObjectsLock);
   end;
end;

{ TLCLEvent }

// ----------------- 事件定义 -------------------------------

procedure TLCLEvent.OnTExceptionEvent(Sender: TObject; E: Exception);
begin
  SendEvent([Sender, E]);
end;

procedure TLCLEvent.OnTNotifyEvent(Sender: TObject);
begin
  SendEvent([Sender]);
end;

function TLCLEvent.OnTHelpEvent(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
var
  LResult: Boolean;
begin
  SendEvent([Command, Data, Pointer(@CallHelp), Pointer(@LResult)]);
  Result := LResult;
end;

procedure TLCLEvent.OnTShortCutEvent(var Msg: TLMKey;
  var Handled: Boolean);
begin
  SendEvent([Pointer(@Msg), Pointer(@Handled)]);
end;

procedure TLCLEvent.OnTAlignPositionEvent(Sender: TWinControl;
  Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  SendEvent([Sender, Control, @NewLeft, @NewTop, @NewWidth, @NewHeight, @AlignRect, @AlignInfo]);
end;

procedure TLCLEvent.OnTCloseEvent(Sender: TObject;
  var Action: TCloseAction);
begin
  SendEvent([Sender, @Action]);
end;

procedure TLCLEvent.OnTCloseQueryEvent(Sender: TObject;
  var CanClose: Boolean);
begin
  SendEvent([Sender, @CanClose]);
end;

procedure TLCLEvent.OnTContextPopupEvent(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  SendEvent([Sender, Pointer(@MousePos), Pointer(@Handled)]);
end;

procedure TLCLEvent.OnTDockDropEvent(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  SendEvent([Sender, Source, X, Y]);
end;

procedure TLCLEvent.OnTDragDropEvent(Sender, Source: TObject; X,
  Y: Integer);
begin
  SendEvent([Sender, Source, X, Y]);
end;

procedure TLCLEvent.OnTDragOverEvent(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  SendEvent([Sender, Source, X, Y, Integer(State), Pointer(@Accept)]);
end;

procedure TLCLEvent.OnTEndDragEvent(Sender, Target: TObject; X,
  Y: Integer);
begin
  SendEvent([Sender, Target, X, Y]);
end;

procedure TLCLEvent.OnTGetSiteInfoEvent(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  SendEvent([Sender, DockClient, Pointer(@InfluenceRect), Pointer(@MousePos), Pointer(@CanDock)]);
end;

procedure TLCLEvent.OnTKeyEvent(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SendEvent([Sender, @Key, PWord(@Shift)^]);
end;

procedure TLCLEvent.OnTKeyPressEvent(Sender: TObject; var Key: Char);
var
  LKey: Word;
begin
  // 这里要修复下
  LKey := Ord(Key);
  SendEvent([Sender, @LKey]);
  Key := Char(LKey);
end;

procedure TLCLEvent.OnTMouseEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SendEvent([Sender, Ord(Button), PWord(@Shift)^, X, Y]);
end;

procedure TLCLEvent.OnTMouseMoveEvent(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  SendEvent([Sender, PWord(@Shift)^, X, Y]);
end;

procedure TLCLEvent.OnTMouseWheelEvent(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  SendEvent([Sender, PWord(@Shift)^, WheelDelta, MousePos.X, MousePos.Y, @Handled]);
end;

procedure TLCLEvent.OnTMouseWheelUpDownEvent(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SendEvent([Sender, PWord(@Shift)^, Pointer(@MousePos), Pointer(@Handled)]);
end;

procedure TLCLEvent.OnTStartDockEvent(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  SendEvent([Sender, @DragObject]);
end;

procedure TLCLEvent.OnTUnDockEvent(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  SendEvent([Sender, Client, NewTarget, Pointer(@Allow)]);
end;

procedure TLCLEvent.OnTMenuChangeEvent(Sender: TObject;
  Source: TMenuItem; Rebuild: Boolean);
begin
  SendEvent([Sender, Source, Rebuild]);
end;

procedure TLCLEvent.OnTDrawItemEvent(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  SendEvent([Control, Index, @ARect, PWord(@State)^]);
end;

procedure TLCLEvent.OnTMeasureItemEvent(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  SendEvent([Control, Index, @Height]);
end;

procedure TLCLEvent.OnTSysLinkEvent(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  SendEvent([Sender, PChar(Link), Ord(LinkType)]);
end;

procedure TLCLEvent.OnTUDChangingEvent(Sender: TObject;
  var AllowChange: Boolean);
begin
  SendEvent([Sender, @AllowChange]);
end;

procedure TLCLEvent.OnTUDClickEvent(Sender: TObject; Button: TUDBtnType);
begin
  SendEvent([Sender, Ord(Button)]);
end;

procedure TLCLEvent.OnTLVAdvancedCustomDrawEvent(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendEvent([Sender, @ARect, Ord(Stage), @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVAdvancedCustomDrawItemEvent(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, Item, PWord(@State)^, Ord(Stage), @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVAdvancedCustomDrawSubItemEvent(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, Item, SubItem, PWord(@State)^, Ord(Stage), @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVChangeEvent(Sender: TObject; AItem: TListItem;
  Change: TItemChange);
begin
  SendEvent([Sender, AItem, Ord(Change)]);
end;

procedure TLCLEvent.OnTLVColumnClickEvent(Sender: TObject;
  Column: TListColumn);
begin
  SendEvent([Sender, Column]);
end;

procedure TLCLEvent.OnTLVCompareEvent(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  SendEvent([Sender, Item1, Item2, Data, @Compare]);
end;

procedure TLCLEvent.OnTLVCustomDrawEvent(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, @ARect, @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVCustomDrawItemEvent(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, Item, PWord(@State)^, @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVCustomDrawSubItemEvent(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, Item, SubItem, PWord(@State)^, @DefaultDraw]);
end;

procedure TLCLEvent.OnTLVDataEvent(Sender: TObject; Item: TListItem
  );
begin
  SendEvent([Sender, Item]);
end;

procedure TLCLEvent.OnTLVDataFindEvent(Sender: TObject;
  Find: TItemFind; const FindString: string; const FindPosition: TPoint;
  FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
  Wrap: Boolean; var Index: Integer);
begin
  SendEvent([Sender, Ord(Find), PChar(FindString), @FindPosition, FindData, StartIndex,
    Ord(Direction), Integer(Wrap), @Index]);
end;

procedure TLCLEvent.OnTLVDataHintEvent(Sender: TObject;
  StartIndex, EndIndex: Integer);
begin
  SendEvent([Sender, StartIndex, EndIndex]);
end;

procedure TLCLEvent.OnTLVDeletedEvent(Sender: TObject; Item: TListItem);
begin
  SendEvent([Sender, Item]);
end;

procedure TLCLEvent.OnTLVEditedEvent(Sender: TObject; Item: TListItem;
  var S: string);
var
  LS: PChar;
begin
  LS := PChar(S);
  SendEvent([Sender, Item, @LS]);
  S := LS;
end;

procedure TLCLEvent.OnTLVEditingEvent(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  SendEvent([Sender, Item, @AllowEdit]);
end;

procedure TLCLEvent.OnTLVSelectItemEvent(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  SendEvent([Sender, Item, Selected]);
end;

procedure TLCLEvent.OnTLVCheckedItemEvent(Sender: TObject;
  Item: TListItem);
begin
  SendEvent([Sender, Item]);
end;

procedure TLCLEvent.OnTLVDrawItemEvent(
  Sender: TCustomListView; AItem: TListItem; ARect: TRect;
  AState: TOwnerDrawState);
begin
  SendEvent([Sender, AItem, @ARect, PWord(@AState)^]);
end;

procedure TLCLEvent.OnTTVExpandedEvent(Sender: TObject; Node: TTreeNode
  );
begin
  SendEvent([Sender, Node]);
end;

procedure TLCLEvent.OnTTVAdvancedCustomDrawEvent(
  Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendEvent([Sender, @ARect, Ord(Stage), DefaultDraw]);
end;

procedure TLCLEvent.OnTTVAdvancedCustomDrawItemEvent(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  SendEvent(
    [Sender, Node, PWord(@State)^, Ord(Stage), @PaintImages, DefaultDraw]);
end;

procedure TLCLEvent.OnTTVChangedEvent(Sender: TObject; ANode: TTreeNode
  );
begin
  SendEvent([Sender, ANode]);
end;

procedure TLCLEvent.OnTTVChangingEvent(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  SendEvent([Sender, Node, @AllowChange]);
end;

procedure TLCLEvent.OnTTVCollapsingEvent(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  SendEvent([Sender, Node, @AllowCollapse]);
end;

procedure TLCLEvent.OnTTVCompareEvent(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  SendEvent([Sender, Node1, Node2, 0, @Compare]);
end;

procedure TLCLEvent.OnTTVCustomDrawEvent(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, @ARect, @DefaultDraw]);
end;

procedure TLCLEvent.OnTTVCustomDrawItemEvent(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent([Sender, Node, PWord(@State)^, @DefaultDraw]);
end;

procedure TLCLEvent.OnTTVEditedEvent(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  LS: PChar;
begin
  LS := PChar(S);
  SendEvent([Sender, Node, @LS]);
  S := LS;
end;

procedure TLCLEvent.OnTTVEditingEvent(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  SendEvent([Sender, Node, @AllowEdit]);
end;

procedure TLCLEvent.OnTTVExpandingEvent(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  SendEvent([Sender, Node, @AllowExpansion]);
end;

procedure TLCLEvent.OnTMenuMeasureItemEvent(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  SendEvent([Sender, ACanvas, @Width, @Height]);
end;

procedure TLCLEvent.OnTTabChangingEvent(Sender: TObject;
  var AllowChange: Boolean);
begin
  SendEvent([Sender, @AllowChange]);
end;

procedure TLCLEvent.OnTWebTitleChangeEvent(Sender: TObject;
  const Text: string);
begin
  SendEvent([Sender, PChar(Text)]);
end;

procedure TLCLEvent.OnTWebJSExternalEvent(Sender: TObject;
  const Afunc: string; const AArgs: string; var ARetval: string);
var
  LRet: PChar;
begin
  LRet := PChar(ARetval);
  SendEvent([Sender, PChar(Afunc), PChar(AArgs), @LRet]);
  ARetval := string(LRet);
end;

procedure TLCLEvent.OnTOnCompareCells(Sender: TObject;
  ACol, ARow, BCol, BRow: Integer; var Result: integer);
begin
  SendEvent([Sender, ACol, ARow, BCol, BRow, @Result]);
end;

procedure TLCLEvent.OnTGetCellHintEvent(Sender: TObject;
  ACol, ARow: Integer; var HintText: String);
var
  LVal: PChar;
begin
  LVal := PChar(HintText);
  SendEvent([Sender, aCol, aRow, @LVal]);
  if LVal <> nil then
    HintText := LVal;
end;

procedure TLCLEvent.OnTGetCheckboxStateEvent(
  Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
begin
  SendEvent([Sender, ACol, ARow, @Value]);
end;

procedure TLCLEvent.OnTSetCheckboxStateEvent(
  Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
begin
  SendEvent([Sender, ACol, ARow, PWord(@Value)^]);
end;

procedure TLCLEvent.OnTHdrEvent(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  SendEvent([Sender, IsColumn, Index]);
end;

procedure TLCLEvent.OnTHeaderSizingEvent(
  sender: TObject; const IsColumn: boolean; const aIndex, aSize: Integer);
begin
  SendEvent([Sender, IsColumn, aIndex, aSize]);
end;

procedure TLCLEvent.OnTOnSelectEvent(Sender: TObject; aCol,
  aRow: Integer);
begin
  SendEvent([Sender, aCol, aRow]);
end;

procedure TLCLEvent.OnTSelectEditorEvent(
  Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  SendEvent([Sender, aCol, aRow, @Editor]);
end;

procedure TLCLEvent.OnTUserCheckboxBitmapEvent(
  Sender: TObject; const aCol, aRow: Integer;
  const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  SendEvent([Sender, aCol, aRow, PWord(@CheckedState)^, @ABitmap]);
end;

procedure TLCLEvent.OnTValidateEntryEvent(
  sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  LVal: PChar;
begin
  LVal := PChar(NewValue);
  SendEvent([Sender, aCol, aRow, PChar(OldValue), @LVal]);
  if LVal <> nil then
    NewValue := LVal;
end;

procedure TLCLEvent.OnTOnPrepareCanvasEvent(
  sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  SendEvent([Sender, aCol, aRow, PWord(@aState)^]);
end;

procedure TLCLEvent.OnTAcceptFileNameEvent(Sender: TObject;
  var Value: String);
var
  LS: PChar;
begin
  LS := PChar(Value);
  SendEvent([Sender, Pointer(@LS)]);
  Value := LS;
end;

procedure TLCLEvent.OnTCheckItemChange(Sender: TObject;
  AIndex: Integer);
begin
  SendEvent([Sender, AIndex]);
end;

procedure TLCLEvent.OnTUTF8KeyPressEvent(
  Sender: TObject; var UTF8Key: TUTF8Char);
begin
  SendEvent([Sender, @UTF8Key]);
end;

procedure TLCLEvent.OnTMenuDrawItemEvent(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  SendEvent([Sender, ACanvas, Pointer(@ARect), PWord(@AState)^]);
end;

class procedure TLCLEvent.ThreadProc;
begin
  GThreadSyncCallbackPtr();
end;

procedure TLCLEvent.OnTDrawCellEvent(Sender: TObject; ACol,
  ARow: Longint; ARect: TRect; State: TGridDrawState);
begin
  SendEvent([Sender, ACol, ARow, Pointer(@ARect), PWord(@State)^]);
end;

procedure TLCLEvent.OnTGetEditEvent(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
var
  LS: PChar;
begin
  LS := PChar(Value);
  SendEvent([Sender, ACol, ARow, Pointer(@LS)]);
  Value := LS;
end;

procedure TLCLEvent.OnTSelectCellEvent(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  SendEvent([Sender, ACol, ARow, Pointer(@CanSelect)]);
end;

procedure TLCLEvent.OnTSetEditEvent(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  SendEvent([Sender, ACol, ARow, PChar(Value)]);
end;

procedure TLCLEvent.OnTSectionNotifyEvent(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  SendEvent([HeaderControl, Section]);
end;

procedure TLCLEvent.OnTSectionTrackEvent(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
begin
  SendEvent([HeaderControl, Section, Width, Integer(State)]);
end;

procedure TLCLEvent.OnTSectionDragEvent(Sender: TObject; FromSection,
  ToSection: THeaderSection; var AllowDrag: Boolean);
begin
  SendEvent([Sender, FromSection, ToSection, Pointer(@AllowDrag)]);
end;

procedure TLCLEvent.OnTTaskDlgClickEvent(Sender: TObject;
  ModalResult: TModalResult; var CanClose: Boolean);
begin
  SendEvent([Sender, ModalResult, @CanClose]);
end;

procedure TLCLEvent.OnTCheckGroupClicked(Sender: TObject; Index: integer
  );
begin
  SendEvent([Sender, Index]);
end;

procedure TLCLEvent.OnTConstrainedResizeEvent(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
  SendEvent([Sender, @MinWidth, @MinHeight, @MaxWidth, @MaxHeight]);
end;

procedure TLCLEvent.OnTDropFilesEvent(Sender: TObject; const AFileNames: array of string);
var
  LLen: Integer;
begin
  LLen := Length(AFileNames);
  if LLen > 0 then
    SendEvent([Sender, @AFileNames[0], LLen]);
end;

procedure TLCLEvent.OnTToggledCheckboxEvent(
  sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  SendEvent([Sender, aCol, aRow, PWord(@aState)^]);
end;

procedure TLCLEvent.OnTGridOperationEvent(
  Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SendEvent([Sender, IsColumn, sIndex, tIndex]);
end;

procedure TLCLEvent.OnTWndProcEvent(Sender: TObject;
  var TheMessage: TLMessage);
begin
  if Assigned(GMessageCallbackPtr) and CheckDataPtr then
    GMessageCallbackPtr(DataPtr, @TheMessage);
end;


end.
