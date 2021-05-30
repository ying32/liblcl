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
  LMessages,
  Grids,
  uLinkLabel,
  fgl;

const
  // call最长参数数，与导出的MySyscall一致，暂定为12个
  CALL_MAX_PARAM = 12;

type
  TEventCallbackPtr = function(f: NativeUInt; args: Pointer; argcout: NativeInt): Pointer; extdecl;
  TMessageCallbackPtr = function(f: NativeUInt; msg: Pointer): Pointer; extdecl;
  TThreadSyncCallbackPtr = function: Pointer; extdecl;

var
  GEventCallbackPtr: TEventCallbackPtr;
  GMessageCallbackPtr: TMessageCallbackPtr;
  GThreadSyncCallbackPtr: TThreadSyncCallbackPtr;


type

  TEventKey = packed record
    Sender: TObject;
    Event: Pointer;
  end;
  PEventKey = ^TEventKey;

  { TEventList }

  TEventList = specialize TFPGMap<NativeUInt, NativeUInt>;


  TMainEventList = specialize TFPGMapObject<Pointer, TEventList>;

  { TEventClass }

  TEventClass = class
  private class var
    FMainEvents: TMainEventList;
    FThreadEvtId: NativeUInt;
    class procedure SendEvent(Sender: TObject; AEvent: Pointer; AArgs: array of const);
  public
    class constructor Create;
    class destructor Destroy;
  public
    //------------------新方式-------------------------------
    class procedure OnTExceptionEvent_OnException(Sender: TObject; E: Exception);


    class procedure OnTNotifyEvent_OnActivate(Sender: TObject);
    class procedure OnTNotifyEvent_OnDeactivate(Sender: TObject);
    class procedure OnTNotifyEvent_OnHint(Sender: TObject);
    class procedure OnTNotifyEvent_OnMinimize(Sender: TObject);
    class procedure OnTNotifyEvent_OnRestore(Sender: TObject);
    class procedure OnTNotifyEvent_OnClick(Sender: TObject);
    class procedure OnTNotifyEvent_OnDblClick(Sender: TObject);
    class procedure OnTNotifyEvent_OnHide(Sender: TObject);
    class procedure OnTNotifyEvent_OnMouseEnter(Sender: TObject);
    class procedure OnTNotifyEvent_OnMouseLeave(Sender: TObject);
    class procedure OnTNotifyEvent_OnPaint(Sender: TObject);
    class procedure OnTNotifyEvent_OnResize(Sender: TObject);
    class procedure OnTNotifyEvent_OnShow(Sender: TObject);
    class procedure OnTNotifyEvent_OnEnter(Sender: TObject);
    class procedure OnTNotifyEvent_OnExit(Sender: TObject);
    class procedure OnTNotifyEvent_OnChange(Sender: TObject);
    class procedure OnTNotifyEvent_OnPopup(Sender: TObject);
    class procedure OnTNotifyEvent_OnSelect(Sender: TObject);
    class procedure OnTNotifyEvent_OnClose(Sender: TObject);
    class procedure OnTNotifyEvent_OnChanging(Sender: TObject);
    class procedure OnTNotifyEvent_OnExecute(Sender: TObject);
    class procedure OnTNotifyEvent_OnUpdate(Sender: TObject);
    class procedure OnTNotifyEvent_OnTimer(Sender: TObject);
    class procedure OnTNotifyEvent_OnClickCheck(Sender: TObject);
    class procedure OnTNotifyEvent_OnFind(Sender: TObject);
    class procedure OnTNotifyEvent_OnReplace(Sender: TObject);
    class procedure OnTNotifyEvent_OnTopLeftChanged(Sender: TObject);
    class procedure OnTNotifyEvent_OnSectionEndDrag(Sender: TObject);
    class procedure OnTNotifyEvent_OnDestroy(Sender: TObject);
    class procedure OnTNotifyEvent_OnDropDown(Sender: TObject);
    class procedure OnTNotifyEvent_OnSelectionChanged(Sender: TObject);
    class procedure OnTNotifyEvent_OnCloseUp(Sender: TObject);



    class function OnTHelpEvent_OnHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
    class procedure OnTShortCutEvent_OnShortCut(var Msg: TLMKey; var Handled: Boolean);
    class procedure OnTAlignPositionEvent_OnAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
    class procedure OnTCloseEvent_OnClose(Sender: TObject; var Action: TCloseAction);
    class procedure OnTCloseQueryEvent_OnCloseQuery(Sender: TObject; var CanClose: Boolean);
    class procedure OnTContextPopupEvent_OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    class procedure OnTDockDropEvent_OnDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    class procedure OnTDragDropEvent_OnDragDrop(Sender, Source: TObject; X, Y: Integer);
    class procedure OnTDragOverEvent_OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    class procedure OnTEndDragEvent_OnEndDrag(Sender, Target: TObject; X, Y: Integer);
    class procedure OnTEndDragEvent_OnEndDock(Sender, Target: TObject; X, Y: Integer);
    class procedure OnTGetSiteInfoEvent_OnGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    class procedure OnTKeyEvent_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    class procedure OnTKeyEvent_OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    class procedure OnTKeyPressEvent_OnKeyPress(Sender: TObject; var Key: Char);
    class procedure OnTMouseEvent_OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    class procedure OnTMouseEvent_OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    class procedure OnTMouseMoveEvent_OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    class procedure OnTMouseWheelEvent_OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    class procedure OnTMouseWheelUpDownEvent_OnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    class procedure OnTMouseWheelUpDownEvent_OnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    class procedure OnTStartDockEvent_OnStartDock(Sender: TObject; var DragObject: TDragDockObject);
    class procedure OnTUnDockEvent_OnUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
    class procedure OnTMenuChangeEvent_OnChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    class procedure OnTDrawItemEvent_OnDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    class procedure OnTMeasureItemEvent_OnMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    class procedure OnTSysLinkEvent_OnLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    class procedure OnTUDChangingEvent_OnChanging(Sender: TObject; var AllowChange: Boolean);
    class procedure OnTUDClickEvent_OnClick(Sender: TObject; Button: TUDBtnType);
    class procedure OnTLVAdvancedCustomDrawEvent_OnAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    class procedure OnTLVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    class procedure OnTLVAdvancedCustomDrawSubItemEvent_OnAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    class procedure OnTLVChangeEvent_OnChange(Sender: TObject; AItem: TListItem; Change: TItemChange);
    class procedure OnTLVColumnClickEvent_OnColumnClick(Sender: TObject; Column: TListColumn);
    class procedure OnTLVCompareEvent_OnCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    class procedure OnTLVCustomDrawEvent_OnCustomDraw(Sender: TCustomListView; const ARect: TRect; var DefaultDraw: Boolean);
    class procedure OnTLVCustomDrawItemEvent_OnCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    class procedure OnTLVCustomDrawSubItemEvent_OnCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    class procedure OnTLVDataEvent_OnData(Sender: TObject; Item: TListItem);
    class procedure OnTLVDataFindEvent_OnDataFind(Sender: TObject; Find: TItemFind; const FindString: string; const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean; var Index: Integer);
    class procedure OnTLVDataHintEvent_OnDataHint(Sender: TObject; StartIndex, EndIndex: Integer);
    class procedure OnTLVDeletedEvent_OnDeletion(Sender: TObject; Item: TListItem);
    class procedure OnTLVEditedEvent_OnEdited(Sender: TObject; Item: TListItem; var S: string);
    class procedure OnTLVEditingEvent_OnEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    class procedure OnTLVDeletedEvent_OnInsert(Sender: TObject; Item: TListItem);
    class procedure OnTLVSelectItemEvent_OnSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    class procedure OnTLVCheckedItemEvent_OnItemChecked(Sender: TObject; Item: TListItem);
    class procedure OnTTVExpandedEvent_OnAddition(Sender: TObject; Node: TTreeNode);
    class procedure OnTTVExpandedEvent_OnCollapsed(Sender: TObject; Node: TTreeNode);
    class procedure OnTTVExpandedEvent_OnDeletion(Sender: TObject; Node: TTreeNode);
    class procedure OnTTVExpandedEvent_OnExpanded(Sender: TObject; Node: TTreeNode);
    class procedure OnTTVExpandedEvent_OnGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    class procedure OnTTVAdvancedCustomDrawEvent_OnAdvancedCustomDraw(Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    class procedure OnTTVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    class procedure OnTTVChangedEvent_OnChange(Sender: TObject; ANode: TTreeNode);
    class procedure OnTTVChangingEvent_OnChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    class procedure OnTTVCollapsingEvent_OnCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    class procedure OnTTVCompareEvent_OnCompare(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);
    class procedure OnTTVCustomDrawEvent_OnCustomDraw(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean);
    class procedure OnTTVCustomDrawItemEvent_OnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    class procedure OnTTVEditedEvent_OnEdited(Sender: TObject; Node: TTreeNode; var S: string);
    class procedure OnTTVEditingEvent_OnEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    class procedure OnTTVExpandingEvent_OnExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    class procedure OnTMenuMeasureItemEvent_OnMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    class procedure OnTTabChangingEvent_OnChanging(Sender: TObject; var AllowChange: Boolean);
    class procedure OnTWebTitleChangeEvent_OnTitleChange(Sender: TObject; const Text: string);
    class procedure OnTWebJSExternalEvent_OnJSExternal(Sender: TObject; const Afunc: string; const AArgs: string; var ARetval: string);

    class procedure OnTDrawCellEvent_OnDrawCell(Sender: TObject; ACol, ARow: Longint; ARect: TRect; State: TGridDrawState);
    class procedure OnTGetEditEvent_OnGetEditMask(Sender: TObject; ACol, ARow: Integer; var Value: string);
    class procedure OnTGetEditEvent_OnGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    class procedure OnTSetEditEvent_OnSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    class procedure OnTSelectCellEvent_OnSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    class procedure OnTSectionNotifyEvent_OnSectionClick(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    class procedure OnTSectionNotifyEvent_OnSectionResize(HeaderControl: TCustomHeaderControl; Section: THeaderSection);
    class procedure OnTSectionTrackEvent_OnSectionTrack(HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    class procedure OnTSectionDragEvent_OnSectionDrag(Sender: TObject; FromSection, ToSection: THeaderSection; var AllowDrag: Boolean);
    class procedure OnTTaskDlgClickEvent_OnButtonClicked(Sender: TObject; ModalResult: TModalResult; var CanClose: Boolean);
    class procedure OnTCheckGroupClicked_OnItemClick(Sender: TObject; Index: integer);

    class procedure OnTConstrainedResizeEvent_OnConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
    class procedure OnTDropFilesEvent_OnDropFiles(Sender: TObject; const AFileNames: array of string);


    //TStringGrid
    class procedure OnTOnSelectEvent_OnAfterSelection(Sender: TObject; aCol, aRow: Integer);
    class procedure OnTOnSelectEvent_OnBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    class procedure OnTOnSelectEvent_OnButtonClick(Sender: TObject; aCol, aRow: Integer);
    class procedure OnTToggledCheckboxEvent_OnCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    class procedure OnTGridOperationEvent_OnColRowDeleted(Sender: TObject; IsColumn:Boolean; sIndex, tIndex: Integer);
    class procedure OnTGridOperationEvent_OnColRowExchanged(Sender: TObject; IsColumn:Boolean; sIndex, tIndex: Integer);
    class procedure OnTGridOperationEvent_OnColRowInserted(Sender: TObject; IsColumn:Boolean; sIndex, tIndex: Integer);
    class procedure OnTGridOperationEvent_OnColRowMoved(Sender: TObject; IsColumn: Boolean; FromIndex, ToIndex: Longint);
    class procedure OnTOnCompareCells_OnCompareCells(Sender: TObject; ACol, ARow, BCol,BRow: Integer; var Result: integer);
    class procedure OnTNotifyEvent_OnEditingDone(Sender: TObject);
    class procedure OnTGetCellHintEvent_OnGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    class procedure OnTGetCheckboxStateEvent_OnGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    class procedure OnTSetCheckboxStateEvent_OnSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    class procedure OnTHdrEvent_OnHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    class procedure OnTHdrEvent_OnHeaderSized(Sender: TObject; IsColumn: Boolean; Index: Integer);
    class procedure OnTHeaderSizingEvent_OnHeaderSizing(sender: TObject; const IsColumn: boolean; const aIndex, aSize: Integer);
    class procedure OnTNotifyEvent_OnPickListSelect(Sender: TObject);
    class procedure OnTOnSelectEvent_OnSelection(Sender: TObject; aCol, aRow: Integer);
    class procedure OnTSelectEditorEvent_OnSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    class procedure OnTUserCheckboxBitmapEvent_OnUserCheckboxBitmap(Sender: TObject; const aCol, aRow: Integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
    class procedure OnTValidateEntryEvent_OnValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    //--------------------------------------------------------------------------

    class procedure Add(AObj: TObject; AEvent: Pointer; AId: NativeUInt);
    class procedure Remove(AObj: TObject; AEvent: Pointer);
    class procedure ThreadProc;

    // 用户定义事件声明
    {$I UserDefineEventsDeclaration.inc}
  public
    class property ThreadEvtId: NativeUInt read FThreadEvtId write FThreadEvtId;
  end;

  // 窗口消息的，不与之前的事件混在一起。
  TMessageEventList = specialize  TFPGMap<NativeUInt, NativeUInt>;

  { TMessageEventClass }

  TMessageEventClass = class
  private class var
    FMsgEvents: TMessageEventList;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Add(AObj: TObject; AId: NativeUInt);
    class procedure Remove(AObj: TObject);

    class procedure OnWndProc(Sender: TObject; var TheMessage: TLMessage);
  end;

implementation


{ TEventClass}

class constructor TEventClass.Create;
begin
  FMainEvents := TMainEventList.Create;
end;

class destructor TEventClass.Destroy;
begin
  FMainEvents.Clear;
  FMainEvents.Free;
end;

class procedure TEventClass.Add(AObj: TObject; AEvent: Pointer; AId: NativeUInt);
var
  LSub: TEventList = nil;
begin
  if AObj is TTrayIcon then
     AObj := Application;
  if not FMainEvents.TryGetData(AEvent, LSub) then
  begin
    LSub := TEventList.Create;
    FMainEvents.AddOrSetData(AEvent, LSub);
  end;
  // 更新数据，对象指针他可能释放了下次还是那块区域，如果不更新则出问题
  if Assigned(LSub) then
    LSub.AddOrSetData(NativeUInt(AObj), AId);
end;

class procedure TEventClass.Remove(AObj: TObject; AEvent: Pointer);
var
  LSub: TEventList = nil;
begin
  if FMainEvents.TryGetData(AEvent, LSub) then
  begin
    if LSub.IndexOf(NativeUInt(AObj)) <> -1 then
      LSub.Remove(NativeUInt(AObj));
    if LSub.Count = 0  then
      FMainEvents.Remove(AEvent);
  end;
end;

class procedure TEventClass.ThreadProc;
begin
  GThreadSyncCallbackPtr();
end;

class procedure TEventClass.SendEvent(Sender: TObject; AEvent: Pointer; AArgs: array of const);


  procedure SendEventSrc(EventId: NativeUInt; AArgs: array of const);
  var
    LParams: array[0..CALL_MAX_PARAM-1] of Pointer;
    LArgLen: Integer;
    LV: TVarRec;
    I: Integer;
  begin
    if Assigned(GEventCallbackPtr) and (EventId > 0) then
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
        GEventCallbackPtr(EventId, @LParams[0], LArgLen);
      end;
    end;
  end;

var
  LEventId: NativeUInt = 0;
  LSub: TEventList = nil;
begin
  if FMainEvents.TryGetData(AEvent, LSub) then
  begin
    if LSub.TryGetData(NativeUInt(Sender), LEventId) then
    begin
      SendEventSrc(LEventId, AArgs);
      Exit;
    end;
  end;
  Writeln('can''t found id, sender:', sender.ToString, ', event:', NativeUInt(AEvent));
end;

//------------------新方式-------------------------------

class procedure TEventClass.OnTExceptionEvent_OnException(Sender: TObject; E: Exception);
begin
  SendEvent(Sender, @TEventClass.OnTExceptionEvent_OnException, [Sender, E]);
end;

class procedure TEventClass.OnTNotifyEvent_OnActivate(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnActivate, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnDeactivate(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnDeactivate, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnHint(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnHint, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnMinimize(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnMinimize, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnRestore(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnRestore, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnClick(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnClick, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnDblClick(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnDblClick, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnHide(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnHide, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnMouseEnter(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnMouseEnter, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnMouseLeave(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnMouseLeave, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnPaint(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnPaint, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnResize(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnResize, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnShow(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnShow, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnEnter(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnEnter, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnExit(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnExit, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnChange(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnChange, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnPopup(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnPopup, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnSelect(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnSelect, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnClose(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnClose, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnChanging(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnChanging, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnExecute(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnExecute, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnUpdate(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnUpdate, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnTimer(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnTimer, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnClickCheck(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnClickCheck, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnFind(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnFind, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnReplace(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnReplace, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnTopLeftChanged(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnTopLeftChanged, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnSectionEndDrag(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnSectionEndDrag, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnDestroy(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnDestroy, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnDropDown(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnDropDown, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnSelectionChanged(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnSelectionChanged, [Sender]);
end;

class procedure TEventClass.OnTNotifyEvent_OnCloseUp(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnCloseUp, [Sender]);
end;

class function TEventClass.OnTHelpEvent_OnHelp(Command: Word; Data: PtrInt;
  var CallHelp: Boolean): Boolean;
var
  LResult: Boolean;
begin
  SendEvent(Application, @TEventClass.OnTHelpEvent_OnHelp, [Command, Data, Pointer(@CallHelp), Pointer(@LResult)]);
  Result := LResult;
end;

class procedure TEventClass.OnTShortCutEvent_OnShortCut(var Msg: TLMKey;
  var Handled: Boolean);
begin
  SendEvent(Application, @TEventClass.OnTShortCutEvent_OnShortCut, [Pointer(@Msg), Pointer(@Handled)]);
end;

class procedure TEventClass.OnTAlignPositionEvent_OnAlignPosition(Sender: TWinControl;
  Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  SendEvent(Sender, @TEventClass.OnTAlignPositionEvent_OnAlignPosition, [Sender, Control, @NewLeft, @NewTop, @NewWidth, @NewHeight, @AlignRect, @AlignInfo]);
end;

class procedure TEventClass.OnTCloseEvent_OnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SendEvent(Sender, @TEventClass.OnTCloseEvent_OnClose, [Sender, @Action]);
end;

class procedure TEventClass.OnTCloseQueryEvent_OnCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTCloseQueryEvent_OnCloseQuery, [Sender, @CanClose]);
end;

class procedure TEventClass.OnTContextPopupEvent_OnContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTContextPopupEvent_OnContextPopup, [Sender, Pointer(@MousePos), Pointer(@Handled)]);
end;

class procedure TEventClass.OnTDockDropEvent_OnDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTDockDropEvent_OnDockDrop, [Sender, Source, X, Y]);
end;

class procedure TEventClass.OnTDragDropEvent_OnDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTDragDropEvent_OnDragDrop, [Sender, Source, X, Y]);
end;

class procedure TEventClass.OnTDragOverEvent_OnDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTDragOverEvent_OnDragOver, [Sender, Source, X, Y, Integer(State), Pointer(@Accept)]);
end;

class procedure TEventClass.OnTEndDragEvent_OnEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTEndDragEvent_OnEndDrag, [Sender, Target, X, Y]);
end;

class procedure TEventClass.OnTEndDragEvent_OnEndDock(Sender, Target: TObject;
  X, Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTEndDragEvent_OnEndDock, [Sender, Target, X, Y]);
end;

class procedure TEventClass.OnTGetSiteInfoEvent_OnGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTGetSiteInfoEvent_OnGetSiteInfo, [Sender, DockClient, Pointer(@InfluenceRect), Pointer(@MousePos), Pointer(@CanDock)]);
end;

class procedure TEventClass.OnTKeyEvent_OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SendEvent(Sender, @TEventClass.OnTKeyEvent_OnKeyDown, [Sender, @Key, PWord(@Shift)^]);
end;

class procedure TEventClass.OnTKeyEvent_OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SendEvent(Sender, @TEventClass.OnTKeyEvent_OnKeyUp, [Sender, @Key, PWord(@Shift)^]);
end;

class procedure TEventClass.OnTKeyPressEvent_OnKeyPress(Sender: TObject; var Key: Char);
var
  LKey: Word;
begin
  // 这里要修复下
  LKey := Ord(Key);
  SendEvent(Sender, @TEventClass.OnTKeyPressEvent_OnKeyPress, [Sender, @LKey]);
  Key := Char(LKey);
end;

class procedure TEventClass.OnTMouseEvent_OnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTMouseEvent_OnMouseDown, [Sender, Ord(Button), PWord(@Shift)^, X, Y]);
end;


class procedure TEventClass.OnTMouseEvent_OnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTMouseEvent_OnMouseUp, [Sender, Ord(Button), PWord(@Shift)^, X, Y]);
end;

class procedure TEventClass.OnTMouseMoveEvent_OnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTMouseMoveEvent_OnMouseMove, [Sender, PWord(@Shift)^, X, Y]);
end;

class procedure TEventClass.OnTMouseWheelEvent_OnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTMouseWheelEvent_OnMouseWheel, [Sender, PWord(@Shift)^, WheelDelta, MousePos.X, MousePos.Y, @Handled]);
end;

class procedure TEventClass.OnTMouseWheelUpDownEvent_OnMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTMouseWheelUpDownEvent_OnMouseWheelDown, [Sender, PWord(@Shift)^, Pointer(@MousePos), Pointer(@Handled)]);
end;

class procedure TEventClass.OnTMouseWheelUpDownEvent_OnMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTMouseWheelUpDownEvent_OnMouseWheelUp, [Sender, PWord(@Shift)^, Pointer(@MousePos), Pointer(@Handled)]);
end;


class procedure TEventClass.OnTStartDockEvent_OnStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  SendEvent(Sender, @TEventClass.OnTStartDockEvent_OnStartDock, [Sender, @DragObject]);
end;

class procedure TEventClass.OnTUnDockEvent_OnUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTUnDockEvent_OnUnDock, [Sender, Client, NewTarget, Pointer(@Allow)]);
end;

class procedure TEventClass.OnTMenuChangeEvent_OnChange(Sender: TObject;
  Source: TMenuItem; Rebuild: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTMenuChangeEvent_OnChange, [Sender, Source, Rebuild]);
end;

class procedure TEventClass.OnTDrawItemEvent_OnDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  SendEvent(Control, @TEventClass.OnTDrawItemEvent_OnDrawItem, [Control, Index, @ARect, PWord(@State)^]);
end;

class procedure TEventClass.OnTMeasureItemEvent_OnMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  SendEvent(Control, @TEventClass.OnTMeasureItemEvent_OnMeasureItem, [Control, Index, @Height]);
end;

class procedure TEventClass.OnTSysLinkEvent_OnLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  SendEvent(Sender, @TEventClass.OnTSysLinkEvent_OnLinkClick, [Sender, PChar(Link), Ord(LinkType)]);
end;

class procedure TEventClass.OnTUDChangingEvent_OnChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTUDChangingEvent_OnChanging, [Sender, @AllowChange]);
end;

class procedure TEventClass.OnTUDClickEvent_OnClick(Sender: TObject; Button: TUDBtnType
  );
begin
  SendEvent(Sender, @TEventClass.OnTUDClickEvent_OnClick, [Sender, Ord(Button)]);
end;

class procedure TEventClass.OnTLVAdvancedCustomDrawEvent_OnAdvancedCustomDraw(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVAdvancedCustomDrawEvent_OnAdvancedCustomDraw, [
    Sender, @ARect, Ord(Stage), @DefaultDraw
  ]);
end;

class procedure TEventClass.OnTLVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem, [
    Sender, Item, PWord(@State)^, Ord(Stage), @DefaultDraw
  ]);
end;

class procedure TEventClass.OnTLVAdvancedCustomDrawSubItemEvent_OnAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVAdvancedCustomDrawSubItemEvent_OnAdvancedCustomDrawSubItem, [
    Sender, Item, SubItem, PWord(@State)^, Ord(Stage), @DefaultDraw
  ]);
end;

class procedure TEventClass.OnTLVChangeEvent_OnChange(Sender: TObject; AItem: TListItem;
  Change: TItemChange);
begin
  SendEvent(Sender, @TEventClass.OnTLVChangeEvent_OnChange, [Sender, AItem, Ord(Change)]);
end;

class procedure TEventClass.OnTLVColumnClickEvent_OnColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SendEvent(Sender, @TEventClass.OnTLVColumnClickEvent_OnColumnClick, [Sender, Column]);
end;

class procedure TEventClass.OnTLVCompareEvent_OnCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTLVCompareEvent_OnCompare, [Sender, Item1, Item2, Data, @Compare]);
end;

class procedure TEventClass.OnTLVCustomDrawEvent_OnCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVCustomDrawEvent_OnCustomDraw, [Sender, @ARect, @DefaultDraw]);
end;

class procedure TEventClass.OnTLVCustomDrawItemEvent_OnCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVCustomDrawItemEvent_OnCustomDrawItem, [Sender, Item, PWord(@State)^, @DefaultDraw]);
end;

class procedure TEventClass.OnTLVCustomDrawSubItemEvent_OnCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVCustomDrawSubItemEvent_OnCustomDrawSubItem, [Sender, Item, SubItem, PWord(@State)^, @DefaultDraw]);
end;

class procedure TEventClass.OnTLVDataEvent_OnData(Sender: TObject; Item: TListItem
  );
begin
  SendEvent(Sender, @TEventClass.OnTLVDataEvent_OnData, [Sender, Item]);
end;

class procedure TEventClass.OnTLVDataFindEvent_OnDataFind(Sender: TObject;
  Find: TItemFind; const FindString: string; const FindPosition: TPoint;
  FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
  Wrap: Boolean; var Index: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTLVDataFindEvent_OnDataFind, [Sender, Ord(Find), PChar(FindString), @FindPosition, FindData, StartIndex,
    Ord(Direction), Integer(Wrap), @Index]);
end;

class procedure TEventClass.OnTLVDataHintEvent_OnDataHint(Sender: TObject;
  StartIndex, EndIndex: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTLVDataHintEvent_OnDataHint, [Sender, StartIndex, EndIndex]);
end;

class procedure TEventClass.OnTLVDeletedEvent_OnDeletion(Sender: TObject; Item: TListItem);
begin
  SendEvent(Sender, @TEventClass.OnTLVDeletedEvent_OnDeletion, [Sender, Item]);
end;

class procedure TEventClass.OnTLVEditedEvent_OnEdited(Sender: TObject; Item: TListItem;
  var S: string);
var
  LS: PChar;
begin
  LS := PChar(S);
  SendEvent(Sender, @TEventClass.OnTLVEditedEvent_OnEdited, [Sender, Item, @LS]);
  S := LS;
end;

class procedure TEventClass.OnTLVEditingEvent_OnEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVEditingEvent_OnEditing, [Sender, Item, @AllowEdit]);
end;

class procedure TEventClass.OnTLVDeletedEvent_OnInsert(Sender: TObject;
  Item: TListItem);
begin
  SendEvent(Sender, @TEventClass.OnTLVDeletedEvent_OnInsert, [Sender, Item]);
end;

class procedure TEventClass.OnTLVSelectItemEvent_OnSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTLVSelectItemEvent_OnSelectItem, [Sender, Item, Selected]);
end;

class procedure TEventClass.OnTLVCheckedItemEvent_OnItemChecked(Sender: TObject;
  Item: TListItem);
begin
  SendEvent(Sender, @TEventClass.OnTLVCheckedItemEvent_OnItemChecked, [Sender, Item]);
end;

class procedure TEventClass.OnTTVExpandedEvent_OnAddition(Sender: TObject; Node: TTreeNode
  );
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandedEvent_OnAddition, [Sender, Node]);
end;

class procedure TEventClass.OnTTVExpandedEvent_OnCollapsed(Sender: TObject;
  Node: TTreeNode);
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandedEvent_OnCollapsed, [Sender, Node]);
end;

class procedure TEventClass.OnTTVExpandedEvent_OnDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandedEvent_OnDeletion, [Sender, Node]);
end;

class procedure TEventClass.OnTTVExpandedEvent_OnExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandedEvent_OnExpanded, [Sender, Node]);
end;

class procedure TEventClass.OnTTVExpandedEvent_OnGetSelectedIndex(
  Sender: TObject; Node: TTreeNode);
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandedEvent_OnGetSelectedIndex, [Sender, Node]);
end;

class procedure TEventClass.OnTTVAdvancedCustomDrawEvent_OnAdvancedCustomDraw(
  Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVAdvancedCustomDrawEvent_OnAdvancedCustomDraw,
    [Sender, @ARect, Ord(Stage), DefaultDraw]);
end;

class procedure TEventClass.OnTTVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVAdvancedCustomDrawItemEvent_OnAdvancedCustomDrawItem,
    [Sender, Node, PWord(@State)^, Ord(Stage), @PaintImages, DefaultDraw]);
end;

class procedure TEventClass.OnTTVChangedEvent_OnChange(Sender: TObject; ANode: TTreeNode
  );
begin
  SendEvent(Sender, @TEventClass.OnTTVChangedEvent_OnChange, [Sender, ANode]);
end;

class procedure TEventClass.OnTTVChangingEvent_OnChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVChangingEvent_OnChanging, [Sender, Node, @AllowChange]);
end;

class procedure TEventClass.OnTTVCollapsingEvent_OnCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVCollapsingEvent_OnCollapsing, [Sender, Node, @AllowCollapse]);
end;

class procedure TEventClass.OnTTVCompareEvent_OnCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTTVCompareEvent_OnCompare, [Sender, Node1, Node2, 0, @Compare]);
end;

class procedure TEventClass.OnTTVCustomDrawEvent_OnCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVCustomDrawEvent_OnCustomDraw, [Sender, @ARect, @DefaultDraw]);
end;

class procedure TEventClass.OnTTVCustomDrawItemEvent_OnCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVCustomDrawItemEvent_OnCustomDrawItem, [Sender, Node, PWord(@State)^, @DefaultDraw]);
end;

class procedure TEventClass.OnTTVEditedEvent_OnEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  LS: PChar;
begin
  LS := PChar(S);
  SendEvent(Sender, @TEventClass.OnTTVEditedEvent_OnEdited, [Sender, Node, @LS]);
  S := LS;
end;

class procedure TEventClass.OnTTVEditingEvent_OnEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVEditingEvent_OnEditing, [Sender, Node, @AllowEdit]);
end;

class procedure TEventClass.OnTTVExpandingEvent_OnExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTVExpandingEvent_OnExpanding, [Sender, Node, @AllowExpansion]);
end;

class procedure TEventClass.OnTMenuMeasureItemEvent_OnMeasureItem(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTMenuMeasureItemEvent_OnMeasureItem, [Sender, ACanvas, @Width, @Height]);
end;

class procedure TEventClass.OnTTabChangingEvent_OnChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTabChangingEvent_OnChanging, [Sender, @AllowChange]);
end;

class procedure TEventClass.OnTWebTitleChangeEvent_OnTitleChange(Sender: TObject;
  const Text: string);
begin
  SendEvent(Sender, @TEventClass.OnTWebTitleChangeEvent_OnTitleChange, [Sender, PChar(Text)]);
end;

class procedure TEventClass.OnTWebJSExternalEvent_OnJSExternal(Sender: TObject;
  const Afunc: string; const AArgs: string; var ARetval: string);
var
  LRet: PChar;
begin
  LRet := PChar(ARetval);
  SendEvent(Sender, @TEventClass.OnTWebJSExternalEvent_OnJSExternal, [Sender, PChar(Afunc), PChar(AArgs), @LRet]);
  ARetval := string(LRet);
end;

class procedure TEventClass.OnTGridOperationEvent_OnColRowMoved(Sender: TObject; IsColumn: Boolean;
  FromIndex, ToIndex: Longint);
begin
  SendEvent(Sender, @TEventClass.OnTGridOperationEvent_OnColRowMoved, [Sender, IsColumn, FromIndex, ToIndex]);
end;

class procedure TEventClass.OnTOnCompareCells_OnCompareCells(Sender: TObject;
  ACol, ARow, BCol, BRow: Integer; var Result: integer);
begin
  SendEvent(Sender, @TEventClass.OnTOnCompareCells_OnCompareCells, [Sender, ACol, ARow, BCol, BRow, @Result]);
end;

class procedure TEventClass.OnTNotifyEvent_OnEditingDone(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnEditingDone, [Sender]);
end;

class procedure TEventClass.OnTGetCellHintEvent_OnGetCellHint(Sender: TObject;
  ACol, ARow: Integer; var HintText: String);
var
  LVal: PChar;
begin
  LVal := PChar(HintText);
  SendEvent(Sender, @TEventClass.OnTGetCellHintEvent_OnGetCellHint, [Sender, aCol, aRow, @LVal]);
  if LVal <> nil then
    HintText := LVal;
end;

class procedure TEventClass.OnTGetCheckboxStateEvent_OnGetCheckboxState(
  Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
begin
  SendEvent(Sender, @TEventClass.OnTGetCheckboxStateEvent_OnGetCheckboxState, [Sender, ACol, ARow, @Value]);
end;

class procedure TEventClass.OnTSetCheckboxStateEvent_OnSetCheckboxState(
  Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
begin
  SendEvent(Sender, @TEventClass.OnTSetCheckboxStateEvent_OnSetCheckboxState, [Sender, ACol, ARow, PWord(@Value)^]);
end;

class procedure TEventClass.OnTHdrEvent_OnHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTHdrEvent_OnHeaderClick, [Sender, IsColumn, Index]);
end;

class procedure TEventClass.OnTHdrEvent_OnHeaderSized(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTHdrEvent_OnHeaderSized, [Sender, IsColumn, Index]);
end;

class procedure TEventClass.OnTHeaderSizingEvent_OnHeaderSizing(
  sender: TObject; const IsColumn: boolean; const aIndex, aSize: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTHeaderSizingEvent_OnHeaderSizing, [Sender, IsColumn, aIndex, aSize]);
end;

class procedure TEventClass.OnTNotifyEvent_OnPickListSelect(Sender: TObject);
begin
  SendEvent(Sender, @TEventClass.OnTNotifyEvent_OnPickListSelect, [Sender]);
end;

class procedure TEventClass.OnTOnSelectEvent_OnSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTOnSelectEvent_OnSelection, [Sender, aCol, aRow]);
end;

class procedure TEventClass.OnTSelectEditorEvent_OnSelectEditor(
  Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
  SendEvent(Sender, @TEventClass.OnTSelectEditorEvent_OnSelectEditor, [Sender, aCol, aRow, @Editor]);
end;

class procedure TEventClass.OnTUserCheckboxBitmapEvent_OnUserCheckboxBitmap(
  Sender: TObject; const aCol, aRow: Integer;
  const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  SendEvent(Sender, @TEventClass.OnTUserCheckboxBitmapEvent_OnUserCheckboxBitmap, [Sender, aCol, aRow, PWord(@CheckedState)^, @ABitmap]);
end;

class procedure TEventClass.OnTValidateEntryEvent_OnValidateEntry(
  sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  LVal: PChar;
begin
  LVal := PChar(NewValue);
  SendEvent(Sender, @TEventClass.OnTValidateEntryEvent_OnValidateEntry, [Sender, aCol, aRow, PChar(OldValue), @LVal]);
  if LVal <> nil then
    NewValue := LVal;
end;

class procedure TEventClass.OnTDrawCellEvent_OnDrawCell(Sender: TObject; ACol,
  ARow: Longint; ARect: TRect; State: TGridDrawState);
begin
  SendEvent(Sender, @TEventClass.OnTDrawCellEvent_OnDrawCell, [Sender, ACol, ARow, Pointer(@ARect), PWord(@State)^]);
end;

class procedure TEventClass.OnTGetEditEvent_OnGetEditMask(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
var
  LS: PChar;
begin
  LS := PChar(Value);
  SendEvent(Sender, @TEventClass.OnTGetEditEvent_OnGetEditMask, [Sender, ACol, ARow, Pointer(@LS)]);
  Value := LS;
end;

class procedure TEventClass.OnTGetEditEvent_OnGetEditText(Sender: TObject;
  ACol, ARow: Integer; var Value: string);
var
  LS: PChar;
begin
  LS := PChar(Value);
  SendEvent(Sender, @TEventClass.OnTGetEditEvent_OnGetEditText, [Sender, ACol, ARow, Pointer(@LS)]);
  Value := LS;
end;

class procedure TEventClass.OnTSelectCellEvent_OnSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTSelectCellEvent_OnSelectCell, [Sender, ACol, ARow, Pointer(@CanSelect)]);
end;

class procedure TEventClass.OnTSetEditEvent_OnSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  SendEvent(Sender, @TEventClass.OnTSetEditEvent_OnSetEditText, [Sender, ACol, ARow, PChar(Value)]);
end;

class procedure TEventClass.OnTSectionNotifyEvent_OnSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  SendEvent(HeaderControl, @TEventClass.OnTSectionNotifyEvent_OnSectionClick, [HeaderControl, Section]);
end;

class procedure TEventClass.OnTSectionNotifyEvent_OnSectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  SendEvent(HeaderControl, @TEventClass.OnTSectionNotifyEvent_OnSectionResize, [HeaderControl, Section]);
end;

class procedure TEventClass.OnTSectionTrackEvent_OnSectionTrack(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
begin
  SendEvent(HeaderControl, @TEventClass.OnTSectionTrackEvent_OnSectionTrack, [HeaderControl, Section, Width, Integer(State)]);
end;

class procedure TEventClass.OnTSectionDragEvent_OnSectionDrag(Sender: TObject; FromSection,
  ToSection: THeaderSection; var AllowDrag: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTSectionDragEvent_OnSectionDrag, [Sender, FromSection, ToSection, Pointer(@AllowDrag)]);
end;

class procedure TEventClass.OnTTaskDlgClickEvent_OnButtonClicked(Sender: TObject;
  ModalResult: TModalResult; var CanClose: Boolean);
begin
  SendEvent(Sender, @TEventClass.OnTTaskDlgClickEvent_OnButtonClicked, [Sender, ModalResult, @CanClose]);
end;

class procedure TEventClass.OnTCheckGroupClicked_OnItemClick(Sender: TObject; Index: integer
  );
begin
  SendEvent(Sender, @TEventClass.OnTCheckGroupClicked_OnItemClick, [Sender, Index]);
end;

class procedure TEventClass.OnTConstrainedResizeEvent_OnConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
  SendEvent(Sender, @TEventClass.OnTConstrainedResizeEvent_OnConstrainedResize, [Sender, @MinWidth, @MinHeight, @MaxWidth, @MaxHeight]);
end;

class procedure TEventClass.OnTDropFilesEvent_OnDropFiles(Sender: TObject; const AFileNames: array of string);
var
  LLen: Integer;
begin
  LLen := Length(AFileNames);
  if LLen > 0 then
    SendEvent(Sender, @TEventClass.OnTDropFilesEvent_OnDropFiles, [Sender, @AFileNames[0], LLen]);
end;

class procedure TEventClass.OnTOnSelectEvent_OnAfterSelection(Sender: TObject;
  aCol, aRow: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTOnSelectEvent_OnAfterSelection, [Sender, aCol, aRow]);
end;

class procedure TEventClass.OnTOnSelectEvent_OnBeforeSelection(Sender: TObject;
  aCol, aRow: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTOnSelectEvent_OnBeforeSelection, [Sender, aCol, aRow]);
end;

class procedure TEventClass.OnTOnSelectEvent_OnButtonClick(Sender: TObject;
  aCol, aRow: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTOnSelectEvent_OnButtonClick, [Sender, aCol, aRow]);
end;

class procedure TEventClass.OnTToggledCheckboxEvent_OnCheckboxToggled(
  sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  SendEvent(Sender, @TEventClass.OnTToggledCheckboxEvent_OnCheckboxToggled, [Sender, aCol, aRow, PWord(@aState)^]);
end;

class procedure TEventClass.OnTGridOperationEvent_OnColRowDeleted(
  Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTGridOperationEvent_OnColRowDeleted, [Sender, IsColumn, sIndex, tIndex]);
end;

class procedure TEventClass.OnTGridOperationEvent_OnColRowExchanged(
  Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTGridOperationEvent_OnColRowExchanged, [Sender, IsColumn, sIndex, tIndex]);
end;

class procedure TEventClass.OnTGridOperationEvent_OnColRowInserted(
  Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  SendEvent(Sender, @TEventClass.OnTGridOperationEvent_OnColRowInserted, [Sender, IsColumn, sIndex, tIndex]);
end;

// 用户定义事件实现引入
{$I UserDefineEventsImplement.inc}

{ TMessageEventClass }

class constructor TMessageEventClass.Create;
begin
  FMsgEvents := TEventList.Create;
end;

class destructor TMessageEventClass.Destroy;
begin
  FMsgEvents.Free;
end;

class procedure TMessageEventClass.OnWndProc(Sender: TObject; var TheMessage: TLMessage);
var
  LId: NativeUInt;
begin
  if Assigned(GMessageCallbackPtr) then
  begin
    if FMsgEvents.TryGetData(NativeUInt(Sender), LId) then
      GMessageCallbackPtr(LId, @TheMessage);
  end;
end;

class procedure TMessageEventClass.Remove(AObj: TObject);
begin
  FMsgEvents.Remove(NativeUInt(AObj));
end;

class procedure TMessageEventClass.Add(AObj: TObject; AId: NativeUInt);
begin
  FMsgEvents.AddOrSetData(NativeUInt(AObj), AId);
end;


end.
