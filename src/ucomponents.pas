//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl;

type
  TClassLists = specialize  TFPGMap<string, TClass>;

  var uClassLists: TClassLists;

implementation

uses
  {$I UseAll.inc}
  ,uControlPatchs;

procedure AddComponentClass(AClass: TClass);
begin
  uClassLists.AddOrSetData(AClass.ClassName, AClass);
end;

procedure MyRegisterComponents(AClass: array of TClass);
var
  LB: TClass;
begin
  for LB in AClass do
    AddComponentClass(LB);
end;

procedure InitClassLists;
begin
  MyRegisterComponents([
      TButton, TBitBtn, TMaskEdit, TEdit, TMainMenu, TPopupMenu, TMemo, TCheckBox,
      TRadioButton,TGroupBox,TLabel,TListBox,TComboBox,TPanel,TImage,TLinkLabel,
      TSpeedButton,TSplitter,TRadioGroup,TStaticText,TColorBox,TColorListBox,
      TTrayIcon,TOpenDialog,TSaveDialog,TColorDialog,TFontDialog,TPrintDialog,
      TOpenPictureDialog,TSavePictureDialog,TRichEdit,TTrackBar,TImageList,TUpDown,
      TProgressBar,TDateTimePicker,TMonthCalendar,TListView,TTreeView,TStatusBar,
      TToolBar,TMenuItem,TPageControl,TTabSheet,TStatusPanels,TActionList,TAction,
      TToolButton,TPaintBox,TTimer,TScrollBar,TShape,TBevel,TScrollBox,
      TCheckListBox,TGauge,TImageButton,TFontDialog,TFindDialog,TReplaceDialog,
      TPageSetupDialog,TPrinterSetupDialog, TSelectDirectoryDialog,TStringGrid,
      TDrawGrid, TValueListEditor, THeaderControl,TLabeledEdit,TBoundLabel,
      TFlowPanel,TCoolBar, TSpinEdit,TMiniWebview,TTaskDialog, TCalendar,
      TComboBoxEx,TXButton, TCheckGroup, TToggleBox
  ]);
  {$I UserDefineComponentsClass.inc}
end;



initialization
  uClassLists := TClassLists.Create;
  InitClassLists;

finalization
  uClassLists.Free;

end.

