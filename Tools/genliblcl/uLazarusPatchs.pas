unit uLazarusPatchs;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Rtti,
  System.IOUtils,
  System.Generics.Collections,
  System.TypInfo,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Types,
  System.IniFiles,
  System.Variants,
  System.Win.Registry,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ImgList,
  Vcl.ExtDlgs,
  Vcl.Buttons,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.Clipbrd,
  Vcl.Mask,

  SHDocVw,
  Vcl.Grids,
  Vcl.ValEdit,
  Vcl.CheckLst,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.jpeg,
  Vcl.Samples.Gauges,
  Vcl.Samples.Spin,
  Vcl.Printers,
  ImageButton,
  uMiniWebview,
  xbutton;

type

  TLMessage = type TMessage;

  TShowInTaskbar = (
    stDefault,  // use default rules for showing taskbar item
    stAlways,   // always show taskbar item for the form
    stNever     // never show taskbar item for the form
  );

  TDropFilesEvent = procedure (Sender: TObject; const FileNames: Array of String) of object;


  TUTF8Char = String[7];

  TGoForm = class(TForm);

  TUTF8KeyPressEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char) of Object;

  TForm = class(Vcl.Forms.TForm)
  private
    FAllowDropFiles: Boolean;
    FOnDropFiles: TDropFilesEvent;
    FShowInTaskBar: TShowInTaskbar;
    FDesignTimePPI: Integer;
    FOnUTF8KeyPress: TUTF8KeyPressEvent;
  public
    procedure InheritedWndProc(var TheMessage: TLMessage);

    // 自定义一些
    procedure EnabledMaximize(AValue: Boolean);
    procedure EnabledMinimize(AValue: Boolean);
    procedure EnabledSystemMenu(AValue: Boolean);

    procedure ScaleForCurrentDpi; override;
    procedure ScaleForPPI(ANewPPI: Integer);


    procedure ScreenCenter;
    procedure WorkAreaCenter;

    class function Create2(AOwner: TComponent; AInitScale: Boolean): TGoForm;
  published
    property AllowDropFiles: Boolean read FAllowDropFiles write FAllowDropFiles;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property ShowInTaskBar: TShowInTaskbar read FShowInTaskBar write FShowInTaskBar;
    property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI default 96;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read FOnUTF8KeyPress write FOnUTF8KeyPress;
  end;

  TFrame = class(Vcl.Forms.TFrame)
  private
    FDesignTimePPI: Integer;
    FOnDestroy: TNotifyEvent;
  published
    property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI default 96;
//    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TPanel = class(Vcl.ExtCtrls.TPanel)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

//  TMovedEvent = procedure (Sender: TObject; IsColumn:Boolean;  sIndex, tIndex: Integer) of object;

  TAutoAdvance = (aaNone,aaDown,aaRight,aaLeft, aaRightDown, aaLeftDown,
    aaRightUp, aaLeftUp);
   TCellHintPriority = (chpAll, chpAllNoDefault, chpTruncOnly);

  TColumnButtonStyle = (
    cbsAuto,
    cbsEllipsis,
    cbsNone,
    cbsPickList,
    cbsCheckboxColumn,
    cbsButton,
    cbsButtonColumn
  );

  TTitleStyle = (tsLazarus, tsStandard, tsNative);

  TGridFlagsOption = (gfEditorUpdateLock, gfNeedsSelectActive, gfEditorTab,
    gfRevEditorTab, gfVisualChange, gfColumnsLocked,
    gfEditingDone, gfSizingStarted, gfPainting, gfUpdatingSize, gfClientRectChange,
    gfAutoEditPending, gfUpdatingScrollbar);
  TGridFlags = set of TGridFlagsOption;

  TSortOrder = (soAscending, soDescending);

  TPrefixOption = (poNone, poHeaderClick);

  TMouseWheelOption = (mwCursor, mwGrid);


  TGridColumn = class;

  TGridColumnTitle = class(TPersistent)
  private
    FColumn: TGridColumn;
    FCaption: PChar;
    FColor: ^TColor;
    FAlignment: ^TAlignment;
    FFont: TFont;
    FImageIndex: TImageIndex;
    FImageLayout: TButtonLayout;
    FIsDefaultTitleFont: boolean;
    FLayout: ^TTextLayout;
    FPrefixOption: TPrefixOption;
    FMultiline: Boolean;
    FIsDefaultCaption: boolean;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetLayout: TTextLayout;
    function IsAlignmentStored: boolean;
    function IsCaptionStored: boolean;
    function IsColorStored: boolean;
    function IsFontStored: boolean;
    function IsLayoutStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetImageLayout(const AValue: TButtonLayout);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetMultiLine(const AValue: Boolean);
    procedure SetPrefixOption(const AValue: TPrefixOption);
    procedure WriteCaption(Writer: TWriter);

    property IsDefaultFont: boolean read FIsDefaultTitleFont;
  protected
    function  GetDefaultCaption: string; virtual;
    function  GetDefaultAlignment: TAlignment;
    function  GetDefaultColor: TColor;
    function  GetDefaultLayout: TTextLayout;
    function  GetOwner: TPersistent; override;
    function  GetCaption: TCaption;
    procedure SetCaption(const AValue: TCaption); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(TheColumn: TGridColumn); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FillTitleDefaultFont;
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); virtual;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); virtual;
    function IsDefault: boolean;
    property Column: TGridColumn read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageLayout: TButtonLayout read FImageLayout write SetImageLayout default blGlyphRight;
    property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default false;
    property PrefixOption: TPrefixOption read FPrefixOption write SetPrefixOption default poNone;
  end;


  TGridColumn = class(TCollectionItem)
  private
    FButtonStyle: TColumnButtonStyle;
    FDropDownRows: Longint;
    FTitle: TGridColumnTitle;
    FWidthChanged: boolean;
    FAlignment: ^TAlignment;
    FColor: ^TColor;
    FLayout: ^TTextLayout;
    FVisible: ^Boolean;
    FReadOnly: ^Boolean;
    FWidth: ^Integer;
    FFont: TFont;
    FisDefaultFont: Boolean;
    FPickList: TStrings;
    FMinSize, FMaxSize, FSizePriority: ^Integer;
    FValueChecked,FValueUnchecked: PChar;
    FTag: NativeInt;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetExpanded: Boolean;
    function GetFont: TFont;
    function GetGrid: TCustomGrid;
    function GetLayout: TTextLayout;
    function GetMaxSize: Integer;
    function GetMinSize: Integer;
    function GetSizePriority: Integer;
    function GetReadOnly: Boolean;
    function GetStoredWidth: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsAlignmentStored: boolean;
    function IsColorStored: boolean;
    function IsFontStored: boolean;
    function IsLayoutStored: boolean;
    function IsMinSizeStored: boolean;
    function IsMaxSizeStored: boolean;
    function IsReadOnlyStored: boolean;
    function IsSizePriorityStored: boolean;
    function IsValueCheckedStored: boolean;
    function IsValueUncheckedStored: boolean;
    function IsVisibleStored: boolean;
    function IsWidthStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetButtonStyle(const AValue: TColumnButtonStyle);
    procedure SetColor(const AValue: TColor);
    procedure SetExpanded(const AValue: Boolean);
    procedure SetFont(const AValue: TFont);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetMaxSize(const AValue: Integer);
    procedure SetMinSize(const Avalue: Integer);
    procedure SetPickList(const AValue: TStrings);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetSizePriority(const AValue: Integer);
    procedure SetTitle(const AValue: TGridColumnTitle);
    procedure SetValueChecked(const AValue: string);
    procedure SetValueUnchecked(const AValue: string);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
  protected
    function  GetDisplayName: string; override;
    function  GetDefaultAlignment: TAlignment; virtual;
    function  GetDefaultColor: TColor; virtual;
    function  GetDefaultLayout: TTextLayout; virtual;
    function  GetDefaultMaxSize: Integer; virtual;
    function  GetDefaultMinSize: Integer; virtual;
    function  GetDefaultReadOnly: boolean; virtual;
    function  GetDefaultSizePriority: Integer;
    function  GetDefaultVisible: boolean; virtual;
    function  GetDefaultValueChecked: string; virtual;
    function  GetDefaultValueUnchecked: string; virtual;
    function  GetDefaultWidth: Integer; virtual;
    function  GetPickList: TStrings; virtual;
    function  GetValueChecked: string;
    function  GetValueUnchecked: string;
    procedure ColumnChanged; virtual;
    procedure AllColumnsChange;
    function  CreateTitle: TGridColumnTitle; virtual;
    procedure SetIndex(Value: Integer); override;

    property  IsDefaultFont: boolean read FIsDefaultFont;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FillDefaultFont;
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); virtual;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); virtual;
    function  IsDefault: boolean; virtual;
    property Grid: TCustomGrid read GetGrid;
    property DefaultWidth: Integer read GetDefaultWidth;
    property StoredWidth: Integer read GetStoredWidth;
    property WidthChanged: boolean read FWidthChanged;

  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property DropDownRows: Longint read FDropDownRows write FDropDownRows default 7;
    property Expanded: Boolean read GetExpanded write SetExpanded default True;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
    property MinSize: Integer read GetMinSize write SetMinSize stored IsMinSizeStored;
    property MaxSize: Integer read GetMaxSize write SetMaxSize stored isMaxSizeStored;
    property PickList: TStrings read GetPickList write SetPickList;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property SizePriority: Integer read GetSizePriority write SetSizePriority stored IsSizePriorityStored;
    property Tag: NativeInt read FTag write FTag default 0;
    property Title: TGridColumnTitle read FTitle write SetTitle;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored;
    property ValueChecked: string read GetValueChecked write SetValueChecked
      stored IsValueCheckedStored;
    property ValueUnchecked: string read GetValueUnchecked write SetValueUnchecked
      stored IsValueUncheckedStored;
  end;


  TGridColumns = class(TCollection)
  private
    FGrid: TCustomGrid;
    function GetColumn(Index: Integer): TGridColumn;
    function GetEnabled: Boolean;
    procedure SetColumn(Index: Integer; Value: TGridColumn);
    function GetVisibleCount: Integer;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    procedure TitleFontChanged;
    procedure FontChanged;
    procedure RemoveColumn(Index: Integer);
    procedure MoveColumn(FromIndex,ToIndex: Integer); virtual;
    procedure ExchangeColumn(Index,WithIndex: Integer);
    procedure InsertColumn(Index: Integer);
  public
    constructor Create(AGrid: TCustomGrid; aItemClass: TCollectionItemClass);
    function Add: TGridColumn;
    procedure Clear;
    function RealIndex(Index: Integer): Integer;
    function IndexOf(Column: TGridColumn): Integer;
    function IsDefault: boolean;
    function HasIndex(Index: Integer): boolean;
    function VisibleIndex(Index: Integer): Integer;
    property Grid: TCustomGrid read FGrid;
    property Items[Index: Integer]: TGridColumn read GetColumn write SetColumn; default;
    property VisibleCount: Integer read GetVisibleCount;
    property Enabled: Boolean read GetEnabled;
  end;


  TGridZone = (gzNormal, gzFixedCols, gzFixedRows, gzFixedCells, gzInvalid);
  TGridZoneSet = set of TGridZone;

  TGridOption2 = (
    goScrollToLastCol,  // allow scrolling to last column (so that last column can be leftcol)
    goScrollToLastRow   // allow scrolling to last row (so that last row can be toprow)
  );
  TGridOptions2 = set of TGridOption2;

  TOnSelectCellEvent =
    procedure(Sender: TObject; aCol, aRow: Integer;
              var CanSelect: Boolean) of object;

  TOnSelectEvent =
    procedure(Sender: TObject; aCol, aRow: Integer) of object;

  TGridOperationEvent =
    procedure (Sender: TObject; IsColumn:Boolean;
               sIndex, tIndex: Integer) of object;

  THdrEvent =
    procedure(Sender: TObject; IsColumn: Boolean; Index: Integer) of object;

  TOnCompareCells =
    procedure (Sender: TObject; ACol, ARow, BCol,BRow: Integer;
               var Result: integer) of object;

  TSelectEditorEvent =
    procedure(Sender: TObject; aCol, aRow: Integer;
              var Editor: TWinControl) of object;

  TOnPrepareCanvasEvent =
    procedure(sender: TObject; aCol, aRow: Integer;
              aState: TGridDrawState) of object;

  TUserCheckBoxBitmapEvent =
    procedure(Sender: TObject; const aCol, aRow: Integer;
              const CheckedState: TCheckboxState;
              var ABitmap: TBitmap) of object;

  TValidateEntryEvent =
    procedure(sender: TObject; aCol, aRow: Integer;
              const OldValue: string; var NewValue: String) of object;

  TToggledCheckboxEvent = procedure(sender: TObject; aCol, aRow: Integer;
                                    aState: TCheckboxState) of object;

  THeaderSizingEvent = procedure(sender: TObject; const IsColumn: boolean;
                                    const aIndex, aSize: Integer) of object;

//  TCellProcessEvent = procedure(Sender: TObject; aCol, aRow: Integer;
//                                processType: TCellProcessType;
//                                var aValue: string) of object;

  TGetCellHintEvent = procedure (Sender: TObject; ACol, ARow: Integer;
                                 var HintText: String) of object;

//  TSaveColumnEvent = procedure (Sender, aColumn: TObject; aColIndex: Integer;
//                                aCfg: TXMLConfig; const aVersion: integer;
//                                const aPath: string) of object;

    { Option goRangeSelect: --> select a single range only, or multiple ranges }
  TRangeSelectMode = (rsmSingle, rsmMulti);


  TGetCheckboxStateEvent = procedure (Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState) of object;
  TSetCheckboxStateEvent = procedure (Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState) of object;


  TDrawGrid = class(Vcl.Grids.TDrawGrid)
  private
    FOnColRowMoved: TGridOperationEvent;
    FOnPrepareCanvas: TOnPrepareCanvasEvent;
  published
    property OnColRowMoved: TGridOperationEvent read FOnColRowMoved write FOnColRowMoved;
    property OnPrepareCanvas: TOnPrepareCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
  end;



  TStringGrid = class(Vcl.Grids.TStringGrid)
  private
    FOnColRowMoved: TGridOperationEvent;
    FAlternateColor: TColor;
    FAutoAdvance: TAutoAdvance;
    FAutoEdit: Boolean;
    FAutoFillColumns: Boolean;
    FCellHintPriority: TCellHintPriority;
    FColumnClickSorts: Boolean;
    FColumns: TGridColumns;
    FExtendedSelect: boolean;
    FFlat: Boolean;
    FHeaderHotZones: TGridZoneSet;
    FHeaderPushZones: TGridZoneSet;
    FDescImgInd: TImageIndex;
    FAscImgInd: TImageIndex;
    FMouseWheelOption: TMouseWheelOption;
    FRangeSelectMode: TRangeSelectMode;
    FTabAdvance: TAutoAdvance;
    FOptions2: TGridOptions2;
    FTitleFont: TFont;
    FTitleImageList: TImageList;
    FTitleStyle: TTitleStyle;
    FUseXORFeatures: boolean;
    FOnGetCellHint: TGetCellHintEvent;
//    FOnShowHint: TControlShowHintEvent;
    FOnGetCheckboxState: TGetCheckboxStateEvent;
    FOnPickListSelect: TNotifyEvent;
    FOnButtonClick: TOnSelectEvent;
    FOnAfterSelection: TOnSelectEvent;
    FOnHeaderClick: THdrEvent;
    FOnColRowInserted: TGridOperationEvent;
    FOnColRowExchanged: TgridOperationEvent;
    FOnValidateEntry: TValidateEntryEvent;
    FOnUserCheckboxBitmap: TUserCheckboxBitmapEvent;
    FOnHeaderSizing: THeaderSizingEvent;
    fOnSelection: TOnSelectEvent;
    FOnSetCheckboxState: TSetCheckboxStateEvent;
//    FOnDrawCell: TOnDrawCell;
    FOnEditingDone: TNotifyEvent;
    FOnColRowDeleted: TGridOperationEvent;
    FOnHeaderSized: THdrEvent;
    FOnBeforeSelection: TOnSelectEvent;
    FOnCompareCells: TOnCompareCells;
    FOnSelectEditor: TSelectEditorEvent;
    FOnCheckboxToggled: TToggledCheckboxEvent;
    FOnPrepareCanvas: TOnPrepareCanvasEvent;
    FEditor: TWinControl;
    FEditorBorderStyle: TBorderStyle;
    FEditorOptions: Integer;
    FEditorKey: boolean;
    FEditorMode: Boolean;
    FEditorShowing: boolean;
    FSelectedColor: TColor;
    FSelectedColumn: TGridColumn;
    FAllowOutboundEvents: boolean;
    FFixedGridLineColor: TColor;
    FFastEditing: boolean;
    FFocusColor: TColor;
    FFocusRectVisible: Boolean;
    FGridLineStyle: TPenStyle;
    FGridLineColor: TColor;
    FStrictSort: boolean;
    FFixedHotColor: TColor;
    FSortColumn: Integer;
    FSortOrder: TSortOrder;
    procedure SetOptions2(const Value: TGridOptions2);
    procedure SetRangeSelectMode(const Value: TRangeSelectMode);
    procedure SetUseXorFeatures(const Value: boolean);
    procedure EditorSetMode(const Value: Boolean);
    procedure SetFixedGridLineColor(const Value: TColor);
    procedure SetFocusColor(const Value: TColor);
    function GetIsCellSelected(aCol, aRow: Integer): boolean;
  public
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property SelectedColumn: TGridColumn read FSelectedColumn;
    property AllowOutboundEvents: boolean read FAllowOutboundEvents write FAllowOutboundEvents default true;
    property StrictSort: boolean read FStrictSort write FStrictSort;
    procedure DeleteColRow(IsColumn: Boolean; index: Integer);
    procedure DeleteCol(Index: Integer); virtual;
    procedure DeleteRow(Index: Integer); virtual;
    procedure ExchangeColRow(IsColumn: Boolean; index, WithIndex: Integer); virtual;
    procedure InsertColRow(IsColumn: boolean; index: integer);
    procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
//    procedure SortColRow(IsColumn: Boolean; index:Integer); overload;
    procedure SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer); overload;
    property FixedHotColor: TColor read FFixedHotColor write FFixedHotColor default cl3DLight;
    procedure DefaultDrawCell(aCol,aRow: Integer; var aRect: TRect; aState:TGridDrawState); virtual;
    property FastEditing: boolean read FFastEditing write FFastEditing;
    property FixedGridLineColor: TColor read FFixedGridLineColor write SetFixedGridLineColor default cl3DDKShadow;
    property FocusColor: TColor read FFocusColor write SetFocusColor;
    property FocusRectVisible: Boolean read FFocusRectVisible write FFocusRectVisible;
    property GridLineColor: TColor read FGridLineColor write FGridLineColor default clSilver;
    property GridLineStyle: TPenStyle read FGridLineStyle write FGridLineStyle;
    property IsCellSelected[aCol,aRow: Integer]: boolean read GetIsCellSelected;
    function  EditorByStyle(Style: TColumnButtonStyle): TWinControl;
    procedure EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyUp(Sender: TObject; var key:Word; shift:TShiftState);
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string);
    procedure EditingDone;
    procedure AutoAdjustColumns;
    property Editor: TWinControl read FEditor write FEditor;
    property EditorBorderStyle: TBorderStyle read FEditorBorderStyle write FEditorBorderStyle;
    property EditorMode: Boolean read FEditorMode write EditorSetMode;
    function  CellRect(ACol, ARow: Integer): TRect;
    function  CellToGridZone(aCol,aRow: Integer): TGridZone;
    procedure CheckPosition;
    function ClearCols: Boolean;
    function ClearRows: Boolean;
    procedure Clear;
    procedure ClearSelections;
    property SortOrder: TSortOrder read FSortOrder write FSortOrder;
    property SortColumn: Integer read FSortColumn;
    function  HasMultiSelection: Boolean;
    procedure InvalidateCell(aCol, aRow: Integer); overload;
    procedure InvalidateCol(ACol: Integer);
    procedure InvalidateRange(const aRange: TRect);
    procedure InvalidateRow(ARow: Integer);
    function  IsCellVisible(aCol, aRow: Integer): Boolean;
    function  IsFixedCellVisible(aCol, aRow: Integer): boolean;
    function  MouseCoord(X,Y: Integer): TGridCoord;
    function  MouseToCell(const Mouse: TPoint): TPoint;
    function  MouseToLogcell(Mouse: TPoint): TPoint;
    function  MouseToGridZone(X,Y: Integer): TGridZone;
  published

    property OnAfterSelection: TOnSelectEvent read FOnAfterSelection write FOnAfterSelection;
    property OnBeforeSelection: TOnSelectEvent read FOnBeforeSelection write FOnBeforeSelection;
    property OnButtonClick: TOnSelectEvent read FOnButtonClick write FOnButtonClick;
    property OnCheckboxToggled: TToggledCheckboxEvent read FOnCheckboxToggled write FOnCheckboxToggled;
    property OnColRowDeleted: TGridOperationEvent read FOnColRowDeleted write FOnColRowDeleted;
    property OnColRowExchanged: TgridOperationEvent read FOnColRowExchanged write FOnColRowExchanged;
    property OnColRowInserted: TGridOperationEvent read FOnColRowInserted write FOnColRowInserted;
    property OnColRowMoved: TGridOperationEvent read FOnColRowMoved write FOnColRowMoved;
    property OnCompareCells: TOnCompareCells read FOnCompareCells write FOnCompareCells;
    property OnEditingDone: TNotifyEvent read FOnEditingDone write FOnEditingDone;
    property OnGetCellHint : TGetCellHintEvent read FOnGetCellHint write FOnGetCellHint;
    property OnGetCheckboxState: TGetCheckboxStateEvent read FOnGetCheckboxState write FOnGetCheckboxState;
    property OnSetCheckboxState: TSetCheckboxStateEvent read FOnSetCheckboxState write FOnSetCheckboxState;
    property OnHeaderClick: THdrEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderSized: THdrEvent read FOnHeaderSized write FOnHeaderSized;
    property OnHeaderSizing: THeaderSizingEvent read FOnHeaderSizing write FOnHeaderSizing;
    property OnPickListSelect: TNotifyEvent read FOnPickListSelect write FOnPickListSelect;
    property OnSelection: TOnSelectEvent read fOnSelection write fOnSelection;
    property OnSelectEditor: TSelectEditorEvent read FOnSelectEditor write FOnSelectEditor;
    property OnUserCheckboxBitmap: TUserCheckboxBitmapEvent read FOnUserCheckboxBitmap write FOnUserCheckboxBitmap;
    property OnValidateEntry: TValidateEntryEvent read FOnValidateEntry write FOnValidateEntry;
    property OnPrepareCanvas: TOnPrepareCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;



    property AlternateColor: TColor read FAlternateColor write FAlternateColor;
    property AutoAdvance: TAutoAdvance read FAutoAdvance write FAutoAdvance default aaRight;
    property AutoEdit: Boolean read FAutoEdit write FAutoEdit;
    property AutoFillColumns: Boolean read FAutoFillColumns write FAutoFillColumns;
    property CellHintPriority: TCellHintPriority read FCellHintPriority write FCellHintPriority default chpAllNoDefault;
    property ColumnClickSorts: Boolean read FColumnClickSorts write FColumnClickSorts;
    property Columns: TGridColumns read FColumns write FColumns; //stored IsColumnsStored;
    property ExtendedSelect: boolean read FExtendedSelect write FExtendedSelect default true;
    property Flat: Boolean read FFlat write FFlat;
    property HeaderHotZones: TGridZoneSet read FHeaderHotZones write FHeaderHotZones default [gzFixedCols];
    property HeaderPushZones: TGridZoneSet read FHeaderPushZones write FHeaderPushZones default [gzFixedCols];
    property ImageIndexSortAsc: TImageIndex read FAscImgInd write FAscImgInd default -1;
    property ImageIndexSortDesc: TImageIndex read FDescImgInd write FDescImgInd default -1;
    property MouseWheelOption: TMouseWheelOption read FMouseWheelOption write FMouseWheelOption default mwCursor;
    property Options2: TGridOptions2 read FOptions2 write SetOptions2 ;//default DefaultGridOptions2;
    property RangeSelectMode: TRangeSelectMode read FRangeSelectMode write SetRangeSelectMode default rsmSingle;
    property TabAdvance: TAutoAdvance read FTabAdvance write FTabAdvance default aaRightDown;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property TitleImageList: TImageList read FTitleImageList write FTitleImageList;
    property TitleStyle: TTitleStyle read FTitleStyle write FTitleStyle default tsLazarus;
    property UseXORFeatures: boolean read FUseXORFeatures write SetUseXorFeatures default false;
  end;




  TAnchorSideReference = (asrTop, asrBottom, asrCenter);

  TSpacingSize = Integer;

  TControlCellAlign = (
    ccaFill,
    ccaLeftTop,
    ccaRightBottom,
    ccaCenter
    );
  TControlCellAligns = set of TControlCellAlign;

  TAnchorSide = class(TPersistent)
  private
    FKind: TAnchorKind;
    FControl: TControl;
    FOwner: TControl;
    FSide: TAnchorSideReference;
//    function IsSideStored: boolean;
//    procedure SetControl(const AValue: TControl);
//    procedure SetSide(const AValue: TAnchorSideReference);
//  protected
//    function GetOwner: TPersistent; override;
  public
//    constructor Create(TheOwner: TControl; TheKind: TAnchorKind);
//    destructor Destroy; override;
//    procedure GetSidePosition(out ReferenceControl: TControl; out ReferenceSide: TAnchorSideReference; out Position: Integer);
//    function CheckSidePosition(NewControl: TControl; NewSide: TAnchorSideReference; out ReferenceControl: TControl; out ReferenceSide: TAnchorSideReference; out Position: Integer): boolean;
    procedure Assign(Source: TPersistent); override;
    function IsAnchoredToParent(ParentSide: TAnchorKind): boolean;
    procedure FixCenterAnchoring;
  public
    property Owner: TControl read FOwner;
    property Kind: TAnchorKind read FKind;
  published
    property Control: TControl read FControl write FControl;
    property Side: TAnchorSideReference read FSide write FSide default asrTop;
  end;

  TControlBorderSpacing = class(TPersistent)
  private
    FCellAlignHorizontal: TControlCellAlign;
    FInnerBorder: Integer;
    FAroundBottom: Integer;
    FRight: TSpacingSize;
    FControlTop: Integer;
    FControlHeight: Integer;
    FControlLeft: Integer;
    FControl: TControl;
    FAroundTop: Integer;
    FAroundLeft: Integer;
    FBottom: TSpacingSize;
    FControlRight: Integer;

    FAround: TSpacingSize;
    FControlWidth: Integer;
    FCellAlignVertical: TControlCellAlign;
    FAroundRight: Integer;
    FOnChange: TNotifyEvent;
    FTop: TSpacingSize;
    FLeft: TSpacingSize;
    FControlBottom: Integer;
    function GetSpace(Kind: TAnchorKind): integer;
    procedure SetSpace(Kind: TAnchorKind; const Value: integer);
  public
    property Control: TControl read FControl;
    property Space[Kind: TAnchorKind]: integer read GetSpace write SetSpace;
    property AroundLeft: Integer read FAroundLeft;
    property AroundTop: Integer read FAroundTop;
    property AroundRight: Integer read FAroundRight;
    property AroundBottom: Integer read FAroundBottom;
    property ControlLeft: Integer read FControlLeft;
    property ControlTop: Integer read FControlTop;
    property ControlWidth: Integer read FControlWidth;
    property ControlHeight: Integer read FControlHeight;
    property ControlRight: Integer read FControlRight;
    property ControlBottom: Integer read FControlBottom;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: TSpacingSize read FLeft write FLeft;
    property Top: TSpacingSize read FTop write FTop;
    property Right: TSpacingSize read FRight write FRight;
    property Bottom: TSpacingSize read FBottom write FBottom;
    property Around: TSpacingSize read FAround write FAround;
    property InnerBorder: Integer read FInnerBorder write FInnerBorder;
    property CellAlignHorizontal: TControlCellAlign read FCellAlignHorizontal write FCellAlignHorizontal default ccaFill;
    property CellAlignVertical: TControlCellAlign read FCellAlignVertical write FCellAlignVertical default ccaFill;
  end;


  TChildControlResizeStyle = (
      crsAnchorAligning, // (like Delphi)
      crsScaleChilds, // scale children equally, keep space between children fixed
      crsHomogenousChildResize, // enlarge children equally (i.e. by the same amount of pixel)
      crsHomogenousSpaceResize // enlarge space between children equally
      {$IFDEF EnablecrsSameSize}
      ,crsSameSize  // each child gets the same size (maybe one pixel difference)
      {$ENDIF}
    );

  TControlChildrenLayout = (
      cclNone,
      cclLeftToRightThenTopToBottom, // if BiDiMode <> bdLeftToRight then it becomes RightToLeft
      cclTopToBottomThenLeftToRight
    );




  TControlChildSizing = class(TPersistent)
  private
    FLayout: TControlChildrenLayout;
    FEnlargeVertical: TChildControlResizeStyle;
    FVerticalSpacing: integer;
    FShrinkVertical: TChildControlResizeStyle;
    FControl: TWinControl;
    FEnlargeHorizontal: TChildControlResizeStyle;
    FLeftRightSpacing: integer;
    FHorizontalSpacing: integer;
    FShrinkHorizontal: TChildControlResizeStyle;
    FOnChange: TNotifyEvent;
    FControlsPerLine: integer;
    FTopBottomSpacing: integer;
  public
    property Control: TWinControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property LeftRightSpacing: integer read FLeftRightSpacing write FLeftRightSpacing default 0;
    property TopBottomSpacing: integer read FTopBottomSpacing write FTopBottomSpacing default 0;
    property HorizontalSpacing: integer read FHorizontalSpacing write FHorizontalSpacing default 0;
    property VerticalSpacing: integer read FVerticalSpacing write FVerticalSpacing default 0;
    property EnlargeHorizontal: TChildControlResizeStyle read FEnlargeHorizontal write FEnlargeHorizontal default crsAnchorAligning;
    property EnlargeVertical: TChildControlResizeStyle read FEnlargeVertical write FEnlargeVertical default crsAnchorAligning;
    property ShrinkHorizontal: TChildControlResizeStyle read FShrinkHorizontal write FShrinkHorizontal default crsAnchorAligning;
    property ShrinkVertical: TChildControlResizeStyle read FShrinkVertical write FShrinkVertical default crsAnchorAligning;
    property Layout: TControlChildrenLayout read FLayout write FLayout default cclNone;
    property ControlsPerLine: integer read FControlsPerLine write FControlsPerLine default 0;
  end;


  TApplication = class(Vcl.Forms.TApplication)
  private
    FScaled: Boolean;
    FSingleInstanceEnabled: Boolean;
    FLocation: string;
    FStopOnException: Boolean;
    FExceptionExitCode: LongInt;
  published
    property Scaled: Boolean read FScaled write FScaled;
    property SingleInstanceEnabled: Boolean read FSingleInstanceEnabled write FSingleInstanceEnabled;
    property Location: string read FLocation;
    property StopOnException: Boolean read FStopOnException write FStopOnException;
    property ExceptionExitCode: LongInt read FExceptionExitCode write FExceptionExitCode;
  end;

  TSortDirection = (sdAscending, sdDescending);

  TListItemState = (lisCut, lisDropTarget, lisFocused, lisSelected);
  TListItemStates = set of TListItemState;

  TListView = class(Vcl.ComCtrls.TListView)
  private
    FAutoSortIndicator: Boolean;
    FAutoWidthLastColumn: Boolean;
    FSmallImagesWidth: Integer;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;
    FLargeImagesWidth: Integer;
    FStateImagesWidth: Integer;
    FToolTips: Boolean;
    FAutoSort: Boolean;
    FScrollBars: TScrollStyle;
    FColumnCount: Integer;

  public
//    function GetNextItem(StartItem: TListItem; Direction: TSearchDirection; States: TListItemStates): TListItem;
  published
    property AutoSort: Boolean read FAutoSort write FAutoSort;
    property AutoSortIndicator: Boolean read FAutoSortIndicator write FAutoSortIndicator;
    property AutoWidthLastColumn: Boolean read FAutoWidthLastColumn write FAutoWidthLastColumn;
    property SmallImagesWidth: Integer read FSmallImagesWidth write FSmallImagesWidth;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property LargeImagesWidth: Integer read FLargeImagesWidth write FLargeImagesWidth;
    property StateImagesWidth: Integer read FStateImagesWidth write FStateImagesWidth;
    property ToolTips: Boolean read FToolTips write FToolTips;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;

//    function GetHitTestInfoAt(X, Y: Integer): THitTests;
//    function GetNearestItem(APoint: TPoint; Direction: TSearchDirection): TListItem;

    property ColumnCount: Integer read FColumnCount;

  end;

  TTreeViewExpandSignType = (
    tvestTheme,      // use themed sign
    tvestPlusMinus,  // use +/- sign
    tvestArrow,      // use blank arrow
    tvestArrowFill   // use filled arrow
  );

    TTreeViewOption = (
    tvoAllowMultiselect,
    tvoAutoExpand,
    tvoAutoInsertMark,
    tvoAutoItemHeight,
    tvoHideSelection,
    tvoHotTrack,
    tvoKeepCollapsedNodes,
    tvoReadOnly,
    tvoRightClickSelect,
    tvoRowSelect,
    tvoShowButtons,
    tvoShowLines,
    tvoShowRoot,
    tvoShowSeparators,
    tvoToolTips,
    tvoNoDoubleClickExpand,
    tvoThemedDraw
    );
  TTreeViewOptions = set of TTreeViewOption;


  TTreeNode = class(Vcl.ComCtrls.TTreeNode)
  private
//    FStates: TNodeStates;
    FParent: TTreeNode;
    FOwner: TTreeNodes;
    FSelectedIndex: Integer;
    FOverlayIndex: Integer;
    FSubTreeCount: integer;
    FText: string;
//    FNodeEffect: TGraphicsDrawEffect;
    FStateIndex: Integer;
    FImageIndex: TImageIndex;
    FData: Pointer;
    function GetAbsoluteIndex: Integer;
    function GetCount: Integer;
    function GetCut: Boolean;
    function GetDeleting: Boolean;
    function GetDropTarget: Boolean;
    function GetExpanded: Boolean;
    function GetFocused: Boolean;
    function GetHasChildren: Boolean;
    function GetHeight: integer;
    function GetIndex: Integer;
    function GetItems(ItemIndex: Integer): TTreeNode;
    function GetLevel: Integer;
    function GetMultiSelected: Boolean;
    function GetSelected: Boolean;
    function GetTop: integer;
    function GetTreeNodes: TTreeNodes;
    function GetTreeView: TCustomTreeView;
    function GetVisible: Boolean;
    function IsNodeHeightFullVisible: Boolean;
    function IsNodeVisible: Boolean;
    procedure SetCut(const Value: Boolean);
    procedure SetData(const Value: Pointer);
    procedure SetDropTarget(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetHasChildren(const Value: Boolean);
    procedure SetHeight(const Value: integer);
//    procedure SetImageEffect(const Value: TGraphicsDrawEffect);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetIndex(const Value: Integer);
    procedure SetItems(ItemIndex: Integer; const Value: TTreeNode);
    procedure SetMultiSelected(const Value: Boolean);
    procedure SetOverlayIndex(const Value: Integer);
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetStateIndex(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetVisible(const Value: Boolean);
  public
    function AlphaSort: Boolean;
    function Bottom: integer;
    function BottomExpanded: integer;
//    function CustomSort(SortProc: TTreeNodeCompare): Boolean;
    function DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
    function DisplayExpandSignLeft: integer;
    function DisplayExpandSignRect: TRect;
    function DisplayExpandSignRight: integer;
    function DisplayIconLeft: integer;
    function DisplayRect(TextOnly: Boolean): TRect;
    function DisplayStateIconLeft: integer;
    function DisplayTextLeft: integer;
    function DisplayTextRight: integer;
    function EditText: Boolean;
    function FindNode(const NodeText: string): TTreeNode;
    function GetFirstChild: TTreeNode;
    function GetFirstVisibleChild: TTreeNode;
    function GetHandle: THandle;
    function GetLastChild: TTreeNode;
    function GetLastSibling: TTreeNode;
    function GetLastSubChild: TTreeNode;
    function GetLastVisibleChild: TTreeNode;
    function GetNext: TTreeNode;
    function GetNextChild(AValue: TTreeNode): TTreeNode;
    function GetNextExpanded: TTreeNode;
    function GetNextMultiSelected: TTreeNode;
    function GetNextSibling: TTreeNode;
    function GetNextSkipChildren: TTreeNode;
    function GetNextVisible: TTreeNode;
    function GetNextVisibleSibling: TTreeNode;
    function GetParentNodeOfAbsoluteLevel(TheAbsoluteLevel: integer): TTreeNode;
    function GetPrev: TTreeNode;
    function GetPrevChild(AValue: TTreeNode): TTreeNode;
    function GetPrevExpanded: TTreeNode;
    function GetPrevMultiSelected: TTreeNode;
    function GetPrevSibling: TTreeNode;
    function GetPrevVisible: TTreeNode;
    function GetPrevVisibleSibling: TTreeNode;
    function GetTextPath: string;
    function HasAsParent(AValue: TTreeNode): Boolean;
    function IndexOf(AValue: TTreeNode): Integer;
    function IndexOfText(const NodeText: string): Integer;
    procedure Assign(Source: TPersistent); override;
    procedure Collapse(Recurse: Boolean);
    procedure ConsistencyCheck;
    procedure Delete;
    procedure DeleteChildren;
    procedure EndEdit(Cancel: Boolean);
    procedure Expand(Recurse: Boolean);
    procedure ExpandParents;
    procedure FreeAllNodeData;
    procedure MakeVisible;
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual;
    procedure MultiSelectGroup;
    procedure Update;
    procedure WriteDebugReport(const Prefix: string; Recurse: boolean);
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    property Data: Pointer read FData write SetData;
    property Deleting: Boolean read GetDeleting;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Focused: Boolean read GetFocused write SetFocused;
    property Handle: THandle read GetHandle;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property Height: integer read GetHeight write SetHeight;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Index: Integer read GetIndex write SetIndex;
    property IsFullHeightVisible: Boolean read IsNodeHeightFullVisible;
    property IsVisible: Boolean read IsNodeVisible;
    property Items[ItemIndex: Integer]: TTreeNode read GetItems write SetItems; default;
    property Level: Integer read GetLevel;
    property MultiSelected: Boolean read GetMultiSelected write SetMultiSelected;
//    property NodeEffect: TGraphicsDrawEffect read FNodeEffect write SetImageEffect;
    property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex default -1;
    property Owner: TTreeNodes read FOwner;
    property Parent: TTreeNode read FParent;
    property Selected: Boolean read GetSelected write SetSelected;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default -1;
    property StateIndex: Integer read FStateIndex write SetStateIndex default -1;
//    property States: TNodeStates read FStates;
    property SubTreeCount: integer read FSubTreeCount;
    property Text: string read FText write SetText;
    property Top: integer read GetTop;
    property TreeNodes: TTreeNodes read GetTreeNodes;
    property TreeView: TCustomTreeView read GetTreeView;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  TTreeView = class(Vcl.ComCtrls.TTreeView)
  private
    FDefaultItemHeight: Integer;
    FExpandSignColor: TColor;
    FExpandSignSize: Integer;
    FExpandSignType: TTreeViewExpandSignType;
    FHotTrackColor: TColor;
    FImagesWidth: Integer;
    FOptions: TTreeViewOptions;
    FScrollBars: TScrollStyle;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FSelectionFontColorUsed: Boolean;
    FSeparatorColor: TColor;
    FStateImagesWidth: Integer;
    FToolTips: Boolean;
    FTreeLineColor: TColor;
    FTreeLinePenStyle: TPenStyle;
  published
    property DefaultItemHeight: Integer read FDefaultItemHeight write FDefaultItemHeight;
    property ExpandSignColor: TColor read FExpandSignColor write FExpandSignColor;
    property ExpandSignSize: Integer read FExpandSignSize write FExpandSignSize;
    property ExpandSignType: TTreeViewExpandSignType read FExpandSignType write FExpandSignType;
    property HotTrackColor: TColor read FHotTrackColor write FHotTrackColor;
    property ImagesWidth: Integer read FImagesWidth write FImagesWidth;
    property Options: TTreeViewOptions read FOptions write FOptions;
    property ScrollBars: TScrollStyle read FScrollBars write FScrollBars;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property SelectionFontColor: TColor read FSelectionFontColor write FSelectionFontColor;
    property SelectionFontColorUsed: Boolean read FSelectionFontColorUsed write FSelectionFontColorUsed;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor;
    property StateImagesWidth: Integer read FStateImagesWidth write FStateImagesWidth;
    property ToolTips: Boolean read FToolTips write FToolTips;
    property TreeLineColor: TColor read FTreeLineColor write FTreeLineColor;
    property TreeLinePenStyle: TPenStyle read FTreeLinePenStyle write FTreeLinePenStyle;
  end;



  TMainMenu = class(Vcl.Menus.TMainMenu)
  private
    FImagesWidth: Integer;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
  published
    property ImagesWidth: Integer read FImagesWidth write FImagesWidth;
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

  TPopupMenu = class(Vcl.Menus.TPopupMenu)
  private
    FImagesWidth: Integer;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
  published
    property ImagesWidth: Integer read FImagesWidth write FImagesWidth;
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;


  TLabel = class(Vcl.StdCtrls.TLabel)
  private
    FOptimalFill: Boolean;
  published
    property OptimalFill: Boolean read FOptimalFill write FOptimalFill;
  end;


  TAntialiasingMode = (
    amDontCare, // default antialiasing
    amOn,       // enabled
    amOff       // disabled
  );

  TImage = class(Vcl.ExtCtrls.TImage)
  private
    FAntialiasingMode: TAntialiasingMode;
    FKeepOriginXWhenClipped: Boolean;
    FKeepOriginYWhenClipped: Boolean;
    FStretchInEnabled: Boolean;
    FStretchOutEnabled: Boolean;

  published
    property AntialiasingMode: TAntialiasingMode read FAntialiasingMode write FAntialiasingMode default amDontCare;
    property KeepOriginXWhenClipped: Boolean read FKeepOriginXWhenClipped write FKeepOriginXWhenClipped;
    property KeepOriginYWhenClipped: Boolean read FKeepOriginYWhenClipped write FKeepOriginYWhenClipped;
    property StretchInEnabled: Boolean read FStretchInEnabled write FStretchInEnabled;
    property StretchOutEnabled: Boolean read FStretchOutEnabled write FStretchOutEnabled;
  end;

  TSpeedButton = class(Vcl.Buttons.TSpeedButton)
  private
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FImageWidth: Integer;
    FShowCaption: Boolean;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Images: TCustomImageList read FImages write FImages;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
  end;

  TCustomFloatSpinEdit = class(TCustomEdit)
  private const
    DefIncrement = 1;
    DefDecimals = 2;
    DefMaxValue = 0;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetLimitedValue(const AValue: Double): Double; virtual;
    function ValueToStr(const AValue: Double): String; virtual;
    function StrToValue(const S: String): Double; virtual;
  private
    FMinValue: Double;
    FIncrement: Double;
    FDecimals: Integer;
    FValueEmpty: Boolean;
    FMaxValue: Double;
    FEditorEnabled: Boolean;
    FAutoSelected: Boolean;
    function GetValue: Double;
    function IncrementStored: Boolean;
    function MaxValueStored: Boolean;
    procedure SetDecimals(const Value: Integer);
    procedure SetEditorEnabled(const Value: Boolean);
    procedure SetIncrement(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetMinValue(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure SetValueEmpty(const Value: Boolean);
  public
//    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
    property AutoSelected: Boolean read FAutoSelected write FAutoSelected;
    property DecimalPlaces: Integer read FDecimals write SetDecimals default DefDecimals;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property Increment: Double read FIncrement write SetIncrement stored IncrementStored nodefault;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property Value: Double read GetValue write SetValue;
    property ValueEmpty: Boolean read FValueEmpty write SetValueEmpty default False;
  end;

  TFloatSpinEdit = class(TCustomFloatSpinEdit)
  public
    property AutoSelected;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
//    property BorderSpacing;
    property Color;
    property Constraints;
    property DecimalPlaces;
    property EditorEnabled;
    property Enabled;
    property Font;
    property Increment;
    property MaxValue;
    property MinValue;
    property OnChange;
//    property OnChangeBounds;
    property OnClick;
//    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
//    property OnMouseWheelHorz;
//    property OnMouseWheelLeft;
//    property OnMouseWheelRight;
    property OnResize;
//    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Value;
    property Visible;
  end;


  TListBoxOption = (
   lboDrawFocusRect // draw focus rect in case of owner drawing
    );
  TListBoxOptions = set of TListBoxOption;

  TListBox = class(Vcl.StdCtrls.TListBox)
  private
    FClickOnSelChange: Boolean;
    FOptions: TListBoxOptions;
    FTopIndex: Integer;
  published
    property ClickOnSelChange: Boolean read FClickOnSelChange write FClickOnSelChange;
    property Options: TListBoxOptions read FOptions write FOptions;
    property TopIndex: Integer read FTopIndex write FTopIndex;
  end;

    TGlyphShowMode = (
    gsmAlways,       // always show
    gsmNever,        // never show
    gsmApplication,  // depends on application settings
    gsmSystem        // depends on system settings
  );

  TBitBtn = class(Vcl.Buttons.TBitBtn)
  private
    FDefaultCaption: Boolean;
    FGlyphShowMode: TGlyphShowMode;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageWidth: Integer;
  published
    property DefaultCaption: Boolean read FDefaultCaption write FDefaultCaption;
    property GlyphShowMode: TGlyphShowMode read FGlyphShowMode write FGlyphShowMode default gsmApplication;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Images: TCustomImageList read FImages write FImages;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
  end;


  TSplitter = class(Vcl.ExtCtrls.TSplitter)
  private
    FResizeAnchor: TAnchorKind;
  published
     property ResizeAnchor: TAnchorKind read FResizeAnchor write FResizeAnchor default akLeft;
  end;

    // These are LCL additions
  TCTabControlOption = (
    nboShowCloseButtons, nboMultiLine, nboHidePageListPopup,
    nboKeyboardTabSwitch, nboShowAddTabButton, nboDoChangeOnSetIndex);
  TCTabControlOptions = set of TCTabControlOption;

  TPageControl = class(Vcl.ComCtrls.TPageControl)
  private
    FOptions: TCTabControlOptions;
    FScrollOpposite: Boolean;
    FMultiSelect: Boolean;
    FHotTrack: Boolean;
    FPageIndex: Integer;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FOnCloseTabClicked: TNotifyEvent;
    procedure SetPageIndex(const Value: Integer);
  public
    procedure Clear;
    function FindNextPage(CurPage: TTabSheet; GoForward, CheckTabVisible: Boolean): TTabSheet;
    procedure SelectNextPage(GoForward: Boolean);
    function AddTabSheet: TTabSheet;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function IndexOfPageAt(X, Y: Integer): Integer;
    property RaggedRight: Boolean read FRaggedRight write FRaggedRight default False;
    property ScrollOpposite: Boolean read FScrollOpposite write FScrollOpposite default False;
    property OwnerDraw: Boolean read FOwnerDraw write FOwnerDraw default False;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;

  published
    property Options: TCTabControlOptions read FOptions write FOptions default [];

    property OnCloseTabClicked: TNotifyEvent read FOnCloseTabClicked  write FOnCloseTabClicked;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
  end;


  TSelectDirectoryDialog = class(TOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TCheckBox = class(Vcl.StdCtrls.TCheckBox)
  private
    FOnChange: TNotifyEvent;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TRadioButton = class(Vcl.StdCtrls.TRadioButton)
  private
    FOnChange: TNotifyEvent;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCheckGroupClicked = procedure(Sender: TObject; Index: integer) of object;

  TColumnLayout = (
    clHorizontalThenVertical,
    clVerticalThenHorizontal
    );


  TRadioGroup = class(Vcl.ExtCtrls.TRadioGroup)
  private
    FOnSelectionChanged: TNotifyEvent;
  public
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

  TCustomCheckGroup = class(TCustomGroupBox)
  private
    FAutoFill: Boolean;
    FItems: TStrings;
    FColumns: integer;
    FOnItemClick: TCheckGroupClicked;
    FColumnLayout: TColumnLayout;
    FParentBackground: Boolean;
    function GetChecked(Index: integer): boolean;
    function GetCheckEnabled(Index: integer): boolean;
    procedure SetAutoFill(const Value: Boolean);
    procedure SetChecked(Index: integer; const Value: boolean);
    procedure SetCheckEnabled(Index: integer; const Value: boolean);
    procedure SetColumnLayout(const Value: TColumnLayout);
    procedure SetColumns(const Value: integer);
    procedure SetItems(const Value: TStrings);
    procedure SetParentBackground(const Value: Boolean);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    function Rows: integer;
  public
    property AutoFill: Boolean read FAutoFill write SetAutoFill;
    property ParentBackground: Boolean read FParentBackground write SetParentBackground;
    property Items: TStrings read FItems write SetItems;
    property Checked[Index: integer]: boolean read GetChecked write SetChecked;
    property CheckEnabled[Index: integer]: boolean read GetCheckEnabled write SetCheckEnabled;
    property Columns: integer read FColumns write SetColumns default 1;
    property ColumnLayout: TColumnLayout read FColumnLayout write SetColumnLayout default clHorizontalThenVertical;
    property OnItemClick: TCheckGroupClicked read FOnItemClick write FOnItemClick;
  end;

  TCheckGroup = class(TCustomCheckGroup)
  published
    property Align;
    property Anchors;
    property AutoFill;
    property AutoSize;
    property BiDiMode;
//    property BorderSpacing;
    property Caption;
//    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Items;
//    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
//    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentFont;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  TToggleBox = class(TCustomCheckBox)
  private
    FOnChange: TNotifyEvent;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property AllowGrayed;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
//    property BorderSpacing;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;


  TListItem = class(Vcl.ComCtrls.TListItem)
  private
    FDropTarget: Boolean;
  public
    function DisplayRectSubItem(subItem: integer; Code: TDisplayCode): TRect;
    property DropTarget: Boolean read FDropTarget write FDropTarget;
  end;

  TSortIndicator = (siNone, siAscending, siDescending);

  TListColumn = class(Vcl.ComCtrls.TListColumn)
  private
    FSortIndicator: TSortIndicator;
  public
    property SortIndicator: TSortIndicator read FSortIndicator write FSortIndicator;
  end;

  TMemo = class(Vcl.StdCtrls.TMemo)
  public
    procedure Append(const Value: String);
  end;



//  TPrinter = class(Vcl.Printer.TPrinter)
//  public
//     procedure Abort;
//     procedure BeginDoc;
//     procedure EndDoc;
//     procedure NewPage;
//     procedure BeginPage;
//     procedure EndPage;
//     procedure Refresh;
//     procedure SetPrinter(aName : String);
//     Procedure RestoreDefaultBin; virtual;
//     function  Write(const Buffer; Count:Integer; out Written: Integer): Boolean; virtual;
//     function  Write(const s: ansistring): boolean; overload;

//     property PrinterIndex : integer read FPrinterIndex write FPrinterIndex;
//     property PrinterName: string read FPrinterName;
//     property PaperSize : TPaperSize read FPaperSize;
//     property Orientation: TPrinterOrientation read FOrientation write FOrientation;
//     property PrinterState : TPrinterState read FPrinterState;
//     property Copies : Integer read GetCopies write SetCopies;
//     property Printers: TStrings read GetPrinters;
//     property FileName: string read FFileName write FFileName;
//     property Fonts: TStrings read GetFonts;
//     property Canvas: TCanvas read GetCanvas;
//     property CanvasClass: TPrinterCanvasRef read GetCanvasClass write SetCanvasClass;
//     property PageHeight: Integer read GetPageHeight;
//     property PageWidth: Integer read GetPageWidth;
//     property PageNumber : Integer read fPageNumber;
//     property Aborted: Boolean read GetAborted;
//     property Printing: Boolean read GetPrinting;
//     property Title: string read fTitle write fTitle;
//     property PrinterType : TPrinterType read GetPrinterType;
//     property CanPrint : Boolean read GetCanPrint;
//     property CanRenderCopies : Boolean read GetCanRenderCopies;
//     property XDPI : Integer read GetXDPI;
//     property YDPI : Integer read GetYDPI;
//     property RawMode: boolean read GetRawMode write SetRawMode;
//     property DefaultBinName: string read GetDefaultBinName;
//     property BinName: string read GetBinName write SetBinName;
//     property SupportedBins: TStrings read GetBins;
//  end;

  TArrowShape = (asClassicSmaller, asClassicLarger, asModernSmaller,
    asModernLarger, asYetAnotherShape, asTheme);

  TDateDisplayOrder = (ddoDMY, ddoMDY, ddoYMD, ddoTryDefault);
  TDateTimePart = (dtpDay, dtpMonth, dtpYear, dtpHour, dtpMinute,  dtpSecond, dtpMiliSec, dtpAMPM);
  TDateTimeParts = set of TDateTimePart;//dtpDay..dtpMiliSec;
  TDateTimePickerOption = (dtpoDoChangeOnSetDateTime, dtpoEnabledIfUnchecked, dtpoAutoCheck, dtpoFlatButton);
  TDateTimePickerOptions = set of TDateTimePickerOption;
   TTimeDisplay = (tdHM,   // hour and minute
                  tdHMS,  // hour, minute and second
                  tdHMSMs // hour, minute, second and milisecond
                  );

  TTimeFormat = (tf12, // 12 hours format, with am/pm string
                 tf24  // 24 hours format
                 );

  TDateTimePicker = class(Vcl.ComCtrls.TDateTimePicker)
  private
    FArrowShape: TArrowShape;
    FAutoAdvance: Boolean;
    FAutoButtonSize: Boolean;
    FCascade: Boolean;
    FCenturyFrom: Word;
    FDateDisplayOrder: TDateDisplayOrder;
    FDateSeparator: String;
    FHideDateTimeParts: TDateTimeParts;
    FLeadingZeros: Boolean;
    FMonthNames: String;
    FShowMonthNames: Boolean;
    FNullInputAllowed: Boolean;
    FOptions: TDateTimePickerOptions;
    FShowCheckBox: Boolean;
    FReadOnly: Boolean;
    FTextForNullDate: TCaption;
    FTimeDisplay: TTimeDisplay;
    FTimeSeparator: String;
    FTrailingSeparator: Boolean;
    FUseDefaultSeparators: Boolean;
    FDroppedDown: Boolean;
    FTimeFormat: TTimeFormat;

  public
    property ArrowShape: TArrowShape read FArrowShape write FArrowShape default asTheme;
    property AutoAdvance: Boolean read FAutoAdvance write FAutoAdvance default True;
    property AutoButtonSize: Boolean read FAutoButtonSize write FAutoButtonSize default False;
    property Cascade: Boolean read FCascade write FCascade default False;
    property CenturyFrom: Word  read FCenturyFrom write FCenturyFrom;
    property DateDisplayOrder: TDateDisplayOrder read FDateDisplayOrder write FDateDisplayOrder default ddoTryDefault;
    property DateSeparator: String read FDateSeparator write FDateSeparator;
    property HideDateTimeParts: TDateTimeParts read FHideDateTimeParts write FHideDateTimeParts;
    property LeadingZeros: Boolean read FLeadingZeros write FLeadingZeros;
    property MonthNames: String read FMonthNames write FMonthNames;
    property ShowMonthNames: Boolean read FShowMonthNames write FShowMonthNames default False;
    property NullInputAllowed: Boolean  read FNullInputAllowed write FNullInputAllowed default True;
    property Options: TDateTimePickerOptions read FOptions write FOptions;
    property ShowCheckBox: Boolean read FShowCheckBox write FShowCheckBox default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property TextForNullDate: TCaption read FTextForNullDate write FTextForNullDate;
    property TimeDisplay: TTimeDisplay read FTimeDisplay write FTimeDisplay;
    property TimeFormat: TTimeFormat read FTimeFormat write FTimeFormat;
    property TimeSeparator: String read FTimeSeparator write FTimeSeparator;
    property TrailingSeparator: Boolean read FTrailingSeparator write FTrailingSeparator;
    property UseDefaultSeparators: Boolean read FUseDefaultSeparators write FUseDefaultSeparators;
    property DroppedDown: Boolean read FDroppedDown;

    function DateIsNull: Boolean;
    procedure SelectDate;
    procedure SelectTime;
//    procedure SendExternalKey(const aKey: Char);
//    procedure SendExternalKeyCode(const Key: Word);
  end;


  TDisplaySetting = (
    dsShowHeadings,
    dsShowDayNames,
    dsNoMonthChange,
    dsShowWeekNumbers,
    dsStartMonday
  );
  TDisplaySettings = set of TDisplaySetting;

  TMonthCalendar = class(Vcl.ComCtrls.TMonthCalendar)
  private
    FDisplaySettings: TDisplaySettings;
    FDateTime: TDateTime;
    function GetDateTime: TDateTime;
  public
    property DisplaySettings: TDisplaySettings read FDisplaySettings  write FDisplaySettings;// default DefaultDisplaySettings;
    property DateTime: TDateTime read GetDateTime write FDateTime;
  end;

//  TMenuItem = class(Vcl.Menus.TMenuItem)
//  public
//    property ShortCutText: string read FShortCutText
//  end;

  TBitmap = class(Vcl.Graphics.TBitmap)
  public
    procedure LoadFromDevice(ADc: HDC);
    procedure EndUpdate(AStreamIsValid: Boolean);
    procedure BeginUpdate(ACanvasOnly: Boolean);
    procedure Clear;
  end;

  TCustomBitmap = class(vcl.Graphics.TGraphic)

  end;

  TGraphicsDrawEffect =
  (
    gdeNormal,      // no effect
    gdeDisabled,    // grayed image
    gdeHighlighted, // a bit highlighted image
    gdeShadowed,    // a bit shadowed image
    gde1Bit         // 1 Bit image (for non-XP windows buttons)
  );

  TImageList = class(Vcl.Controls.TImageList)
  private
    FScaled: Boolean;
    FShareImages: Boolean;
    FResolutionCount: Integer;
    FCount: Integer;
    function GetHeightForPPI(AImageWidth, APPI: Integer): Integer;
    function GetHeightForWidth(AWidth: Integer): Integer;
    function GetSizeForPPI(AImageWidth, APPI: Integer): TSize;
    function GetWidthForPPI(AImageWidth, APPI: Integer): Integer;
  public
    procedure StretchDraw(ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AEnabled: Boolean);
    function AddSliced(Image: TCustomBitmap; AHorizontalCount, AVerticalCount: Integer): Integer;
    function AddLazarusResource(const ResourceName: string; MaskColor: TColor): integer;
    function AddResourceName(Instance: THandle; const ResourceName: string; MaskColor: TColor): integer;
    function AddSlice(Image: TCustomBitmap; AImageRect: TRect): Integer;
    function AddSliceCentered(Image: TCustomBitmap): Integer;
//    function AddMultipleResolutions(Images: array of TBitmap): Integer;
    procedure RegisterResolutions(const AResolutionWidths: array of Integer);
    procedure DeleteResolution(const AWidth: Integer);

    // 要修改类型
    function AddMultipleResolutions(Images: array of TCustomBitmap): Integer; // always pass sorted array from smallest to biggest
    function Add(Image, Mask: TCustomBitmap): Integer;
    procedure Insert(AIndex: Integer; AImage, AMask: TCustomBitmap);
    procedure InsertMasked(Index: Integer; AImage: TCustomBitmap; MaskColor: TColor);
    procedure Replace(AIndex: Integer; AImage, AMask: TCustomBitmap; const AllResolutions: Boolean = True);
    procedure ReplaceMasked(Index: Integer; NewImage: TCustomBitmap; MaskColor: TColor; const AllResolutions: Boolean = True);
    procedure ReplaceIcon(AIndex: Integer; AIcon: TIcon);

    procedure GetBitmap(Index: Integer; Image: TCustomBitmap; AEffect: TGraphicsDrawEffect);
    procedure GetFullBitmap(Image: TCustomBitmap; AEffect: TGraphicsDrawEffect = gdeNormal);

    property Scaled: Boolean read FScaled write FScaled;
    property ShareImages: Boolean read FShareImages write FShareImages;
    property ResolutionCount: Integer read FResolutionCount;

    property HeightForPPI[AImageWidth, APPI: Integer]: Integer read GetHeightForPPI;
    property HeightForWidth[AWidth: Integer]: Integer read GetHeightForWidth;
    property WidthForPPI[AImageWidth, APPI: Integer]: Integer read GetWidthForPPI;
    property SizeForPPI[AImageWidth, APPI: Integer]: TSize read GetSizeForPPI;
    property Count: Integer read FCount;
  end;

  TClipboardFormat = type UIntPtr;

  TClipboard = class(Vcl.Clipbrd.TClipboard)
  private
    function GetFormats(Index: Integer): TClipboardFormat;
    procedure SetFormats(Index: Integer; const Value: TClipboardFormat);
  public
    function FindPictureFormatID: TClipboardFormat;
    function FindFormatID(const FormatName: string): TClipboardFormat;
    function GetAsHtml(ExtractFragmentOnly: Boolean): string;
    procedure SupportedFormats(List: TStrings);
    function HasFormat(FormatID: TClipboardFormat): Boolean;
    function HasFormatName(const FormatName: string): Boolean;
    function HasPictureFormat: boolean;
    procedure SetAsHtml(Html: String; const PlainText: String);
    function GetFormat(FormatID: TClipboardFormat; Stream: TStream): Boolean;

    property Formats[Index: Integer]: TClipboardFormat read GetFormats;// write SetFormats;
  end;


  TScreen = class(Vcl.Forms.TScreen)
  private
    FFocusedForm: TCustomForm;
    function GetRealCursor: TCursor;
  public
    procedure BeginTempCursor(const aCursor: TCursor);
    procedure EndTempCursor(const aCursor: TCursor);
    procedure BeginWaitCursor;
    procedure EndWaitCursor;

    property RealCursor: TCursor read GetRealCursor;
    property FocusedForm: TCustomForm read FFocusedForm;
  end;


  TEchoMode = (emNormal, emNone, emPassword);

  TTranslateString = type String;

  TCustomAbstractGroupedEdit = class(TCustomControl)
  private
    FLayout: TLeftRight;
    FEdit: TEdit;
    FBuddy: TControl;
    FFocusOnBuddyClick: Boolean;
    function GetAutoSelect: Boolean;
    function GetAutoSelected: Boolean;
    function GetBuddyCaption: TCaption;
    function GetBuddyCursor: TCursor;
    function GetBuddyHint: TTranslateString;
    function GetBuddyWidth: Integer;
    function GetDirectInput: Boolean;
    function GetEditMask: String;
    function GetEditText: string;
    function GetIsMasked: Boolean;
    function GetSpacing: Integer;
    procedure SetAutoSelect(const Value: Boolean);
    procedure SetAutoSelected(const Value: Boolean);
    procedure SetBuddyCaption(const Value: TCaption);
    procedure SetBuddyCursor(const Value: TCursor);
    procedure SetBuddyHint(const Value: TTranslateString);
    procedure SetBuddyWidth(const Value: Integer);
    procedure SetDirectInput(const Value: Boolean);
    procedure SetEditMask(const Value: String);
    procedure SetEditText(const Value: string);
    procedure SetLayout(const Value: TLeftRight);
    procedure SetSpacing(const Value: Integer);
  protected
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property AutoSelected: Boolean read GetAutoSelected write SetAutoSelected;
    property Buddy: TControl read FBuddy;
    property BuddyCaption: TCaption read GetBuddyCaption write SetBuddyCaption;
    property BuddyCursor: TCursor read GetBuddyCursor write SetBuddyCursor default crDefault;
    property BuddyHint: TTranslateString read GetBuddyHint write SetBuddyHint;
    property BuddyWidth: Integer read GetBuddyWidth write SetBuddyWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property BaseEditor: TEdit read FEdit;
    property EditMask: String read GetEditMask write SetEditMask;
    property EditText: string read GetEditText write SetEditText;
    property FocusOnBuddyClick: Boolean read FFocusOnBuddyClick write FFocusOnBuddyClick default False;
    property IsMasked: Boolean read GetIsMasked;
    property Layout: TLeftRight read FLayout write SetLayout default taLeftJustify;
    property Spacing: Integer read GetSpacing write SetSpacing default 0;
  private
    FOnEditDragDrop: TDragDropEvent;
    FOnEditMouseWheelDown: TMouseWheelUpDownEvent;
    FOnEditMouseDown: TMouseEvent;
    FOnEditExit: TNotifyEvent;
    FOnEditDragOver: TDragOverEvent;
    FOnEditMouseMove: TMouseMoveEvent;
    FOnEditMouseEnter: TNotifyEvent;
    FOnEditMouseWheelUp: TMouseWheelUpDownEvent;
    FAutoSizeHeightIsEditHeight: Boolean;
//    FOnEditUtf8KeyPress: TUtf8KeyPressEvent;
    FOnEditMouseUp: TMouseEvent;
    FOnEditMouseWheel: TMouseWheelEvent;
    FOnEditStartDrag: TStartDragEvent;
    FOnEditEditingDone: TNotifyEvent;
    FOnEditKeyPress: TKeyPressEvent;
    FOnEditKeyDown: TKeyEvent;
    FOnEditDblClick: TNotifyEvent;
    FOnEditChange: TNotifyEvent;
    FOnEditEnter: TNotifyEvent;
    FOnEditMouseLeave: TNotifyEvent;
    FOnEditContextPopup: TContextPopupEvent;
    FOnEditKeyUp: TKeyEvent;
    FOnEditClick: TNotifyEvent;
    FOnEditEndDrag: TEndDragEvent;
    FAutoSelected: Boolean;
    function GetAlignment: TAlignment;
    function GetCanUndo: Boolean;
    function GetCaretPos: TPoint;
    function GetCharCase: TEditCharCase;
    function GetColor: TColor;
    function GetEchoMode: TEchoMode;
    function GetEditPopupMenu: TPopupMenu;
    function GetHideSelection: Boolean;
    function GetMaxLength: Integer;
    function GetModified: Boolean;
    function GetNumbersOnly: Boolean;
    function GetParentColor: Boolean;
    function GetPasswordChar: char;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: String;
    function GetTabStop: Boolean;
    function GetTextHint: TTranslateString;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaretPos(const Value: TPoint);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetColor(const Value: TColor);
    procedure SetEchoMode(const Value: TEchoMode);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);
    procedure SetModified(const Value: Boolean);
    procedure SetNumbersOnly(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetPasswordChar(const Value: char);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelText(const Value: String);
    procedure SetTabStop(const Value: Boolean);
    procedure SetTextHint(const Value: TTranslateString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;
    function Focused: Boolean; override;
    procedure Clear;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure SelectAll;
    procedure Undo; virtual;
    procedure ValidateEdit; virtual;

///    property AutoSelected: Boolean read FAutoSelected write FAutoSelected;
    property Autosize default True;
    property AutoSizeHeightIsEditHeight: Boolean read FAutoSizeHeightIsEditHeight write FAutoSizeHeightIsEditHeight default True;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property Color: TColor read GetColor write SetColor stored True default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property ParentColor: Boolean read GetParentColor write SetParentColor default False;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default False;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property Modified: Boolean read GetModified write SetModified;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly default False;
    property PasswordChar: char read GetPasswordChar write SetPasswordChar;
    property PopupMenu: TPopupMenu read GetEditPopupMenu write SetPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
    property Text;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;

    property OnChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnContextPopup: TContextPopupEvent read FOnEditContextPopup write FOnEditContextPopup;
    property OnDblClick: TNotifyEvent read FOnEditDblClick write FOnEditDblClick;
    property OnDragDrop: TDragDropEvent read FOnEditDragDrop write FOnEditDragDrop;
    property OnDragOver: TDragOverEvent read FOnEditDragOver write FOnEditDragOver;
    property OnEditingDone: TNotifyEvent read FOnEditEditingDone write FOnEditEditingDone;
    property OnEndDrag: TEndDragEvent read FOnEditEndDrag write FOnEditEndDrag;
    property OnEnter: TNotifyEvent read FOnEditEnter write FOnEditEnter;
    property OnExit: TNotifyEvent read FOnEditExit write FOnEditExit;
    property OnMouseDown: TMouseEvent read FOnEditMouseDown write FOnEditMouseDown;
    property OnKeyPress: TKeyPressEvent read FOnEditKeyPress write FOnEditKeyPress;
    property OnKeyDown: TKeyEvent read FOnEditKeyDown write FOnEditKeyDown;
    property OnKeyUp: TKeyEvent read FOnEditKeyUp write FOnEditKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnEditMouseEnter write FOnEditMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnEditMouseLeave write FOnEditMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnEditMouseMove write FOnEditMouseMove;
    property OnMouseWheel: TMouseWheelEvent read FOnEditMouseWheel write FOnEditMouseWheel;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnEditMouseWheelUp write FOnEditMouseWheelUp;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnEditMouseWheelDown write FOnEditMouseWheelDown;
    property OnMouseUp: TMouseEvent read FOnEditMouseUp write FOnEditMouseUp;
    property OnStartDrag: TStartDragEvent read FOnEditStartDrag write FOnEditStartDrag;
//    property OnUtf8KeyPress: TUtf8KeyPressEvent read FOnEditUtf8KeyPress write FOnEditUtf8KeyPress;
  end;

  TAcceptFileNameEvent = procedure (Sender : TObject; Var Value : String) of Object;
  TDialogKind = (dkOpen,dkSave,dkPictureOpen,dkPictureSave);

  TCustomEditButton = class(TCustomAbstractGroupedEdit)
  private
    FFlat: Boolean;
    FButtonOnlyWhenFocused: Boolean;
    FSpacing: Integer;
    function GetBuddyCaption: TCaption;
    function GetBuddyCursor: TCursor;
    function GetBuddyHint: TTranslateString;
    function GetBuddyWidth: Integer;
    function GetButton: TSpeedButton;
    function GetEdit: TEdit;
    function GetFocusOnButtonClick: Boolean;
    function GetGlyph: TBitmap;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetImageWidth: Integer;
    function GetNumGlyps: Integer;
    function GetOnButtonClick: TNotifyEvent;
    procedure SetBuddyCaption(const Value: TCaption);
    procedure SetBuddyCursor(const Value: TCursor);
    procedure SetBuddyHint(const Value: TTranslateString);
    procedure SetBuddyWidth(const Value: Integer);
    procedure SetButtonOnlyWhenFocused(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetFocusOnButtonClick(const Value: Boolean);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageWidth(const Value: Integer);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetOnButtonClick(const Value: TNotifyEvent);
    procedure BuddyClick;
    procedure ButtonClick;
    function CalcButtonVisible: Boolean;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean);
    procedure CheckButtonVisible;
  protected

    property Button: TSpeedButton read GetButton;
    property ButtonCaption: TCaption read GetBuddyCaption write SetBuddyCaption;
    property ButtonCursor: TCursor read GetBuddyCursor write SetBuddyCursor default crDefault;
    property ButtonHint: TTranslateString read GetBuddyHint write SetBuddyHint;
    property ButtonOnlyWhenFocused: Boolean read FButtonOnlyWhenFocused write SetButtonOnlyWhenFocused default False;
    property ButtonWidth: Integer read GetBuddyWidth write SetBuddyWidth;
    property Edit: TEdit read GetEdit;
    property Flat: Boolean read FFlat write SetFlat default False;
    property FocusOnButtonClick: Boolean read GetFocusOnButtonClick write SetFocusOnButtonClick default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: Integer read GetNumGlyps write SetNumGlyphs;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth default 0;
    property Spacing default 4;

    property OnButtonClick: TNotifyEvent read GetOnButtonClick write SetOnButtonClick;

  end;



  TDirectoryEdit = class(TCustomEditButton)
  private
    FRootDir: String;
    FShowHidden: Boolean;
    FDialogOptions: TOpenOptions;
    FOnAcceptDir: TAcceptFileNameEvent;
    FDialogTitle: String;
    function GetDirectory: String;
    procedure SetDirectory(const Value: String);
  public
    property AutoSelected;
    constructor Create(AOwner: TComponent); override;
    procedure RunDialog; virtual;
  published
    // TDirectory properties.
    property Directory: String read GetDirectory write SetDirectory;
    property RootDir: String read FRootDir write FRootDir;
    property OnAcceptDirectory: TAcceptFileNameEvent read FOnAcceptDir write FonAcceptDir;
    property DialogTitle: String read FDialogTitle write FDialogTitle;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions;// default DefaultOpenDialogOptions;
    property ShowHidden: Boolean read FShowHidden write FShowHidden;
    // TEditButton properties.
    property ButtonCaption;
    property ButtonCursor;
    property ButtonHint;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Constraints;
    property DirectInput;
    property Glyph;
    property NumGlyphs;
    property Images;
    property ImageIndex;
    property ImageWidth;
    property Flat;
    property FocusOnButtonClick;
    // Other properties
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
//    property BorderSpacing;
//    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Layout;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property Spacing;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
//    property OnUTF8KeyPress;
    property Text;
    property TextHint;
  end;

  TColorButton = class(TSpeedButton)
  private
    FBorderWidth: Integer;
    FButtonColorAutoSize: Boolean;
    FButtonColorSize: Integer;
    FButtonColor: TColor;
    FColorDialog: TColorDialog;
    FOnColorChanged: TNotifyEvent;
    FDisabledPattern: TBitmap;
    FOnPaint: TNotifyEvent;
    function IsButtonColorAutoSizeStored: boolean;
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetButtonColorAutoSize(const AValue: Boolean);
    procedure SetButtonColorSize(const AValue: Integer);
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
      BiDiFlags: Longint): TRect;
    class function GetControlClassDefaultSize: TSize; static;
    function GetDisabledPattern: TBitmap;
    function GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure ShowColorDialog;
    class procedure WSRegisterClass; static;
  protected
//    class procedure WSRegisterClass; override;
//    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
//      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; override;
//    function GetDisabledPattern: TBitmap; virtual;
//    function GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize; override;
//    class function GetControlClassDefaultSize: TSize; override;
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//    procedure ShowColorDialog; virtual;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; Override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property Anchors;
    property AllowAllUp;
//    property BorderSpacing;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property ButtonColorAutoSize: Boolean read FButtonColorAutoSize  write SetButtonColorAutoSize stored IsButtonColorAutoSizeStored;
    property ButtonColorSize: Integer read FButtonColorSize write SetButtonColorSize;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property GroupIndex;
    property Hint;
    property Layout;
    property Margin;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnColorChanged: TNotifyEvent read FOnColorChanged
                                          write FOnColorChanged;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnResize;
//    property OnChangeBounds;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;


  TCheckItemChange = procedure(Sender: TObject; AIndex: Integer) of object;


  TCustomCheckCombo = class(Vcl.StdCtrls.TCustomComboBox)
  private
    FAllowGrayed: Boolean;
    FOnItemChange: TCheckItemChange;
//    FOnItemChange: TCheckItemChange;
//    procedure AsyncCheckItemStates(Data: PtrInt);
    function GetChecked(AIndex: Integer): Boolean;
    function GetCount: Integer;
    function GetItemEnabled(AIndex: Integer): Boolean;
    function GetObject(AIndex: Integer): TObject;
    function GetState(AIndex: Integer): TCheckBoxState;
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetItemEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetObject(AIndex: Integer; AValue: TObject);
    procedure SetState(AIndex: Integer; AValue: TCheckBoxState);
  protected
    FCheckHighlight: Boolean;
    FCheckSize: TSize;
    FDropped: Boolean;
    FHilightedIndex: Integer;
    FHiLiteLeft: Integer;
    FHiLiteRight: Integer;
    FNeedMeasure: Boolean;
    FRejectDropDown: Boolean;
    FRejectToggleOnSelect: Boolean;
    FRightToLeft: Boolean;
    FTextHeight: SmallInt;
//    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
//    procedure ClearItemStates;
//    procedure CloseUp; override;
//    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
//    procedure DropDown; override;
//    procedure FontChanged(Sender: TObject); override;
//    procedure InitializeWnd; override;
//    procedure InitItemStates;
//    procedure CheckItemStates;
//    procedure QueueCheckItemStates;
//    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
//    procedure Loaded; override;
//    procedure MouseLeave; override;
//    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
//    procedure SetItemHeight(const AValue: Integer); override;
//    procedure SetItems(const Value: TStrings); override;
//    procedure Select; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const AItem: string; AState: TCheckBoxState; AEnabled: Boolean = True); reintroduce;
    procedure AssignItems(AItems: TStrings);
    procedure Clear; override;
    procedure DeleteItem(AIndex: Integer);
    procedure CheckAll(AState: TCheckBoxState; AAllowGrayed: Boolean = True; AAllowDisabled: Boolean = True);
    procedure Toggle(AIndex: Integer);
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Count: Integer read GetCount;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property Objects[AIndex: Integer]: TObject read GetObject write SetObject;
    property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
    property OnItemChange: TCheckItemChange read FOnItemChange write FOnItemChange;
  end;

  TCheckComboBox = class(TCustomCheckCombo)
  private
    function GetItemWidth: Integer;
    procedure SetItemWidth(const Value: Integer);
  published
    property Align;
    property AllowGrayed;
    property Anchors;
//    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BidiMode;
//    property BorderSpacing;
//    property BorderStyle;
    property Color;
    property Constraints;
    property Count;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth: Integer read GetItemWidth write SetItemWidth ;
    property MaxLength;
    property OnChange;
//    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
//    property OnEditingDone;
    property OnEnter;
    property OnExit;
//    property OnGetItems;
    property OnItemChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnSelect;
//    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
  end;

  TComboBox = class(Vcl.StdCtrls.TComboBox)
  private
    FReadOnly: Boolean;
  published
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;


  TWrapAfter = (
    waAuto,    // auto
    waForce,   // always wrap after this control
    waAvoid,   // try not to wrap after this control, if the control is already at the beginning of the row, wrap though
    waForbid); // never wrap after this control

  TFlowPanelControl = class(TCollectionItem)
  private
    FControl: TControl;
    FWrapAfter: TWrapAfter;
    procedure SetControl(const aControl: TControl);
    procedure SetWrapAfter(const AWrapAfter: TWrapAfter);
  protected
    function GetDisplayName: String; override;
    procedure SetIndex(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
//    function FPCollection: TFlowPanelControlList;
    function FPOwner: TCustomFlowPanel;
  public
    // These methods are used by the Object Inspector only
    function AllowAdd: Boolean;
    function AllowDelete: Boolean;
  published
    property Control: TControl read FControl write SetControl;
    property WrapAfter: TWrapAfter read FWrapAfter write SetWrapAfter;
    property Index;
  end;

  TFlowPanelControlList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TFlowPanelControl;
    procedure SetItem(Index: Integer; const Value: TFlowPanelControl);
  public
    function IndexOf(AControl: TControl): Integer;
    property Items[Index: Integer]: TFlowPanelControl read GetItem write SetItem; default;

    procedure Exchange(Const Index1, index2: integer);
    procedure Move(Const Index1, index2: integer);
    // These methods are used by the Object Inspector only
    function AllowAdd: Boolean;
    function AllowDelete: Boolean;
  end;

  TFlowPanel = class(Vcl.ExtCtrls.TFlowPanel)
  private
    FControlList: TFlowPanelControlList;
    procedure SetControlList(const Value: TFlowPanelControlList);
  published
    property ControlList: TFlowPanelControlList read FControlList write SetControlList;
  end;


procedure AddControlDefaultMethodsRtti(AC: TClass; var ADest: TArray<TRttiMethod>);
procedure AddControlDefaultPropsRtti(AC: TClass; var ADest: TArray<TRttiProperty>);
procedure AddControlDefaultIndexPropsRtti(AC: TClass; var ADest: TArray<TRttiIndexedProperty>);


procedure AddWinControlDefaultMethodsRtti(AC: TClass; var ADest: TArray<TRttiMethod>);
procedure AddWinControlDefaultPropsRtti(AC: TClass; var ADest: TArray<TRttiProperty>);
procedure AddWinControlDefaultIndexPropsRtti(AC: TClass; var ADest: TArray<TRttiIndexedProperty>);
implementation




type

  TLayoutAdjustmentPolicy = (
    lapDefault,     // widgetset dependent
    lapFixedLayout, // A fixed absolute layout in all platforms
    lapAutoAdjustWithoutHorizontalScrolling, // Smartphone platforms use this one,
                                             // the x axis is stretched to fill the screen and
                                             // the y is scaled to fit the DPI
    lapAutoAdjustForDPI // For desktops using High DPI, scale x and y to fit the DPI
  );

  TControlDefault = class(TObject)
  private
    FAnchorSideTop: TAnchorSide;
    FAnchorSideLeft: TAnchorSide;
    FBorderSpacing: TControlBorderSpacing;
    FAnchorSideRight: TAnchorSide;
    FClientOrigin: TPoint;
    FAnchorSideBottom: TAnchorSide;
    FChildSizing: TControlChildSizing;
    function GetAnchorSide(AKind: TAnchorKind): TAnchorSide;
  public
    property  AnchorSide[Kind: TAnchorKind]: TAnchorSide read GetAnchorSide;

    procedure AnchorToNeighbour(ASide: TAnchorKind; ASpace: TSpacingSize; ASibling: TControl);
    procedure AnchorParallel(ASide: TAnchorKind; ASpace: TSpacingSize; ASibling: TControl);
    procedure AnchorHorizontalCenterTo(ASibling: TControl);
    procedure AnchorVerticalCenterTo(ASibling: TControl);
//    procedure AnchorToCompanion(Side: TAnchorKind; Space: TSpacingSize; Sibling: TControl; FreeCompositeSide: boolean = true);
    procedure AnchorSame(ASide: TAnchorKind; ASibling: TControl);
    procedure AnchorAsAlign(ATheAlign: TAlign; ASpace: TSpacingSize);
    procedure AnchorClient(ASpace: TSpacingSize);

//    procedure Click; virtual;
//    procedure DblClick; virtual;

    //scale support
    function ScaleDesignToForm(const ASize: Integer): Integer;
    function ScaleFormToDesign(const ASize: Integer): Integer;
    function Scale96ToForm(const ASize: Integer): Integer;
    function ScaleFormTo96(const ASize: Integer): Integer;
    function Scale96ToFont(const ASize: Integer): Integer;
    function ScaleFontTo96(const ASize: Integer): Integer;
    function ScaleScreenToFont(const ASize: Integer): Integer;
    function ScaleFontToScreen(const ASize: Integer): Integer;
    function Scale96ToScreen(const ASize: Integer): Integer;
    function ScaleScreenTo96(const ASize: Integer): Integer;

    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy; const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer); virtual;
//    procedure ShouldAutoAdjust(var AWidth, AHeight: Boolean); virtual;
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); virtual;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); virtual;

  published
  {$M+}
    property AnchorSideLeft: TAnchorSide index akLeft read FAnchorSideLeft write FAnchorSideLeft;
    property AnchorSideTop: TAnchorSide index akTop read FAnchorSideTop write FAnchorSideTop;
    property AnchorSideRight: TAnchorSide index akRight read FAnchorSideRight write FAnchorSideRight;
    property AnchorSideBottom: TAnchorSide index akBottom read FAnchorSideBottom write FAnchorSideBottom;
    property ClientOrigin: TPoint read FClientOrigin;
    property ChildSizing: TControlChildSizing read FChildSizing write FChildSizing;
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write FBorderSpacing;
  {$M-}
  end;

  TWinControlDefault = class(TObject)
  public
//    procedure PaintTo(DC: HDC; X, Y: Integer);
  end;



var
  controlDefMethods: TArray<TRttiMethod>;
  controlDefProps: TArray<TRttiProperty>;
  controlDefIndexProps: TArray<TRttiIndexedProperty>;
  controlContext: TRttiContext;

  winControlDefMethods: TArray<TRttiMethod>;
  winControlDefProps: TArray<TRttiProperty>;
  winControlDefIndexProps: TArray<TRttiIndexedProperty>;
  winControlContext: TRttiContext;

procedure InitDefaultControlRtti;
var

  LInst: TRttiInstanceType;//TRttiInstanceType;

  LM: TRttiMethod;
  LProp: TRttiProperty;
  LIdxProp: TRttiIndexedProperty;
begin
  controlContext := TRttiContext.Create;
  LInst := controlContext.GetType(TypeInfo(TControlDefault)).AsInstance;

  for LM in Linst.GetMethods do
  begin
    SetLength(controlDefMethods, Length(controlDefMethods)+1);
    controlDefMethods[High(controlDefMethods)] := LM;
  end;

  for LProp in Linst.GetProperties do
  begin
    SetLength(controlDefProps, Length(controlDefProps)+1);
    controlDefProps[High(controlDefProps)] := LProp;
  end;

  for LIdxProp in Linst.GetIndexedProperties do
  begin
    SetLength(controlDefIndexProps, Length(controlDefIndexProps)+1);
    controlDefIndexProps[High(controlDefIndexProps)] := LIdxProp;
  end;

end;



procedure AddControlDefaultMethodsRtti(AC: TClass; var ADest: TArray<TRttiMethod>);
var
  LM: TRttiMethod;
begin
  if not AC.InheritsFrom(TControl) then
    Exit;

  for LM in controlDefMethods do
  begin

    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LM;
  end;

end;

procedure AddControlDefaultPropsRtti(AC: TClass; var ADest: TArray<TRttiProperty>);
var
  LProp: TRttiProperty;
begin
  if not AC.InheritsFrom(TControl) then
    Exit;

  for LProp in controlDefProps do
  begin
    if LProp.Name = 'ChildSizing' then
    begin
      if (not AC.InheritsFrom(TWinControl)) or AC.InheritsFrom(TLinkLabel) then
        Continue;
    end;

    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LProp;
  end;

end;


procedure AddControlDefaultIndexPropsRtti(AC: TClass; var ADest: TArray<TRttiIndexedProperty>);
var
  LIdxProp: TRttiIndexedProperty;
begin
  if not AC.InheritsFrom(TControl) then
    Exit;

  for LIdxProp in controlDefIndexProps do
  begin
    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LIdxProp;
  end;
end;



//------------------------
procedure InitDefaultWinControlRtti;
var

  LInst: TRttiInstanceType;//TRttiInstanceType;

  LM: TRttiMethod;
  LProp: TRttiProperty;
  LIdxProp: TRttiIndexedProperty;
begin
  winControlContext := TRttiContext.Create;
  LInst := winControlContext.GetType(TypeInfo(TWinControlDefault)).AsInstance;

  for LM in Linst.GetMethods do
  begin
    SetLength(winControlDefMethods, Length(winControlDefMethods)+1);
    winControlDefMethods[High(winControlDefMethods)] := LM;
  end;

  for LProp in Linst.GetProperties do
  begin
    SetLength(winControlDefProps, Length(winControlDefProps)+1);
    winControlDefProps[High(winControlDefProps)] := LProp;
  end;

  for LIdxProp in Linst.GetIndexedProperties do
  begin
    SetLength(winControlDefIndexProps, Length(winControlDefIndexProps)+1);
    winControlDefIndexProps[High(winControlDefIndexProps)] := LIdxProp;
  end;

end;



procedure AddWinControlDefaultMethodsRtti(AC: TClass; var ADest: TArray<TRttiMethod>);
var
  LM: TRttiMethod;
begin
  if not AC.InheritsFrom(TWinControl) then
    Exit;

  for LM in winControlDefMethods do
  begin

    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LM;
  end;

end;

procedure AddWinControlDefaultPropsRtti(AC: TClass; var ADest: TArray<TRttiProperty>);
var
  LProp: TRttiProperty;
begin
  if not AC.InheritsFrom(TWinControl) then
    Exit;

  for LProp in winControlDefProps do
  begin
    if LProp.Name = 'ChildSizing' then
    begin
      if (not AC.InheritsFrom(TWinControl)) or AC.InheritsFrom(TLinkLabel) then
        Continue;
    end;

    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LProp;
  end;

end;


procedure AddWinControlDefaultIndexPropsRtti(AC: TClass; var ADest: TArray<TRttiIndexedProperty>);
var
  LIdxProp: TRttiIndexedProperty;
begin
  if not AC.InheritsFrom(TWinControl) then
    Exit;

  for LIdxProp in winControlDefIndexProps do
  begin
    SetLength(ADest, Length(ADest)+1);
    ADest[High(ADest)] := LIdxProp;
  end;
end;

{ TAnchorSide }


{ TAnchorSide }

procedure TAnchorSide.Assign(Source: TPersistent);
begin
  inherited;
end;

procedure TAnchorSide.FixCenterAnchoring;
begin

end;

function TAnchorSide.IsAnchoredToParent(ParentSide: TAnchorKind): boolean;
begin
  Result := False;
end;

{ TControlDefault }

procedure TControlDefault.AnchorAsAlign(ATheAlign: TAlign; ASpace: TSpacingSize);
begin

end;

procedure TControlDefault.AnchorClient(ASpace: TSpacingSize);
begin

end;

procedure TControlDefault.AnchorHorizontalCenterTo(ASibling: TControl);
begin

end;

procedure TControlDefault.AnchorParallel(ASide: TAnchorKind; ASpace: TSpacingSize;
  ASibling: TControl);
begin

end;

procedure TControlDefault.AnchorSame(ASide: TAnchorKind; ASibling: TControl);
begin

end;

procedure TControlDefault.AnchorToNeighbour(ASide: TAnchorKind;
  ASpace: TSpacingSize; ASibling: TControl);
begin

end;

procedure TControlDefault.AnchorVerticalCenterTo(ASibling: TControl);
begin

end;

procedure TControlDefault.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin

end;

procedure TControlDefault.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin

end;

function TControlDefault.GetAnchorSide(AKind: TAnchorKind): TAnchorSide;
begin

end;


function TControlDefault.Scale96ToFont(const ASize: Integer): Integer;
begin

end;

function TControlDefault.Scale96ToForm(const ASize: Integer): Integer;
begin

end;

function TControlDefault.Scale96ToScreen(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleDesignToForm(const ASize: Integer): Integer;
begin

end;

procedure TControlDefault.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin

end;

function TControlDefault.ScaleFontTo96(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleFontToScreen(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleFormTo96(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleFormToDesign(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleScreenTo96(const ASize: Integer): Integer;
begin

end;

function TControlDefault.ScaleScreenToFont(const ASize: Integer): Integer;
begin

end;

//procedure TControlDefault.ShouldAutoAdjust(var AWidth, AHeight: Boolean);
//begin
//
//end;

{ TControlBorderSpacing }

function TControlBorderSpacing.GetSpace(Kind: TAnchorKind): integer;
begin

end;

procedure TControlBorderSpacing.SetSpace(Kind: TAnchorKind;
  const Value: integer);
begin

end;

{ TSelectDirectoryDialog }

constructor TSelectDirectoryDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

{ TCustomCheckGroup }

constructor TCustomCheckGroup.Create(TheOwner: TComponent);
begin
  inherited;

end;

destructor TCustomCheckGroup.Destroy;
begin

  inherited;
end;

procedure TCustomCheckGroup.FlipChildren(AllLevels: Boolean);
begin
  inherited;

end;

function TCustomCheckGroup.GetChecked(Index: integer): boolean;
begin

end;

function TCustomCheckGroup.GetCheckEnabled(Index: integer): boolean;
begin

end;

function TCustomCheckGroup.Rows: integer;
begin

end;

procedure TCustomCheckGroup.SetAutoFill(const Value: Boolean);
begin
  FAutoFill := Value;
end;

procedure TCustomCheckGroup.SetChecked(Index: integer; const Value: boolean);
begin

end;

procedure TCustomCheckGroup.SetCheckEnabled(Index: integer;
  const Value: boolean);
begin

end;

procedure TCustomCheckGroup.SetColumnLayout(const Value: TColumnLayout);
begin
  FColumnLayout := Value;
end;

procedure TCustomCheckGroup.SetColumns(const Value: integer);
begin
  FColumns := Value;
end;

procedure TCustomCheckGroup.SetItems(const Value: TStrings);
begin
  FItems := Value;
end;

procedure TCustomCheckGroup.SetParentBackground(const Value: Boolean);
begin
  FParentBackground := Value;
end;

{ TToggleBox }

constructor TToggleBox.Create(TheOwner: TComponent);
begin
  inherited;

end;


{ TForm }

class function TForm.Create2(AOwner: TComponent; AInitScale: Boolean): TGoForm;
begin

end;

procedure TForm.EnabledMaximize(AValue: Boolean);
begin

end;

procedure TForm.EnabledMinimize(AValue: Boolean);
begin

end;

procedure TForm.EnabledSystemMenu(AValue: Boolean);
begin

end;


procedure TForm.InheritedWndProc(var TheMessage: TLMessage);
begin

end;

procedure TForm.ScaleForCurrentDpi;
begin
  inherited;

end;

procedure TForm.ScaleForPPI(ANewPPI: Integer);
begin

end;

procedure TForm.ScreenCenter;
begin

end;

procedure TForm.WorkAreaCenter;
begin

end;

{ TListItem }

function TListItem.DisplayRectSubItem(subItem: integer;
  Code: TDisplayCode): TRect;
begin

end;

{ TMemo }

procedure TMemo.Append(const Value: String);
begin

end;

{ TGridColumns }

function TGridColumns.Add: TGridColumn;
begin

end;

procedure TGridColumns.Clear;
begin

end;

constructor TGridColumns.Create(AGrid: TCustomGrid;
  aItemClass: TCollectionItemClass);
begin

end;

procedure TGridColumns.ExchangeColumn(Index, WithIndex: Integer);
begin

end;

procedure TGridColumns.FontChanged;
begin

end;

function TGridColumns.GetColumn(Index: Integer): TGridColumn;
begin

end;

function TGridColumns.GetEnabled: Boolean;
begin

end;

function TGridColumns.GetOwner: TPersistent;
begin

end;

function TGridColumns.GetVisibleCount: Integer;
begin

end;

function TGridColumns.HasIndex(Index: Integer): boolean;
begin

end;

function TGridColumns.IndexOf(Column: TGridColumn): Integer;
begin

end;

procedure TGridColumns.InsertColumn(Index: Integer);
begin

end;

function TGridColumns.IsDefault: boolean;
begin

end;

procedure TGridColumns.MoveColumn(FromIndex, ToIndex: Integer);
begin

end;

function TGridColumns.RealIndex(Index: Integer): Integer;
begin

end;

procedure TGridColumns.RemoveColumn(Index: Integer);
begin

end;

procedure TGridColumns.SetColumn(Index: Integer; Value: TGridColumn);
begin

end;

procedure TGridColumns.TitleFontChanged;
begin

end;

procedure TGridColumns.Update(Item: TCollectionItem);
begin
  inherited;

end;

function TGridColumns.VisibleIndex(Index: Integer): Integer;
begin

end;

{ TGridColumnTitle }

procedure TGridColumnTitle.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TGridColumnTitle.Create(TheColumn: TGridColumn);
begin

end;

procedure TGridColumnTitle.DefineProperties(Filer: TFiler);
begin
  inherited;

end;

destructor TGridColumnTitle.Destroy;
begin

  inherited;
end;

procedure TGridColumnTitle.FillTitleDefaultFont;
begin

end;

procedure TGridColumnTitle.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin

end;

procedure TGridColumnTitle.FontChanged(Sender: TObject);
begin

end;

function TGridColumnTitle.GetAlignment: TAlignment;
begin

end;

function TGridColumnTitle.GetCaption: TCaption;
begin

end;

function TGridColumnTitle.GetColor: TColor;
begin

end;

function TGridColumnTitle.GetDefaultAlignment: TAlignment;
begin

end;

function TGridColumnTitle.GetDefaultCaption: string;
begin

end;

function TGridColumnTitle.GetDefaultColor: TColor;
begin

end;

function TGridColumnTitle.GetDefaultLayout: TTextLayout;
begin

end;

function TGridColumnTitle.GetFont: TFont;
begin

end;

function TGridColumnTitle.GetLayout: TTextLayout;
begin

end;

function TGridColumnTitle.GetOwner: TPersistent;
begin

end;

function TGridColumnTitle.IsAlignmentStored: boolean;
begin

end;

function TGridColumnTitle.IsCaptionStored: boolean;
begin

end;

function TGridColumnTitle.IsColorStored: boolean;
begin

end;

function TGridColumnTitle.IsDefault: boolean;
begin

end;

function TGridColumnTitle.IsFontStored: boolean;
begin

end;

function TGridColumnTitle.IsLayoutStored: boolean;
begin

end;

procedure TGridColumnTitle.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin

end;

procedure TGridColumnTitle.SetAlignment(const AValue: TAlignment);
begin

end;

procedure TGridColumnTitle.SetCaption(const AValue: TCaption);
begin

end;

procedure TGridColumnTitle.SetColor(const AValue: TColor);
begin

end;

procedure TGridColumnTitle.SetFont(const AValue: TFont);
begin

end;

procedure TGridColumnTitle.SetImageIndex(const AValue: TImageIndex);
begin

end;

procedure TGridColumnTitle.SetImageLayout(const AValue: TButtonLayout);
begin

end;

procedure TGridColumnTitle.SetLayout(const AValue: TTextLayout);
begin

end;

procedure TGridColumnTitle.SetMultiLine(const AValue: Boolean);
begin

end;

procedure TGridColumnTitle.SetPrefixOption(const AValue: TPrefixOption);
begin

end;

procedure TGridColumnTitle.WriteCaption(Writer: TWriter);
begin

end;

{ TGridColumn }

procedure TGridColumn.AllColumnsChange;
begin

end;

procedure TGridColumn.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TGridColumn.ColumnChanged;
begin

end;

constructor TGridColumn.Create(ACollection: TCollection);
begin
  inherited;

end;

function TGridColumn.CreateTitle: TGridColumnTitle;
begin

end;

destructor TGridColumn.Destroy;
begin

  inherited;
end;

procedure TGridColumn.FillDefaultFont;
begin

end;

procedure TGridColumn.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin

end;

procedure TGridColumn.FontChanged(Sender: TObject);
begin

end;

function TGridColumn.GetAlignment: TAlignment;
begin

end;

function TGridColumn.GetColor: TColor;
begin

end;

function TGridColumn.GetDefaultAlignment: TAlignment;
begin

end;

function TGridColumn.GetDefaultColor: TColor;
begin

end;

function TGridColumn.GetDefaultLayout: TTextLayout;
begin

end;

function TGridColumn.GetDefaultMaxSize: Integer;
begin

end;

function TGridColumn.GetDefaultMinSize: Integer;
begin

end;

function TGridColumn.GetDefaultReadOnly: boolean;
begin

end;

function TGridColumn.GetDefaultSizePriority: Integer;
begin

end;

function TGridColumn.GetDefaultValueChecked: string;
begin

end;

function TGridColumn.GetDefaultValueUnchecked: string;
begin

end;

function TGridColumn.GetDefaultVisible: boolean;
begin

end;

function TGridColumn.GetDefaultWidth: Integer;
begin

end;

function TGridColumn.GetDisplayName: string;
begin

end;

function TGridColumn.GetExpanded: Boolean;
begin

end;

function TGridColumn.GetFont: TFont;
begin

end;

function TGridColumn.GetGrid: TCustomGrid;
begin

end;

function TGridColumn.GetLayout: TTextLayout;
begin

end;

function TGridColumn.GetMaxSize: Integer;
begin

end;

function TGridColumn.GetMinSize: Integer;
begin

end;

function TGridColumn.GetPickList: TStrings;
begin

end;

function TGridColumn.GetReadOnly: Boolean;
begin

end;

function TGridColumn.GetSizePriority: Integer;
begin

end;

function TGridColumn.GetStoredWidth: Integer;
begin

end;

function TGridColumn.GetValueChecked: string;
begin

end;

function TGridColumn.GetValueUnchecked: string;
begin

end;

function TGridColumn.GetVisible: Boolean;
begin

end;

function TGridColumn.GetWidth: Integer;
begin

end;

function TGridColumn.IsAlignmentStored: boolean;
begin

end;

function TGridColumn.IsColorStored: boolean;
begin

end;

function TGridColumn.IsDefault: boolean;
begin

end;

function TGridColumn.IsFontStored: boolean;
begin

end;

function TGridColumn.IsLayoutStored: boolean;
begin

end;

function TGridColumn.IsMaxSizeStored: boolean;
begin

end;

function TGridColumn.IsMinSizeStored: boolean;
begin

end;

function TGridColumn.IsReadOnlyStored: boolean;
begin

end;

function TGridColumn.IsSizePriorityStored: boolean;
begin

end;

function TGridColumn.IsValueCheckedStored: boolean;
begin

end;

function TGridColumn.IsValueUncheckedStored: boolean;
begin

end;

function TGridColumn.IsVisibleStored: boolean;
begin

end;

function TGridColumn.IsWidthStored: boolean;
begin

end;

procedure TGridColumn.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin

end;

procedure TGridColumn.SetAlignment(const AValue: TAlignment);
begin

end;

procedure TGridColumn.SetButtonStyle(const AValue: TColumnButtonStyle);
begin

end;

procedure TGridColumn.SetColor(const AValue: TColor);
begin

end;

procedure TGridColumn.SetExpanded(const AValue: Boolean);
begin

end;

procedure TGridColumn.SetFont(const AValue: TFont);
begin

end;

procedure TGridColumn.SetIndex(Value: Integer);
begin
  inherited;

end;

procedure TGridColumn.SetLayout(const AValue: TTextLayout);
begin

end;

procedure TGridColumn.SetMaxSize(const AValue: Integer);
begin

end;

procedure TGridColumn.SetMinSize(const Avalue: Integer);
begin

end;

procedure TGridColumn.SetPickList(const AValue: TStrings);
begin

end;

procedure TGridColumn.SetReadOnly(const AValue: Boolean);
begin

end;

procedure TGridColumn.SetSizePriority(const AValue: Integer);
begin

end;

procedure TGridColumn.SetTitle(const AValue: TGridColumnTitle);
begin

end;

procedure TGridColumn.SetValueChecked(const AValue: string);
begin

end;

procedure TGridColumn.SetValueUnchecked(const AValue: string);
begin

end;

procedure TGridColumn.SetVisible(const AValue: Boolean);
begin

end;

procedure TGridColumn.SetWidth(const AValue: Integer);
begin

end;

{ TStringGrid }

procedure TStringGrid.AutoAdjustColumns;
begin

end;

function TStringGrid.CellRect(ACol, ARow: Integer): TRect;
begin

end;

function TStringGrid.CellToGridZone(aCol, aRow: Integer): TGridZone;
begin

end;

procedure TStringGrid.CheckPosition;
begin

end;

procedure TStringGrid.Clear;
begin

end;

function TStringGrid.ClearCols: Boolean;
begin

end;

function TStringGrid.ClearRows: Boolean;
begin

end;

procedure TStringGrid.ClearSelections;
begin

end;

procedure TStringGrid.DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
  aState: TGridDrawState);
begin

end;

procedure TStringGrid.DeleteCol(Index: Integer);
begin

end;

procedure TStringGrid.DeleteColRow(IsColumn: Boolean; index: Integer);
begin

end;

procedure TStringGrid.DeleteRow(Index: Integer);
begin

end;

procedure TStringGrid.EditingDone;
begin

end;

function TStringGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;
begin

end;

procedure TStringGrid.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TStringGrid.EditorKeyPress(Sender: TObject; var Key: Char);
begin

end;

procedure TStringGrid.EditorKeyUp(Sender: TObject; var key: Word;
  shift: TShiftState);
begin

end;

procedure TStringGrid.EditorSetMode(const Value: Boolean);
begin
  FEditorMode := Value;
end;

procedure TStringGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin

end;

procedure TStringGrid.ExchangeColRow(IsColumn: Boolean; index,
  WithIndex: Integer);
begin

end;

function TStringGrid.GetIsCellSelected(aCol, aRow: Integer): boolean;
begin

end;

function TStringGrid.HasMultiSelection: Boolean;
begin

end;

procedure TStringGrid.InsertColRow(IsColumn: boolean; index: integer);
begin

end;

procedure TStringGrid.InvalidateCell(aCol, aRow: Integer);
begin

end;

procedure TStringGrid.InvalidateCol(ACol: Integer);
begin

end;

procedure TStringGrid.InvalidateRange(const aRange: TRect);
begin

end;

procedure TStringGrid.InvalidateRow(ARow: Integer);
begin

end;

function TStringGrid.IsCellVisible(aCol, aRow: Integer): Boolean;
begin

end;

function TStringGrid.IsFixedCellVisible(aCol, aRow: Integer): boolean;
begin

end;

//procedure TStringGrid.LoadFromFile(FileName: string);
//begin
//
//end;
//
//procedure TStringGrid.LoadFromStream(AStream: TStream);
//begin
//
//end;

function TStringGrid.MouseCoord(X, Y: Integer): TGridCoord;
begin

end;

//procedure TStringGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
//begin
//
//end;

function TStringGrid.MouseToCell(const Mouse: TPoint): TPoint;
begin

end;

function TStringGrid.MouseToGridZone(X, Y: Integer): TGridZone;
begin

end;

function TStringGrid.MouseToLogcell(Mouse: TPoint): TPoint;
begin

end;

procedure TStringGrid.MoveColRow(IsColumn: Boolean; FromIndex,
  ToIndex: Integer);
begin

end;

procedure TStringGrid.SetFixedGridLineColor(const Value: TColor);
begin
  FFixedGridLineColor := Value;
end;

procedure TStringGrid.SetFocusColor(const Value: TColor);
begin
  FFocusColor := Value;
end;

procedure TStringGrid.SetOptions2(const Value: TGridOptions2);
begin
  FOptions2 := Value;
end;

procedure TStringGrid.SetRangeSelectMode(const Value: TRangeSelectMode);
begin
  FRangeSelectMode := Value;
end;

procedure TStringGrid.SetUseXorFeatures(const Value: boolean);
begin
  FUseXORFeatures := Value;
end;

procedure TStringGrid.SortColRow(IsColumn: Boolean; Index, FromIndex,
  ToIndex: Integer);
begin

end;

//procedure TStringGrid.SortColRow(IsColumn: Boolean; index: Integer);
//begin
//
//end;

{ TDateTimePicker }



{ TDateTimePicker }

function TDateTimePicker.DateIsNull: Boolean;
begin

end;

procedure TDateTimePicker.SelectDate;
begin

end;

procedure TDateTimePicker.SelectTime;
begin

end;

//procedure TDateTimePicker.SendExternalKey(const aKey: Char);
//begin
//
//end;
//
//procedure TDateTimePicker.SendExternalKeyCode(const Key: Word);
//begin
//
//end;

{ TMonthCalendar }

function TMonthCalendar.GetDateTime: TDateTime;
begin
  Result := FDateTime;
end;

{ TClipboard }

function TClipboard.FindFormatID(const FormatName: string): TClipboardFormat;
begin

end;

function TClipboard.FindPictureFormatID: TClipboardFormat;
begin
  Result := 0;
end;

function TClipboard.GetAsHtml(ExtractFragmentOnly: Boolean): string;
begin
   Result := '';
end;

function TClipboard.GetFormat(FormatID: TClipboardFormat;
  Stream: TStream): Boolean;
begin

end;

function TClipboard.GetFormats(Index: Integer): TClipboardFormat;
begin
  Result := 0;
end;

function TClipboard.HasFormat(FormatID: TClipboardFormat): Boolean;
begin
  Result := False;
end;

function TClipboard.HasFormatName(const FormatName: string): Boolean;
begin

end;

function TClipboard.HasPictureFormat: boolean;
begin

end;

procedure TClipboard.SetAsHtml(Html: String; const PlainText: String);
begin

end;

procedure TClipboard.SetFormats(Index: Integer; const Value: TClipboardFormat);
begin

end;

procedure TClipboard.SupportedFormats(List: TStrings);
begin

end;

{ TImageList }



{ TWinControlDefault }

//procedure TWinControlDefault.PaintTo(DC: HDC; X, Y: Integer);
//begin
//
//end;

{ TListView }





{ TListView }

//function TListView.GetNextItem(StartItem: TListItem;
//  Direction: TSearchDirection; States: TListItemStates): TListItem;
//begin
//
//end;

{ TBitmap }

procedure TBitmap.BeginUpdate(ACanvasOnly: Boolean);
begin

end;

procedure TBitmap.Clear;
begin

end;

procedure TBitmap.EndUpdate(AStreamIsValid: Boolean);
begin

end;

procedure TBitmap.LoadFromDevice(ADc: HDC);
begin

end;

{ TImageList }

function TImageList.Add(Image, Mask: TCustomBitmap): Integer;
begin

end;

function TImageList.AddLazarusResource(const ResourceName: string;
  MaskColor: TColor): integer;
begin

end;

function TImageList.AddMultipleResolutions(
  Images: array of TCustomBitmap): Integer;
begin

end;

function TImageList.AddResourceName(Instance: THandle;
  const ResourceName: string; MaskColor: TColor): integer;
begin

end;

function TImageList.AddSlice(Image: TCustomBitmap; AImageRect: TRect): Integer;
begin

end;

function TImageList.AddSliceCentered(Image: TCustomBitmap): Integer;
begin

end;

function TImageList.AddSliced(Image: TCustomBitmap; AHorizontalCount,
  AVerticalCount: Integer): Integer;
begin

end;

procedure TImageList.DeleteResolution(const AWidth: Integer);
begin

end;

procedure TImageList.GetBitmap(Index: Integer; Image: TCustomBitmap;
  AEffect: TGraphicsDrawEffect);
begin

end;

procedure TImageList.GetFullBitmap(Image: TCustomBitmap;
  AEffect: TGraphicsDrawEffect);
begin

end;

function TImageList.GetHeightForPPI(AImageWidth, APPI: Integer): Integer;
begin

end;

function TImageList.GetHeightForWidth(AWidth: Integer): Integer;
begin

end;

function TImageList.GetSizeForPPI(AImageWidth, APPI: Integer): TSize;
begin

end;

function TImageList.GetWidthForPPI(AImageWidth, APPI: Integer): Integer;
begin

end;

procedure TImageList.Insert(AIndex: Integer; AImage, AMask: TCustomBitmap);
begin

end;

procedure TImageList.InsertMasked(Index: Integer; AImage: TCustomBitmap;
  MaskColor: TColor);
begin

end;

procedure TImageList.RegisterResolutions(
  const AResolutionWidths: array of Integer);
begin

end;

procedure TImageList.Replace(AIndex: Integer; AImage, AMask: TCustomBitmap;
  const AllResolutions: Boolean);
begin

end;

procedure TImageList.ReplaceIcon(AIndex: Integer; AIcon: TIcon);
begin

end;

procedure TImageList.ReplaceMasked(Index: Integer; NewImage: TCustomBitmap;
  MaskColor: TColor; const AllResolutions: Boolean);
begin

end;

procedure TImageList.StretchDraw(ACanvas: TCanvas; AIndex: Integer;
  ARect: TRect; AEnabled: Boolean);
begin

end;

{ TTreeNode }

function TTreeNode.AlphaSort: Boolean;
begin

end;

procedure TTreeNode.Assign(Source: TPersistent);
begin
  inherited;

end;

function TTreeNode.Bottom: integer;
begin

end;

function TTreeNode.BottomExpanded: integer;
begin

end;

procedure TTreeNode.Collapse(Recurse: Boolean);
begin

end;

procedure TTreeNode.ConsistencyCheck;
begin

end;

function TTreeNode.DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
begin

end;

procedure TTreeNode.Delete;
begin

end;

procedure TTreeNode.DeleteChildren;
begin

end;

function TTreeNode.DisplayExpandSignLeft: integer;
begin

end;

function TTreeNode.DisplayExpandSignRect: TRect;
begin

end;

function TTreeNode.DisplayExpandSignRight: integer;
begin

end;

function TTreeNode.DisplayIconLeft: integer;
begin

end;

function TTreeNode.DisplayRect(TextOnly: Boolean): TRect;
begin

end;

function TTreeNode.DisplayStateIconLeft: integer;
begin

end;

function TTreeNode.DisplayTextLeft: integer;
begin

end;

function TTreeNode.DisplayTextRight: integer;
begin

end;

function TTreeNode.EditText: Boolean;
begin

end;

procedure TTreeNode.EndEdit(Cancel: Boolean);
begin

end;

procedure TTreeNode.Expand(Recurse: Boolean);
begin

end;

procedure TTreeNode.ExpandParents;
begin

end;

function TTreeNode.FindNode(const NodeText: string): TTreeNode;
begin

end;

procedure TTreeNode.FreeAllNodeData;
begin

end;

function TTreeNode.GetAbsoluteIndex: Integer;
begin

end;

function TTreeNode.GetCount: Integer;
begin

end;

function TTreeNode.GetCut: Boolean;
begin

end;

function TTreeNode.GetDeleting: Boolean;
begin

end;

function TTreeNode.GetDropTarget: Boolean;
begin

end;

function TTreeNode.GetExpanded: Boolean;
begin

end;

function TTreeNode.GetFirstChild: TTreeNode;
begin

end;

function TTreeNode.GetFirstVisibleChild: TTreeNode;
begin

end;

function TTreeNode.GetFocused: Boolean;
begin

end;

function TTreeNode.GetHandle: THandle;
begin

end;

function TTreeNode.GetHasChildren: Boolean;
begin

end;

function TTreeNode.GetHeight: integer;
begin

end;

function TTreeNode.GetIndex: Integer;
begin

end;

function TTreeNode.GetItems(ItemIndex: Integer): TTreeNode;
begin

end;

function TTreeNode.GetLastChild: TTreeNode;
begin

end;

function TTreeNode.GetLastSibling: TTreeNode;
begin

end;

function TTreeNode.GetLastSubChild: TTreeNode;
begin

end;

function TTreeNode.GetLastVisibleChild: TTreeNode;
begin

end;

function TTreeNode.GetLevel: Integer;
begin

end;

function TTreeNode.GetMultiSelected: Boolean;
begin

end;

function TTreeNode.GetNext: TTreeNode;
begin

end;

function TTreeNode.GetNextChild(AValue: TTreeNode): TTreeNode;
begin

end;

function TTreeNode.GetNextExpanded: TTreeNode;
begin

end;

function TTreeNode.GetNextMultiSelected: TTreeNode;
begin

end;

function TTreeNode.GetNextSibling: TTreeNode;
begin

end;

function TTreeNode.GetNextSkipChildren: TTreeNode;
begin

end;

function TTreeNode.GetNextVisible: TTreeNode;
begin

end;

function TTreeNode.GetNextVisibleSibling: TTreeNode;
begin

end;

function TTreeNode.GetParentNodeOfAbsoluteLevel(
  TheAbsoluteLevel: integer): TTreeNode;
begin

end;

function TTreeNode.GetPrev: TTreeNode;
begin

end;

function TTreeNode.GetPrevChild(AValue: TTreeNode): TTreeNode;
begin

end;

function TTreeNode.GetPrevExpanded: TTreeNode;
begin

end;

function TTreeNode.GetPrevMultiSelected: TTreeNode;
begin

end;

function TTreeNode.GetPrevSibling: TTreeNode;
begin

end;

function TTreeNode.GetPrevVisible: TTreeNode;
begin

end;

function TTreeNode.GetPrevVisibleSibling: TTreeNode;
begin

end;

function TTreeNode.GetSelected: Boolean;
begin

end;

function TTreeNode.GetTextPath: string;
begin

end;

function TTreeNode.GetTop: integer;
begin

end;

function TTreeNode.GetTreeNodes: TTreeNodes;
begin

end;

function TTreeNode.GetTreeView: TCustomTreeView;
begin

end;

function TTreeNode.GetVisible: Boolean;
begin

end;

function TTreeNode.HasAsParent(AValue: TTreeNode): Boolean;
begin

end;

function TTreeNode.IndexOf(AValue: TTreeNode): Integer;
begin

end;

function TTreeNode.IndexOfText(const NodeText: string): Integer;
begin

end;

function TTreeNode.IsNodeHeightFullVisible: Boolean;
begin

end;

function TTreeNode.IsNodeVisible: Boolean;
begin

end;

procedure TTreeNode.MakeVisible;
begin

end;

procedure TTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
begin

end;

procedure TTreeNode.MultiSelectGroup;
begin

end;

procedure TTreeNode.SetCut(const Value: Boolean);
begin

end;

procedure TTreeNode.SetData(const Value: Pointer);
begin
  FData := Value;
end;

procedure TTreeNode.SetDropTarget(const Value: Boolean);
begin

end;

procedure TTreeNode.SetExpanded(const Value: Boolean);
begin

end;

procedure TTreeNode.SetFocused(const Value: Boolean);
begin

end;

procedure TTreeNode.SetHasChildren(const Value: Boolean);
begin

end;

procedure TTreeNode.SetHeight(const Value: integer);
begin

end;

//procedure TTreeNode.SetImageEffect(const Value: TGraphicsDrawEffect);
//begin
//  FNodeEffect := Value;
//end;

procedure TTreeNode.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
end;

procedure TTreeNode.SetIndex(const Value: Integer);
begin

end;

procedure TTreeNode.SetItems(ItemIndex: Integer; const Value: TTreeNode);
begin

end;

procedure TTreeNode.SetMultiSelected(const Value: Boolean);
begin

end;

procedure TTreeNode.SetOverlayIndex(const Value: Integer);
begin
  FOverlayIndex := Value;
end;

procedure TTreeNode.SetSelected(const Value: Boolean);
begin

end;

procedure TTreeNode.SetSelectedIndex(const Value: Integer);
begin
  FSelectedIndex := Value;
end;

procedure TTreeNode.SetStateIndex(const Value: Integer);
begin
  FStateIndex := Value;
end;

procedure TTreeNode.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TTreeNode.SetVisible(const Value: Boolean);
begin

end;

procedure TTreeNode.Update;
begin

end;

procedure TTreeNode.WriteDebugReport(const Prefix: string; Recurse: boolean);
begin

end;

{ TScreen }

procedure TScreen.BeginTempCursor(const aCursor: TCursor);
begin

end;

procedure TScreen.BeginWaitCursor;
begin

end;

procedure TScreen.EndTempCursor(const aCursor: TCursor);
begin

end;

procedure TScreen.EndWaitCursor;
begin

end;

function TScreen.GetRealCursor: TCursor;
begin

end;

{ TCustomFloatSpinEdit }

constructor TCustomFloatSpinEdit.Create(TheOwner: TComponent);
begin
  inherited;

end;

function TCustomFloatSpinEdit.GetLimitedValue(const AValue: Double): Double;
begin

end;

function TCustomFloatSpinEdit.GetValue: Double;
begin

end;

function TCustomFloatSpinEdit.IncrementStored: Boolean;
begin

end;

function TCustomFloatSpinEdit.MaxValueStored: Boolean;
begin

end;

procedure TCustomFloatSpinEdit.SetDecimals(const Value: Integer);
begin
  FDecimals := Value;
end;

procedure TCustomFloatSpinEdit.SetEditorEnabled(const Value: Boolean);
begin
  FEditorEnabled := Value;
end;

procedure TCustomFloatSpinEdit.SetIncrement(const Value: Double);
begin
  FIncrement := Value;
end;

procedure TCustomFloatSpinEdit.SetMaxValue(const Value: Double);
begin
  FMaxValue := Value;
end;

procedure TCustomFloatSpinEdit.SetMinValue(const Value: Double);
begin
  FMinValue := Value;
end;

procedure TCustomFloatSpinEdit.SetValue(const Value: Double);
begin

end;

procedure TCustomFloatSpinEdit.SetValueEmpty(const Value: Boolean);
begin
  FValueEmpty := Value;
end;

function TCustomFloatSpinEdit.StrToValue(const S: String): Double;
begin

end;

function TCustomFloatSpinEdit.ValueToStr(const AValue: Double): String;
begin

end;

{ TCustomAbstractGroupedEdit }

procedure TCustomAbstractGroupedEdit.Clear;
begin

end;

procedure TCustomAbstractGroupedEdit.ClearSelection;
begin

end;

procedure TCustomAbstractGroupedEdit.CopyToClipboard;
begin

end;

constructor TCustomAbstractGroupedEdit.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TCustomAbstractGroupedEdit.CutToClipboard;
begin

end;

destructor TCustomAbstractGroupedEdit.Destroy;
begin

  inherited;
end;

function TCustomAbstractGroupedEdit.Focused: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetAlignment: TAlignment;
begin

end;

function TCustomAbstractGroupedEdit.GetAutoSelect: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetAutoSelected: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetBuddyCaption: TCaption;
begin

end;

function TCustomAbstractGroupedEdit.GetBuddyCursor: TCursor;
begin

end;

function TCustomAbstractGroupedEdit.GetBuddyHint: TTranslateString;
begin

end;

function TCustomAbstractGroupedEdit.GetBuddyWidth: Integer;
begin

end;

function TCustomAbstractGroupedEdit.GetCanUndo: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetCaretPos: TPoint;
begin

end;

function TCustomAbstractGroupedEdit.GetCharCase: TEditCharCase;
begin

end;

function TCustomAbstractGroupedEdit.GetColor: TColor;
begin

end;

function TCustomAbstractGroupedEdit.GetDirectInput: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetEchoMode: TEchoMode;
begin

end;

function TCustomAbstractGroupedEdit.GetEditMask: String;
begin

end;

function TCustomAbstractGroupedEdit.GetEditPopupMenu: TPopupMenu;
begin

end;

function TCustomAbstractGroupedEdit.GetEditText: string;
begin

end;

function TCustomAbstractGroupedEdit.GetHideSelection: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetIsMasked: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetMaxLength: Integer;
begin

end;

function TCustomAbstractGroupedEdit.GetModified: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetNumbersOnly: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetParentColor: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetPasswordChar: char;
begin

end;

function TCustomAbstractGroupedEdit.GetReadOnly: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetSelLength: Integer;
begin

end;

function TCustomAbstractGroupedEdit.GetSelStart: Integer;
begin

end;

function TCustomAbstractGroupedEdit.GetSelText: String;
begin

end;

function TCustomAbstractGroupedEdit.GetSpacing: Integer;
begin

end;

function TCustomAbstractGroupedEdit.GetTabStop: Boolean;
begin

end;

function TCustomAbstractGroupedEdit.GetTextHint: TTranslateString;
begin

end;

procedure TCustomAbstractGroupedEdit.PasteFromClipboard;
begin

end;

procedure TCustomAbstractGroupedEdit.SelectAll;
begin

end;

procedure TCustomAbstractGroupedEdit.SetAlignment(const Value: TAlignment);
begin

end;

procedure TCustomAbstractGroupedEdit.SetAutoSelect(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetAutoSelected(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetBuddyCaption(const Value: TCaption);
begin

end;

procedure TCustomAbstractGroupedEdit.SetBuddyCursor(const Value: TCursor);
begin

end;

procedure TCustomAbstractGroupedEdit.SetBuddyHint(
  const Value: TTranslateString);
begin

end;

procedure TCustomAbstractGroupedEdit.SetBuddyWidth(const Value: Integer);
begin

end;

procedure TCustomAbstractGroupedEdit.SetCaretPos(const Value: TPoint);
begin

end;

procedure TCustomAbstractGroupedEdit.SetCharCase(const Value: TEditCharCase);
begin

end;

procedure TCustomAbstractGroupedEdit.SetColor(const Value: TColor);
begin

end;

procedure TCustomAbstractGroupedEdit.SetDirectInput(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetEchoMode(const Value: TEchoMode);
begin

end;

procedure TCustomAbstractGroupedEdit.SetEditMask(const Value: String);
begin

end;

procedure TCustomAbstractGroupedEdit.SetEditText(const Value: string);
begin

end;

procedure TCustomAbstractGroupedEdit.SetFocus;
begin
  inherited;

end;

procedure TCustomAbstractGroupedEdit.SetHideSelection(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetLayout(const Value: TLeftRight);
begin
  FLayout := Value;
end;

procedure TCustomAbstractGroupedEdit.SetMaxLength(const Value: Integer);
begin

end;

procedure TCustomAbstractGroupedEdit.SetModified(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetNumbersOnly(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetParentColor(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetPasswordChar(const Value: char);
begin

end;

procedure TCustomAbstractGroupedEdit.SetPopupMenu(const Value: TPopupMenu);
begin

end;

procedure TCustomAbstractGroupedEdit.SetReadOnly(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetSelLength(const Value: Integer);
begin

end;

procedure TCustomAbstractGroupedEdit.SetSelStart(const Value: Integer);
begin

end;

procedure TCustomAbstractGroupedEdit.SetSelText(const Value: String);
begin

end;

procedure TCustomAbstractGroupedEdit.SetSpacing(const Value: Integer);
begin

end;

procedure TCustomAbstractGroupedEdit.SetTabStop(const Value: Boolean);
begin

end;

procedure TCustomAbstractGroupedEdit.SetTextHint(const Value: TTranslateString);
begin

end;

procedure TCustomAbstractGroupedEdit.Undo;
begin

end;

procedure TCustomAbstractGroupedEdit.ValidateEdit;
begin

end;

{ TDirectoryEdit }

constructor TDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited;

end;

function TDirectoryEdit.GetDirectory: String;
begin

end;

procedure TDirectoryEdit.RunDialog;
begin

end;

procedure TDirectoryEdit.SetDirectory(const Value: String);
begin

end;

{ TCustomEditButton }

procedure TCustomEditButton.BuddyClick;
begin
  inherited;

end;

procedure TCustomEditButton.ButtonClick;
begin

end;

function TCustomEditButton.CalcButtonVisible: Boolean;
begin

end;

procedure TCustomEditButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited;

end;

procedure TCustomEditButton.CheckButtonVisible;
begin

end;

function TCustomEditButton.GetBuddyCaption: TCaption;
begin

end;

//function TCustomEditButton.GetBuddyClassType: TControlClass;
//begin
//
//end;

function TCustomEditButton.GetBuddyCursor: TCursor;
begin

end;

function TCustomEditButton.GetBuddyHint: TTranslateString;
begin

end;

function TCustomEditButton.GetBuddyWidth: Integer;
begin

end;

function TCustomEditButton.GetButton: TSpeedButton;
begin

end;

//class function TCustomEditButton.GetControlClassDefaultSize: TSize;
//begin
//
//end;

//function TCustomEditButton.GetDefaultGlyphName: string;
//begin
//
//end;

function TCustomEditButton.GetEdit: TEdit;
begin

end;

//function TCustomEditButton.GetEditorClassType: TGEEditClass;
//begin
//
//end;

function TCustomEditButton.GetFocusOnButtonClick: Boolean;
begin

end;

function TCustomEditButton.GetGlyph: TBitmap;
begin

end;

function TCustomEditButton.GetImageIndex: TImageIndex;
begin

end;

function TCustomEditButton.GetImages: TCustomImageList;
begin

end;

function TCustomEditButton.GetImageWidth: Integer;
begin

end;

function TCustomEditButton.GetNumGlyps: Integer;
begin

end;

function TCustomEditButton.GetOnButtonClick: TNotifyEvent;
begin

end;

//procedure TCustomEditButton.GlyphChanged(Sender: TObject);
//begin
//
//end;

//procedure TCustomEditButton.LoadDefaultGlyph;
//begin
//
//end;

procedure TCustomEditButton.SetBuddyCaption(const Value: TCaption);
begin

end;

procedure TCustomEditButton.SetBuddyCursor(const Value: TCursor);
begin

end;

procedure TCustomEditButton.SetBuddyHint(const Value: TTranslateString);
begin

end;

procedure TCustomEditButton.SetBuddyWidth(const Value: Integer);
begin

end;

procedure TCustomEditButton.SetButtonOnlyWhenFocused(const Value: Boolean);
begin
  FButtonOnlyWhenFocused := Value;
end;

procedure TCustomEditButton.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
end;

procedure TCustomEditButton.SetFocusOnButtonClick(const Value: Boolean);
begin

end;

procedure TCustomEditButton.SetGlyph(const Value: TBitmap);
begin

end;

procedure TCustomEditButton.SetImageIndex(const Value: TImageIndex);
begin

end;

procedure TCustomEditButton.SetImages(const Value: TCustomImageList);
begin

end;

procedure TCustomEditButton.SetImageWidth(const Value: Integer);
begin

end;

procedure TCustomEditButton.SetNumGlyphs(const Value: Integer);
begin

end;

procedure TCustomEditButton.SetOnButtonClick(const Value: TNotifyEvent);
begin

end;

{ TColorButton }

procedure TColorButton.Click;
begin
  inherited;

end;

constructor TColorButton.Create(AnOwner: TComponent);
begin
  inherited;

end;

destructor TColorButton.Destroy;
begin

  inherited;
end;

function TColorButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
begin

end;

class function TColorButton.GetControlClassDefaultSize: TSize;
begin

end;

function TColorButton.GetDisabledPattern: TBitmap;
begin

end;

function TColorButton.GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize;
begin

end;

function TColorButton.IsButtonColorAutoSizeStored: boolean;
begin

end;

procedure TColorButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

procedure TColorButton.SetBorderWidth(const AValue: Integer);
begin

end;

procedure TColorButton.SetButtonColor(const AValue: TColor);
begin

end;

procedure TColorButton.SetButtonColorAutoSize(const AValue: Boolean);
begin

end;

procedure TColorButton.SetButtonColorSize(const AValue: Integer);
begin

end;

procedure TColorButton.ShowColorDialog;
begin

end;

class procedure TColorButton.WSRegisterClass;
begin
  inherited;

end;

{ TCustomCheckCombo }

procedure TCustomCheckCombo.AddItem(const AItem: string; AState: TCheckBoxState;
  AEnabled: Boolean);
begin

end;

procedure TCustomCheckCombo.AssignItems(AItems: TStrings);
begin

end;

procedure TCustomCheckCombo.CheckAll(AState: TCheckBoxState; AAllowGrayed,
  AAllowDisabled: Boolean);
begin

end;

procedure TCustomCheckCombo.Clear;
begin
  inherited;

end;

constructor TCustomCheckCombo.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TCustomCheckCombo.DeleteItem(AIndex: Integer);
begin

end;

destructor TCustomCheckCombo.Destroy;
begin

  inherited;
end;

function TCustomCheckCombo.GetChecked(AIndex: Integer): Boolean;
begin

end;

function TCustomCheckCombo.GetCount: Integer;
begin

end;

function TCustomCheckCombo.GetItemEnabled(AIndex: Integer): Boolean;
begin

end;

function TCustomCheckCombo.GetObject(AIndex: Integer): TObject;
begin

end;

function TCustomCheckCombo.GetState(AIndex: Integer): TCheckBoxState;
begin

end;

procedure TCustomCheckCombo.SetChecked(AIndex: Integer; AValue: Boolean);
begin

end;

procedure TCustomCheckCombo.SetItemEnabled(AIndex: Integer; AValue: Boolean);
begin

end;

procedure TCustomCheckCombo.SetObject(AIndex: Integer; AValue: TObject);
begin

end;

procedure TCustomCheckCombo.SetState(AIndex: Integer; AValue: TCheckBoxState);
begin

end;

procedure TCustomCheckCombo.Toggle(AIndex: Integer);
begin

end;

{ TCheckComboBox }

function TCheckComboBox.GetItemWidth: Integer;
begin

end;

procedure TCheckComboBox.SetItemWidth(const Value: Integer);
begin

end;

{ TFlowPanel }

procedure TFlowPanel.SetControlList(const Value: TFlowPanelControlList);
begin
  FControlList := Value;
end;

{ TFlowPanelControlList }

function TFlowPanelControlList.AllowAdd: Boolean;
begin

end;

function TFlowPanelControlList.AllowDelete: Boolean;
begin

end;

procedure TFlowPanelControlList.Exchange(const Index1, index2: integer);
begin

end;

function TFlowPanelControlList.GetItem(Index: Integer): TFlowPanelControl;
begin

end;

function TFlowPanelControlList.IndexOf(AControl: TControl): Integer;
begin

end;

procedure TFlowPanelControlList.Move(const Index1, index2: integer);
begin

end;

procedure TFlowPanelControlList.SetItem(Index: Integer;
  const Value: TFlowPanelControl);
begin

end;

{ TFlowPanelControl }

function TFlowPanelControl.AllowAdd: Boolean;
begin

end;

function TFlowPanelControl.AllowDelete: Boolean;
begin

end;

procedure TFlowPanelControl.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

function TFlowPanelControl.FPOwner: TCustomFlowPanel;
begin

end;

function TFlowPanelControl.GetDisplayName: String;
begin

end;

procedure TFlowPanelControl.SetControl(const aControl: TControl);
begin

end;

procedure TFlowPanelControl.SetIndex(Value: Integer);
begin
  inherited;

end;

procedure TFlowPanelControl.SetWrapAfter(const AWrapAfter: TWrapAfter);
begin

end;

{ TPageControl }

function TPageControl.AddTabSheet: TTabSheet;
begin

end;

procedure TPageControl.Clear;
begin

end;

function TPageControl.FindNextPage(CurPage: TTabSheet; GoForward,
  CheckTabVisible: Boolean): TTabSheet;
begin

end;

function TPageControl.IndexOfPageAt(X, Y: Integer): Integer;
begin

end;

function TPageControl.IndexOfTabAt(X, Y: Integer): Integer;
begin

end;

procedure TPageControl.SelectNextPage(GoForward: Boolean);
begin

end;

procedure TPageControl.SetPageIndex(const Value: Integer);
begin
  FPageIndex := Value;
end;

initialization
  InitDefaultControlRtti;
  InitDefaultWinControlRtti;

finalization



end.
