unit qt5richmemo;

interface

{$mode delphi}

{$define RMQT5_TEXTFORMATS} // the feature is available in Qt5 Trunk
                            // it allows to query Qt5TextEdit for character formats array
{$ifdef RMQT5_NOTEXTFORMATS}{$undef RMQT5_TEXTFORMATS}{$endif}

//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  LCLType, Controls, StdCtrls, Graphics,
  qt5, qtobjects, qtwidgets, qtprivate,
  WSProc,
  RichMemo, WSRichMemo;

type
  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function isInternalChange(const AWinControl: TWinControl; Params: TTextModifyMask
      ): Boolean; override;
    class procedure SetTextAttributesInternal(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AModifyMask: TTextModifyMask; const Params: TIntFontParams); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;

  end;

type
  TEditorState = record
    selst  : integer; // selection start
    sellen : integer; // selection length
  end;

// no sanity check is done in these functions
procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);

implementation

const
  WordWrapMap: array[Boolean] of QTextEditLineWrapMode =
  (
    QTextEditNoWrap,
    QTextEditWidgetWidth
  );

  AlignmentMap: array[TIntParaAlignment] of QtAlignment =
  (
    QtAlignLeft,
    QtAlignRight,
    QtAlignHCenter,
    QtAlignJustify
  );

{ TQtWSCustomRichMemo }

class function TQtWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtTextEdit;
begin
  QtTextEdit := TQtTextEdit.Create(AWinControl, AParams);
  QtTextEdit.AcceptRichText := True;
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);
  QtTextEdit.AttachEvents;

  Result := TLCLIntfHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  // alignment
  QTextEdit_setAlignment(w, AlignmentMap[AAlign]);

  te.setSelection(ss, sl);
end;

const
  QNormal = 50;
  QBold   = 75;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : WideString;
  clr: TQColor;
  bck : TEditorState;
begin
  InitFontParams(Params);
  if not WSCheckHandleAllocated(AWinControl, 'GetTextAttributes') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  MakeBackup(te, bck);

  te.setSelection(TextStart, 1);

  //todo!
  ws:='';
  QTextEdit_fontFamily(w, @ws);
  if ws<>'' then Params.Name:=UTF8Encode(ws);

  Params.Size:=round(QTextEdit_fontPointSize(w));
  if QTextEdit_fontWeight(w)>=QBold then Include(Params.Style, fsBold);
  if QTextEdit_fontItalic(w) then Include(Params.Style, fsItalic);
  if QTextEdit_fontUnderline(w) then Include(Params.Style, fsUnderline);

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.Color));

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textBackgroundColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.BkColor));
  //todo!
  params.HasBkClr:=false;

  ApplyBackup(te, bck);

  Result:=true;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
  ws : WideString;
  clr: TQColor;
const
  QNormal = 50;
  QBold   = 75;
const
  QIsBold: array [Boolean] of integer = (QNormal, QBold);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextAttributes') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  ws:=UTF8Decode(Params.Name);
  if ws<>'' then QTextEdit_setFontFamily(w, @ws);
  if Params.Size>0 then QTextEdit_setFontPointSize(w, Params.Size);
  QTextEdit_setFontUnderline(w, fsUnderline in Params.Style);
  QTextEdit_setFontWeight(w, QisBold[fsBold in Params.Style]);
  QTextEdit_setFontItalic(w, fsItalic in Params.Style);

  ColorRefToTQColor(Params.Color, clr);
  QTextEdit_setTextColor(w, @clr);

  //todo:
  {
  if not Params.HasBkClr then begin
    ColorRefToTQColor(Params.BkColor, clr);
    clr.Alpha:=0;
  end else
    ColorRefToTQColor(Params.BkColor, clr);
  QTextEdit_setTextBackgroundColor(w, @clr);
  }

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : Widestring;
  fl : QTextDocumentFindFlags;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  fl:=0;
  if soMatchCase in SearchOpts.Options then fl:=fl or QTextDocumentFindCaseSensitively;
  if soWholeWord in SearchOpts.Options then fl:=fl or QTextDocumentFindWholeWords;
  if soBackward in SearchOpts.Options then fl:=fl or QTextDocumentFindBackward;

  //todo: range filtering in Serach Opts
  ws:=UTF8Decode(ANiddle);
  if QTextEdit_find(w, @ws, fl) then  Result:=te.getSelectionStart
  else Result:=-1;
end;

class function TQtWSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result := false;

  //Result:=inherited isInternalChange(AWinControl, Params);
end;

class procedure TQtWSCustomRichMemo.SetTextAttributesInternal(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AModifyMask: TTextModifyMask; const Params: TIntFontParams);
begin
  SetTextAttributes(AWinControl, TextStart, TextLen, Params);
end;

class function TQtWSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  te : TQtTextEdit;
  bck : TEditorState;
  al : QtAlignment;
  qcur : QTextCursorH;
  qblck : QTextBlockH;
  qbfmt : QTextBlockFormatH;
  i   : integer;
  cnt : integer;
  rng : array of TTextRange;
  blckofs: integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetStyleRange') then begin
    Result:=false;
    Exit;
  end;

  RangeStart:=TextStart;
  RangeLen:=1;
  Result:=true;
  {$ifndef RMQT5_TEXTFORMATS}
  Exit;
  {$else}
  te:=TQtTextEdit(AWinControl.Handle);

  MakeBackup(te, bck);
  qcur := QTextCursor_Create();
  qblck := QTextBlock_Create();
  try
    te.setSelection(TextStart, 0);
    QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
    QTextCursor_block(qcur, qblck);

    cnt := QTextBlock_textFormatsCount(qblck);
    SetLength(rng, cnt);
    if cnt>0 then begin
      blckofs := QTextBlock_position(qblck);
      textStart := textStart - blckofs;
      for i:=0 to cnt-1 do begin
        if (textStart >= rng[i].start) and (textStart<rng[i].start+rng[i].length) then
        begin
          RangeStart := rng[i].start + blckofs;
          RangeLen := rng[i].length;
          break;
        end;
      end;
    end;
  finally
    QTextBlock_Destroy(qblck);
    QTextCursor_Destroy(qcur);
    ApplyBackup(te, bck);
  end;
  {$endif}
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te : TQtTextEdit;
  al : QtAlignment;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetParaAlignment') then begin
    Result:=false;
    Exit;
  end;
  te:=TQtTextEdit(AWinControl.Handle);
  al:=QTextEdit_alignment(QTextEditH(te.Widget));
  if QtAlignLeading and al > 0 then AAlign:=paLeft
  else if QtAlignTrailing and al > 0 then AAlign:=paRight
  else if QtAlignCenter and al > 0 then AAlign:=paCenter
  else if QtAlignJustify and al > 0 then AAlign:=paJustify
  else AAlign:=paLeft;
  Result:=true;
end;

procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
begin
  backup.selst:=te.getSelectionStart;
  backup.sellen:=te.getSelectionLength;
end;

procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);
begin
  te.setSelection(backup.selst, backup.sellen);
end;

end.
