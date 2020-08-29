{
 richmemoutils.pas

 Author: Dmitry 'skalogryz' Boyarintsev

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit RichMemoUtils;

interface

{$mode objfpc}{$h+}

uses
  Types, SysUtils, Classes, Graphics, LazFileUtils, LazUTF8, RichMemo;

const
  NoResize : TSize = ( cx: 0; cy : 0 );

var
  { Disclaimer: the function would insert an image file into RichMemo
    (if implemented by the widgetset) But in a very inefficient way.
    The image would be read again and the memory would be re-allocated for
    the image every time. So please, don't use it for smileys in
    your chat instant messaging.  A better API (with data caching) is considered.
    (That's why this method is not part of TCustomRichMemo class)

    APos - position in the text
    AImgSize - size to be inserted (in POINTS, not pixels!).
        if both width and height are 0, the image would not be resized at all.
    }
  InsertImageFromFile : function (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean = nil;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string): Boolean;

procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);

procedure SaveRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);

procedure InsertStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AStyle: TFontStyles;
  InsPos : Integer = -1 );
procedure InsertColorStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AColor: TColor; AStyle: TFontStyles;
  InsPos : Integer = -1 );
procedure InsertFontText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; const prms: TFontParams;
  InsPos : Integer = -1 );

implementation

procedure InsertFontText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; const prms: TFontParams;
  InsPos : Integer = -1 );
var
  len : Integer;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  len:=ARichMemo.InDelText(TextUTF8, InsPos, 0);
  ARichMemo.SetTextAttributes(InsPos, len, prms);
end;

procedure InsertColorStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AColor: TColor; AStyle: TFontStyles;
  InsPos : Integer = -1 );
var
  sel: Integer;
  ft : TFontParams;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  ARichMemo.GetTextAttributes(InsPos, ft);
  ft.Style:=AStyle;
  ft.Color:=AColor;
  InsertFontText(ARichMemo, TextUTF8, ft, InsPos);
end;

procedure InsertStyledText(const ARichMemo: TCustomRichMemo; const TextUTF8: String; AStyle: TFontStyles;
  InsPos : Integer );
var
  sel: Integer;
  ft : TFontParams;
begin
  if InsPos<0 then InsPos:=UTF8Length(ARichMemo.Text);
  ARichMemo.GetTextAttributes(InsPos, ft);
  ft.Style:=AStyle;
  InsertFontText(ARichMemo, TextUTF8, ft, InsPos);
end;

function InsertImageFileDummy(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize): Boolean;
begin
  Result:=false;
end;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string): Boolean;
begin
  Result:=InsertImageFromFile(ARichMemo, APos, FileNameUTF8, NoResize);
end;

{$IFDEF USELCLUtf8}
procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : THandleStream;
  h  : THandle;
begin
  if not Assigned(ARichMemo) then Exit;
  h:= FileOpenUTF8(FileNameUTF8, fmShareDenyNone or fmOpenRead);
  fs := THandleStream.Create( h );
  try
    ARichMemo.LoadRichText(fs);
  finally
    fs.Free;
  end;
  FileClose(h);
end;
{$ENDIF}

procedure LoadRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : TFileStream;
begin
  if not Assigned(ARichMemo) then Exit;
  fs:= TFileStream.Create( UTF8Decode(FileNameUTF8), fmShareDenyNone or fmOpenRead);
  try
    ARichMemo.LoadRichText(fs);
  finally
    fs.Free;
  end;
end;

procedure SaveRTFFile(const ARichMemo: TCustomRichMemo; const FileNameUTF8: string);
var
  fs : TFileStream;
begin
  if not Assigned(ARichMemo) then Exit;
  fs:= TFileStream.Create( UTF8Decode(FileNameUTF8), fmCreate);
  try
    ARichMemo.SaveRichText(fs);
  finally
    fs.Free;
  end;
end;


initialization
  if not Assigned(InsertImageFromFile)  then
    InsertImageFromFile := @InsertImageFileDummy;

end.
