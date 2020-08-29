unit ATFlatThemes;

{$ifdef FPC}
{$ModeSwitch advancedrecords}
{$endif}

interface

uses
  Classes, Graphics;

type
  PATFlatTheme = ^TATFlatTheme;

  TATButtonOverlayPosition = (
    bopLeftTop,
    bopRightTop,
    bopLeftBottom,
    bopRightBottom
    );

  { TATFlatTheme }

  TATFlatTheme = record
    FontName: string;
    FontSize: integer;
    FontStyles: TFontStyles;

    ColorFont: TColor;
    ColorFontDisabled: TColor;
    ColorFontListbox: TColor;
    ColorFontListboxSel: TColor;
    ColorFontOverlay: TColor;
    ColorBgPassive: TColor;
    ColorBgOver: TColor;
    ColorBgChecked: TColor;
    ColorBgDisabled: TColor;
    ColorBgListbox: TColor;
    ColorBgListboxSel: TColor;
    ColorBgListboxHottrack: TColor;
    ColorBgOverlay: TColor;
    ColorArrows: TColor;
    ColorArrowsOver: TColor;
    ColorSeparators: TColor;
    ColorBorderPassive: TColor;
    ColorBorderOver: TColor;
    ColorBorderFocused: TColor;
    EnableColorBgOver: boolean;

    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
    BoldBorderWidth: integer;
    ChoiceBorderWidth: integer;
    ArrowSize: integer;
    GapForAutoSize: integer;
    TextOverlayPosition: TATButtonOverlayPosition;
    SeparatorOffset: integer;
    XMarkWidth: integer;
    XMarkOffsetLeft: integer;
    XMarkOffsetRight: integer;
    XMarkLineWidth: integer;
    ScalePercents: integer;
    ScaleFontPercents: integer;
    function DoScale(AValue: integer): integer;
    function DoScaleFont(AValue: integer): integer;
  end;

var
  ATFlatTheme: TATFlatTheme;

implementation

{ TATFlatTheme }

function TATFlatTheme.DoScale(AValue: integer): integer;
begin
  Result:= AValue * ScalePercents div 100;
end;

function TATFlatTheme.DoScaleFont(AValue: integer): integer;
begin
  if ScaleFontPercents=0 then
    Result:= DoScale(AValue)
  else
    Result:= AValue * ScaleFontPercents div 100;
end;


initialization

  with ATFlatTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontStyles:= [];

    ColorFont:= $303030;
    ColorFontDisabled:= $808088;
    ColorFontListbox:= ColorFont;
    ColorFontListboxSel:= clWhite;
    ColorFontOverlay:= clWhite;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $90a080;
    ColorBgChecked:= $b0b0b0;
    ColorBgDisabled:= $c0c0d0;
    ColorBgListbox:= ColorBgPassive;
    ColorBgListboxSel:= clMedGray;
    ColorBgListboxHottrack:= clMoneyGreen;
    ColorBgOverlay:= clRed;
    ColorArrows:= clGray;
    ColorArrowsOver:= clBlue;
    ColorSeparators:= clDkGray;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;
    EnableColorBgOver:= true;

    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
    BoldBorderWidth:= 3;
    ChoiceBorderWidth:= 1;
    ArrowSize:= 2;
    GapForAutoSize:= 8;
    TextOverlayPosition:= bopRightBottom;
    SeparatorOffset:= 2;
    XMarkWidth:= 8;
    XMarkOffsetLeft:= 1;
    XMarkOffsetRight:= 1;
    XMarkLineWidth:= 1;

    ScalePercents:= 100;
    ScaleFontPercents:= 0;
  end;

end.
