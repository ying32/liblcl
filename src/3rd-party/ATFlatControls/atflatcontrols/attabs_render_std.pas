unit ATTabs_Render_Std;

interface

uses
  SysUtils, Classes, Graphics,
  ATTabs_Render;

type

  { TATTabsRendererStd }

  TATTabsRendererStd = class(TATTabsRenderer)
    procedure PaintTab(C: TCanvas; const ARect: TRect); override;
    procedure PaintPlus(C: TCanvas; const ARect: TRect; AActive: boolean); override;
    procedure PaintX(C: TCanvas; const ARect: TRect; AActive: boolean); override;
  end;


implementation

{ TATTabsRendererStd }

procedure TATTabsRendererStd.PaintTab(C: TCanvas; const ARect: TRect);
begin

end;

procedure TATTabsRendererStd.PaintPlus(C: TCanvas; const ARect: TRect;
  AActive: boolean);
begin

end;

procedure TATTabsRendererStd.PaintX(C: TCanvas; const ARect: TRect;
  AActive: boolean);
begin

end;

end.
