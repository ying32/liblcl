unit ATTabs_Render;

interface

uses
  SysUtils, Classes, Graphics;

type
  { TATTabsRenderer }

  TATTabsRenderer = class
  protected
    FTabs: TObject;
  public
    procedure SetTabs(Sender: TObject);
    procedure PaintTab(C: TCanvas; const ARect: TRect); virtual; abstract;
    procedure PaintPlus(C: TCanvas; const ARect: TRect; AActive: boolean); virtual; abstract;
    procedure PaintX(C: TCanvas; const ARect: TRect; AActive: boolean); virtual; abstract;
  end;

implementation

{ TATTabsRenderer }

procedure TATTabsRenderer.SetTabs(Sender: TObject);
begin
  FTabs:= Sender;
end;

end.
