unit atflatcontrols_register;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  ATTabs,
  {$ifdef FPC}ATGroups,{$endif}
  ATButtons, ATFlatToolbar, 
  ATListbox, ATLinkLabel, ATScrollbar, ATStatusbar,
  ATPanelSimple, ATPanelColor, ATGauge {$ifdef FPC}, LResources{$endif};

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AT Controls', [
    TATTabs,
    {$ifdef FPC}TATGroups,{$endif}
    TATButton,
    TATFlatToolbar,
    TATListbox,
    TATScrollbar,
    TATStatus,
    TATPanelSimple,
    TATPanelColor,
    TATLabelLink,
    TATGauge
    ]);
end;

initialization
  {$ifdef FPC}
  {$I res/icons.lrs}
  {$endif}

end.

