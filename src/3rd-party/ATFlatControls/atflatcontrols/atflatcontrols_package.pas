{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atflatcontrols_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATButtons, atflatcontrols_register, ATListbox, ATLinkLabel, ATFlatToolbar, ATPanelSimple, ATPanelColor, 
  ATScrollBar, ATStatusBar, ATGauge, ATFlatThemes, ATGroups, attabs, ATCanvasPrimitives, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('atflatcontrols_register', @atflatcontrols_register.Register);
end;

initialization
  RegisterPackage('atflatcontrols_package', @Register);
end.
