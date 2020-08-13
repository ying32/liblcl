{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mylazpkgs;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImageButton, uGauge, uLinkLabel, uMiniWebview, uRichEdit, xButton, 
  uRegComponents, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uRegComponents', @uRegComponents.Register);
end;

initialization
  RegisterPackage('mylazpkgs', @Register);
end.
