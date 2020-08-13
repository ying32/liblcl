unit uRegComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ImageButton, uGauge, uLinkLabel, uMiniWebview, uRichEdit, xButton;

procedure Register;

implementation

{$R './images/mycomponents.res'}

procedure Register;
begin
  Classes.RegisterComponents('GoVCL', [TImageButton, TGauge, TLinkLabel, TMiniWebview, TRichEdit, TXButton]);
end;

end.

