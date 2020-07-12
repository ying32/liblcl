//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uMiniWebview;

{$IF Defined(MSWINDOWS)}
  {$I MiniWebviewWin.inc}
{$ELSEIF Defined(DARWIN) or Defined(MACOS)}
  {$IFDEF LCLcocoa}
    {$I MiniWebviewMac.inc}
  {$ELSE}
     interface
       type TMiniWebview = class end;
     implementation
  {$ENDIF}
{$ELSEIF Defined(LINUX)}
     {$IF Defined(LCLgtk2) or Defined(LCLgtk3)}
        {$I MiniWebviewLinux.inc}
     {$ELSE}
        interface
          type TMiniWebview = class end;
        implementation
     {$ENDIF}
{$ELSE}
interface
  type TMiniWebview = class end;
implementation
{$ENDIF}

end.
