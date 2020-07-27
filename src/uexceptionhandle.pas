//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uExceptionHandle;

{$mode objfpc}{$H+}
{$I ExtDecl.inc}

interface

uses
  Classes, SysUtils, Forms, Dialogs;

type
  TExceptionHandlerCallbackPtr = function(AErrorMsg: PChar): Pointer; extdecl; // 留给支持异常处理的语言所用

var
  GExceptionHandlerCallbackPtr: TExceptionHandlerCallbackPtr;


  procedure CallException(E: Exception);
implementation

// 显示异常
procedure CallException(E: Exception);
begin
  // 优先这个
  if Assigned(GExceptionHandlerCallbackPtr) then
  begin
    GExceptionHandlerCallbackPtr(PChar(E.Message));
    Exit;
  end;

  // 然后再是Application.OnException事件的
  if Assigned(Application) and Assigned(Application.OnException) then
  begin
    Application.OnException(Application, E);
    Exit;
  end;

  // 都没有的话，弹出错误对话框。
  MessageDlg(E.Message,  mtError, [mbOK], 0);
end;

end.

