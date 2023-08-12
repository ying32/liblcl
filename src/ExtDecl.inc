//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

{$MACRO ON}
{$IFDEF Windows}
  {$DEFINE extdecl := stdcall}
{$ELSE}
  {$DEFINE extdecl := cdecl}
{$ENDIF}

// 枚举都按4字节对齐
{$Z4+}

// 正式启用此定义，只是文件增大些而已，但通用性更强
{$define UsehandleException}

// 异常宏定义
{$ifdef UsehandleException}
  {$define handleExceptionBegin :=try}
  {$define handleExceptionEnd :=
     except
       on E: Exception do
         CallException(E);
     end;
  }

{$else}
  {$define handleExceptionBegin :=}
  {$define handleExceptionEnd :=}
{$endif}

(*
   EventMethodName 实际的方法名称
   EventName 组件的方法名称
   EventMethod 方法
*)
// 事件方法定义
{$DEFINE EventMethodCode :=
    handleExceptionBegin
    if AEventData = nil then
    begin
      // 这里移除掉对象
      TLCLEvent.Remove(TMethod(AObj.EventName));
      AObj.EventName := nil;
      Exit;
    end;
    if TLCLEvent.CheckAndUpdate(TMethod(AObj.EventName), AEventData) then
      Exit;
    // 创建一个事件对象
    AObj.EventName := TLCLEvent.Create(AEventData).EventMethodName;
    handleExceptionEnd
}



