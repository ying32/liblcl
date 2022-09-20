
//----------------------------------------
// The code is automatically generated by the GenlibLcl tool.
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

//CLASSMETHOD:
function FlowPanelControl_AllowAdd(AObj: TFlowPanelControl): LongBool; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.AllowAdd;
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_AllowDelete(AObj: TFlowPanelControl): LongBool; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.AllowDelete;
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_GetNamePath(AObj: TFlowPanelControl): PChar; extdecl;
begin
  handleExceptionBegin
  Result :=  PChar(AObj.GetNamePath);
  handleExceptionEnd
end;

//CLASSMETHOD:
procedure FlowPanelControl_Assign(AObj: TFlowPanelControl; Source: TPersistent); extdecl;
begin
  handleExceptionBegin
  AObj.Assign(Source);
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_ClassType(AObj: TFlowPanelControl): TClass; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.ClassType;
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_ClassName(AObj: TFlowPanelControl): PChar; extdecl;
begin
  handleExceptionBegin
  Result :=  ToPChar(AObj.ClassName);
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_InstanceSize(AObj: TFlowPanelControl): Integer; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.InstanceSize;
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_InheritsFrom(AObj: TFlowPanelControl; AClass: TClass): LongBool; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.InheritsFrom(AClass);
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_Equals(AObj: TFlowPanelControl; Obj: TObject): LongBool; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.Equals(Obj);
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_GetHashCode(AObj: TFlowPanelControl): Integer; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.GetHashCode;
  handleExceptionEnd
end;

//CLASSMETHOD:
function FlowPanelControl_ToString(AObj: TFlowPanelControl): PChar; extdecl;
begin
  handleExceptionBegin
  Result :=  PChar(AObj.ToString);
  handleExceptionEnd
end;

function FlowPanelControl_GetControl(AObj: TFlowPanelControl): TControl; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.Control;
  handleExceptionEnd
end;

procedure FlowPanelControl_SetControl(AObj: TFlowPanelControl; AValue: TControl); extdecl;
begin
  handleExceptionBegin
  AObj.Control := AValue;
  handleExceptionEnd
end;

function FlowPanelControl_GetWrapAfter(AObj: TFlowPanelControl): TWrapAfter; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.WrapAfter;
  handleExceptionEnd
end;

procedure FlowPanelControl_SetWrapAfter(AObj: TFlowPanelControl; AValue: TWrapAfter); extdecl;
begin
  handleExceptionBegin
  AObj.WrapAfter := AValue;
  handleExceptionEnd
end;

function FlowPanelControl_GetIndex(AObj: TFlowPanelControl): Integer; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.Index;
  handleExceptionEnd
end;

procedure FlowPanelControl_SetIndex(AObj: TFlowPanelControl; AValue: Integer); extdecl;
begin
  handleExceptionBegin
  AObj.Index := AValue;
  handleExceptionEnd
end;

function FlowPanelControl_GetCollection(AObj: TFlowPanelControl): TCollection; extdecl;
begin
  handleExceptionBegin
  Result :=  AObj.Collection;
  handleExceptionEnd
end;

procedure FlowPanelControl_SetCollection(AObj: TFlowPanelControl; AValue: TCollection); extdecl;
begin
  handleExceptionBegin
  AObj.Collection := AValue;
  handleExceptionEnd
end;

function FlowPanelControl_GetDisplayName(AObj: TFlowPanelControl): PChar; extdecl;
begin
  handleExceptionBegin
  Result :=  PChar(AObj.DisplayName);
  handleExceptionEnd
end;

procedure FlowPanelControl_SetDisplayName(AObj: TFlowPanelControl; AValue: PChar); extdecl;
begin
  handleExceptionBegin
  AObj.DisplayName := AValue;
  handleExceptionEnd
end;

function FlowPanelControl_StaticClassType: TClass; extdecl;
begin
  Result := TFlowPanelControl;
end;

exports
  FlowPanelControl_AllowAdd,
  FlowPanelControl_AllowDelete,
  FlowPanelControl_GetNamePath,
  FlowPanelControl_Assign,
  FlowPanelControl_ClassType,
  FlowPanelControl_ClassName,
  FlowPanelControl_InstanceSize,
  FlowPanelControl_InheritsFrom,
  FlowPanelControl_Equals,
  FlowPanelControl_GetHashCode,
  FlowPanelControl_ToString,
  FlowPanelControl_GetControl,
  FlowPanelControl_SetControl,
  FlowPanelControl_GetWrapAfter,
  FlowPanelControl_SetWrapAfter,
  FlowPanelControl_GetIndex,
  FlowPanelControl_SetIndex,
  FlowPanelControl_GetCollection,
  FlowPanelControl_SetCollection,
  FlowPanelControl_GetDisplayName,
  FlowPanelControl_SetDisplayName,
  FlowPanelControl_StaticClassType;
