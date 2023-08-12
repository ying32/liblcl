//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
// 
// Licensed under Lazarus.modifiedLGPL
//
//----------------------------------------

unit uFormDesignerFile;

{$I ExtDecl.inc}

interface

uses
  Forms,
  sysutils,
  Classes,
  typinfo,
  Types;

  procedure ResFormLoadFromResourceName(AInstance: NativeUInt; AResName: PChar; ARoot: TComponent); extdecl;
  procedure ResFormLoadFromFile(AFileName: PChar; ARoot: TComponent); extdecl;
  procedure ResFormLoadFromStream(AStream: TStream; ARoot: TComponent); extdecl;

  function ResFormRegisterFormResource(AClassName: PChar; AData: Pointer; ALen: Integer): LongBool; extdecl;
  function ResFormLoadFromClassName(AClassName: PChar; ARoot: TComponent): LongBool; extdecl;

implementation

uses
  uComponents, uExceptionHandle;

type

  { TResForm }

  TResForm = class
  public
    procedure OnFindComponentClass(AReader: TReader; const AClassName: string;
       var AComponentClass: TComponentClass);
    procedure OnPropertyNotFound(AReader: TReader; AInstance: TPersistent;
       var APropName: string; AIsPath: boolean; var AHandled, ASkip: Boolean);

    procedure OnAncestorNotFound(Reader: TReader; const ComponentName: string;
      ComponentClass: TPersistentClass; var Component: TComponent);
    procedure OnReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure OnCreateComponentEvent(Reader: TReader;
      ComponentClass: TComponentClass; var Component: TComponent);

    procedure OnReadWriteStringProperty(Sender:TObject;
      const Instance: TPersistent; PropInfo: PPropInfo;
      var Content:string);

    procedure LoadFromFile(const AFileName: string; ARoot: TComponent);
    procedure LoadFromStream(AStream: TStream; ARoot: TComponent);
    procedure LoadFromResourceName(AInstance: NativeUInt; AName: string; ARoot: TComponent);
  end;


procedure TResForm.OnFindComponentClass(AReader: TReader; const AClassName: string;
  var AComponentClass: TComponentClass);
var
  LB: TClass;
begin
  if uClassLists.TryGetData(AClassName, LB) then
    AComponentClass := TComponentClass(LB);
end;

procedure TResForm.OnPropertyNotFound(AReader: TReader; AInstance: TPersistent;
  var APropName: string; AIsPath: boolean; var AHandled, ASkip: Boolean);
begin
   ASkip := True;
end;

procedure TResForm.OnAncestorNotFound(Reader: TReader;
  const ComponentName: string; ComponentClass: TPersistentClass;
  var Component: TComponent);
begin

end;

procedure TResForm.OnReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TResForm.OnCreateComponentEvent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
//{$IFDEF WINDOWS}
//  if ComponentClass.ClassName = 'TStatusBar' then
//    Component := TComponent.Create(Reader.Owner);
//{$ENDIF}
end;

procedure TResForm.OnReadWriteStringProperty(Sender: TObject;
  const Instance: TPersistent; PropInfo: PPropInfo; var Content: string);
begin

end;

type
   TFormPatch = class(TForm)
   public
     procedure FormStateIncludeCreating;
     procedure FormStateExcludeCreating;
   end;

procedure TFormPatch.FormStateIncludeCreating;
begin
  Include(FFormState, fsCreating);
end;

procedure TFormPatch.FormStateExcludeCreating;
begin
  Exclude(FFormState, fsCreating);
end;

function InitLazResourceComponent(AReader: TReader; Instance: TComponent; RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  begin
    Result := False;
    if (ClassType = TComponent) or (ClassType = RootAncestor) then
      Exit;
    if Assigned(ClassType.ClassParent) then
      Result := InitComponent(ClassType.ClassParent);
    AReader.ReadRootComponent(Instance);
    Result := True;
  end;

begin
  if Instance.ComponentState * [csLoading, csInline] <> []
  then begin
    // global loading not needed
    Result := InitComponent(Instance.ClassType);
  end
  else try
    BeginGlobalLoading;
    Result := InitComponent(Instance.ClassType);
    NotifyGlobalLoading;
  finally
    EndGlobalLoading;
  end;
end;


procedure TResForm.LoadFromStream(AStream: TStream; ARoot: TComponent);
const
  // TPF0
  HEADERTPF0: array[0..3] of byte = ($54, $50, $46, $30);
var
  LR: TReader;

  function IsTPF0: Boolean;
  var
    LHeader: array[0..High(HEADERTPF0)] of Byte;
  begin
    AStream.Position:=0;
    AStream.Read(LHeader[0], Length(LHeader));
    Result := CompareMem(@LHeader[0], @HEADERTPF0[0], Length(LHeader));
    AStream.Position:=0;
  end;

begin
  if (AStream = nil) or (ARoot = nil) then
    Exit;
  if not IsTPF0 then
  begin
    Writeln('The Form resource is bad.');
    Exit;
  end;
  AStream.Position := 0;
  LR := TReader.Create(AStream, 4096);
  try
    LR.OnFindComponentClass := @OnFindComponentClass;
    LR.OnPropertyNotFound := @OnPropertyNotFound;
    LR.OnAncestorNotFound := @OnAncestorNotFound;
    LR.OnError:= @OnReaderError;
    LR.OnReadStringProperty := @OnReadWriteStringProperty;
    LR.OnCreateComponent := @OnCreateComponentEvent;

    GlobalNameSpace.BeginWrite;
    try
      if (ARoot.ClassType <> TForm) and not (csDesigning in ARoot.ComponentState) then
      begin
        if ARoot is TForm then
        begin
          TFormPatch(ARoot).FormStateIncludeCreating;
          try
            InitLazResourceComponent(LR, ARoot, TForm);
          finally
            TFormPatch(ARoot).FormStateExcludeCreating;
          end;
        end else if ARoot is TFrame then
        begin
          //ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption,
          //                 csDoubleClicks, csParentBackground];
          if (ARoot.ClassType = TFrame) and ([csDesignInstance, csDesigning] * ARoot.ComponentState = []) then
          begin
            LR.ReadRootComponent(ARoot);
            // InitLazResourceComponent(LR, ARoot, TFrame);
          end
        end;
      end;
    finally
      GlobalNameSpace.EndWrite;
    end;
  finally
    LR.Free;
  end;
end;

procedure TResForm.LoadFromFile(const AFileName: string; ARoot: TComponent);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(LFileStream, ARoot);
  finally
    LFileStream.Free;
  end;
end;

procedure TResForm.LoadFromResourceName(AInstance: NativeUInt; AName: string; ARoot: TComponent);
var
  LRes: TResourceStream;
begin
  LRes := TResourceStream.Create(AInstance, PChar(AName), RT_RCDATA);
  try
    LoadFromStream(LRes, ARoot);
  finally
    LRes.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure ResFormLoadFromStream(AStream: TStream; ARoot: TComponent); extdecl;
var
  LObj: TResForm;
begin
  LObj := TResForm.Create;
  try
    handleExceptionBegin
    LObj.LoadFromStream(AStream, ARoot);
    handleExceptionEnd
  finally
    LObj.Free;
  end;
end;

procedure ResFormLoadFromFile(AFileName: PChar; ARoot: TComponent); extdecl;
var
  LObj: TResForm;
begin
  LObj := TResForm.Create;
  try
    handleExceptionBegin
    LObj.LoadFromFile(AFileName, ARoot);
    handleExceptionEnd
  finally
    LObj.Free;
  end;
end;

procedure ResFormLoadFromResourceName(AInstance: NativeUInt; AResName: PChar; ARoot: TComponent); extdecl;
var
  LObj: TResForm;
begin
  LObj := TResForm.Create;
  try
    handleExceptionBegin
    LObj.LoadFromResourceName(AInstance, AResName, ARoot);
    handleExceptionEnd
  finally
    LObj.Free;
  end;
end;

type
  TResItem = record
    ClassName: string;
    Data: Pointer;
    DataLen: Integer;
  end;

var
  ResItems: array of TResItem;

function ResFormIndexOf(AClassName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(ResItems) do
    if SameText(ResItems[I].ClassName, AClassName) then
      Exit(I);
end;

function ResFormRegisterFormResource(AClassName: PChar; AData: Pointer;
  ALen: Integer): LongBool; extdecl;
begin
  Result := False;
  if ResFormIndexOf(AClassName) = -1 then
  begin
    SetLength(ResItems, Length(ResItems) + 1);
    with ResItems[High(ResItems)] do
    begin
      ClassName := UpperCase(string(AClassName));
      Data := AData;
      DataLen := ALen;
    end;
    Result := True;
  end;
end;

function ResFormLoadFromClassName(AClassName: PChar; ARoot: TComponent): LongBool; extdecl;
var
  LIdx: Integer;
  LItem: TResItem;
  LMem: TMemoryStream;
begin
  Result := False;
  LIdx := ResFormIndexOf(AClassName);
  if LIdx <> -1 then
  begin
    LItem := ResItems[LIdx];
    LMem := TMemoryStream.Create;
    try
      LMem.Write(LItem.Data^, LItem.DataLen);
      LMem.Position := 0;
      ResFormLoadFromStream(LMem, ARoot);
    finally
      LMem.Free;
    end;
    Result := True;
  end;
end;

end.
