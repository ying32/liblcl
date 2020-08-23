/*
   The code is automatically generated by the genBind tool.
   Author: ying32
   https://github.com/ying32
*/
##
#![allow(non_snake_case)]
#![allow(unused_unsafe)]
#![allow(dead_code)]


{{$toRustStr := "ToRustString"}}

##
use lclapi;
use std::borrow::Cow;
use std::ffi::{CStr, CString};
use types::*;
use vcl::{TControl, TWinControl, IObject, IComponent, IStrings, IStream, TClipboard};
##
pub fn GetFPStringArrayMember{{html "<'a>"}}(ptr: usize, index: isize) -> Cow{{html "<'a, str>"}} {
    return {{$toRustStr}}(unsafe { lclapi::DGetStringArrOf(ptr, index) });
}
##
pub fn GetLibResourceItem(index: i32) -> TResItem {
    let mut result = TResItem::empty();
    unsafe {
        lclapi::DGetLibResourceItem(index, &mut result);
    }
    return result;
}
##
pub fn StringToGUID(guidStr: &str) -> TGUID {
    let mut result = TGUID::empty();
    unsafe { lclapi::DStringToGUID(to_CString!(guidStr), &mut result) }
    return result;
}
##
pub fn CreateGUID() -> TGUID {
    let mut result = TGUID::empty();
    unsafe { lclapi::DCreateGUID(&mut result) }
    return result;
}
##
#[cfg(target_os = "linux")]
pub fn GdkWindow_GetXId(AW: PGdkWindow) -> TXId {
    let mut result: TXID = 0;
    unsafe { lclapi::GdkWindow_GetXId(AW, &mut result) }
    return result;
}
##
pub fn SelectDirectory{{html "<'a>"}}(Options: TSelectDirOpts, HelpCtx: i32) -> (bool, {{html "Cow<'a, str>"}}) {
    let mut cstr = to_CString!("");
    let result = unsafe { lclapi::DSelectDirectory1(&mut cstr, Options, HelpCtx) };
    return (result, {{$toRustStr}}(cstr), );
}
##
pub fn SelectDirectory2{{html "<'a>"}}(Caption: &str, Root: &str, AShowHidden: bool) -> (bool, {{html "Cow<'a, str>"}}) {
    let mut cstr = to_CString!("");
    let result = unsafe { lclapi::DSelectDirectory2(to_CString!(Caption), to_CString!(Root), AShowHidden, &mut cstr) };
    return (result, {{$toRustStr}}(cstr), );
}
##
pub fn InputQuery{{html "<'a>"}}(ACaption: &str, APrompt: &str, Value: &str)-> (bool, {{html "Cow<'a, str>"}}) {
    let mut cstr = to_CString!("");
    let result = unsafe { lclapi::DInputQuery(to_CString!(ACaption), to_CString!(APrompt), to_CString!(Value), &mut cstr) };
    return (result, {{$toRustStr}}(cstr), );
}
##

pub fn {{$toRustStr}}{{html "<'a>"}}(s: *const i8) -> {{html "Cow<'a, str>"}} {
    if s == 0 as *const i8 {
        return Cow::Owned(String::from(""));
    }
    return unsafe { CStr::from_ptr(s).to_string_lossy() };
}
##
pub fn WindowFromPoint(point: &TPoint) -> HWND {
    unsafe {
         let mut ps0 = point.clone();
         return lclapi::DWindowFromPoint(&mut ps0);
    }
}
##
{{define "getFunc"}}
    {{$el := .}}
    {{$buff := newBuffer}}

    {{if eq $el.Platform "windows"}}
        {{$buff.Writeln "#[cfg(target_os = \"windows\")]"}}
    {{else if eq $el.Platform "linux,macos"}}
        {{$buff.Writeln "#[cfg(not(target_os = \"windows\"))]"}}
    {{else if eq $el.Platform "macos"}}
        {{$buff.Writeln "#[cfg(target_os = \"macos\")]"}}
    {{else if eq $el.Platform "linux"}}
        {{$buff.Writeln "#[cfg(target_os = \"linux\")]"}}
    {{end}}


    {{$retIsStr := eq $el.Return "string"}}

    {{$buff.Write "pub fn " (delDChar $el.Name)}}
    {{if $retIsStr}}
        {{$buff.Write "<'a>"}}
    {{end}}
    {{$buff.Write "("}}
    {{range $idx, $ps := .Params}}
        {{if gt $idx 0}}
            {{$buff.Write ", "}}
        {{end}}
        {{$buff.Write (fLowCase $ps.Name) ": "}}
        {{if $ps.IsVar}}
            {{if ne $ps.Flag "nonPtr"}}
                {{$buff.Write "*mut "}}
            {{else}}
                {{$buff.Write "&"}}
            {{end}}
        {{end}}
        {{if isObject $ps.Type}}
            {{$buff.Write "&"}}
            {{if isIntf $ps.Type}}
                {{$buff.Write "dyn "}}
            {{end}}
            {{getIntfName $ps.Type|$buff.Write}}
        {{else}}
            {{covType2 $ps.Type|$buff.Write}}
        {{end}}
    {{end}}

    {{$buff.Write ")"}}
    {{if not (isEmpty $el.Return)}}
        {{$buff.Write " -> "}}
        {{if $retIsStr}}
           {{$buff.Write "Cow<'a, str>"}}
        {{else}}
           {{$buff.Write (covType2 $el.Return)}}
        {{end}}
    {{end}}
    {{$buff.Writeln " {"}}

    {{/*这里生成不需要var的变量*/}}
    {{range $ips, $ps := $el.Params}}
        {{if and ($ps.IsVar) (eq $ps.Flag "nonPtr")}}
            {{$buff.Writeln "    let mut ps" $ips " = " (fLowCase $ps.Name) ".clone();"}}
        {{end}}
    {{end}}

    {{$buff.Write "    unsafe { "}}
    {{if not (isEmpty $el.Return)}}
        {{$buff.Write "return "}}
    {{end}}

    {{if isObject $el.Return}}
        {{$buff.Write $el.Return "::As("}}
    {{end}}


    {{if $retIsStr}}
        {{$buff.Write "ToRustString("}}
    {{end}}
    {{$buff.Write "lclapi::" $el.Name "("}}

    {{range $idx, $ps := .Params}}
        {{if gt $idx 0}}
            {{$buff.Write ", "}}
        {{end}}
        {{$lIsObj := isObject $ps.Type}}
        {{if ne $ps.Flag "nonPtr"}}
            {{$isStr := eq $ps.Type "string"}}
            {{if $isStr}}
                {{$buff.Write "to_CString!("}}
            {{end}}
            {{$buff.Write (fLowCase $ps.Name)}}
            {{if $isStr}}
                {{$buff.Write ")"}}
            {{end}}
        {{else}}
            {{$buff.Write "&mut ps" $idx}}
        {{end}}
        {{if $lIsObj}}
            {{$buff.Write ".Instance()"}}
        {{end}}
    {{end}}

    {{$buff.Write ")"}}
    {{if $retIsStr}}
        {{$buff.Write ")"}}
    {{end}}

    {{if isObject $el.Return}}
        {{$buff.Write ")"}}
    {{end}}

    {{$buff.Writeln "}"}}

    {{$buff.Writeln "}"}}

{{$buff.ToStr}}
{{end}}

{{/*执行模板*/}}
{{range $el := .Functions}}
    {{if not (inStrArray $el.Name "DGetStringArrOf" "DSynchronize" "DMove" "DStrLen" "SetEventCallback" "SetThreadSyncCallback" "SetMessageCallback" "DSelectDirectory2" "DSelectDirectory1" "DInputQuery" "GdkWindow_GetXId" "DCreateGUID" "DStringToGUID" "DStringToGUID" "DGetLibResourceItem" "SetExceptionHandlerCallback" "DWindowFromPoint")}}
        {{if not (contains $el.Name "_Instance")}}
      ##
            {{template "getFunc" $el}}
        {{end}}
    {{end}}
{{end}}