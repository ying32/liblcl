/*
    The code is automatically generated by the genBind tool.
    Author: ying32
    https://github.com/ying32
*/
##
#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_unsafe)]
##
use lclapi::*;
use types::*;
##
use std::borrow::Cow;
use std::ffi::{CStr, CString};
##
// IObject IComponent IControl IWinControl只是用来让编译器知道这几个的关系的
pub trait IObject {
    fn Instance(&self) -> usize;
}
pub trait IComponent: IObject {}
pub trait IControl: IComponent {}
pub trait IWinControl: IControl {}
##


##
{{/* usize是指向实例对象的指针，bool是标识能自动drop的，一般只有通过new的才可以 */}}
/* 先定义所有的类 */
{{range $el := .Objects}}
pub struct {{$el.ClassName}}(usize, bool);
{{end}}

{{/*模板定义*/}}
{{define "getlastPs"}}{{if .LastIsReturn}} -> {{$ps := lastParam .Params}}{{covType $ps.Type}}{{end}}{{end}}

/* 开始实现接口 */
{{range $el := .Objects}}
{{$className := $el.ClassName}}
{{$baseClass := $el.BaseClassName}}
{{$classN := rmObjectT $className}}
##
{{$buff := newBuffer}}


impl {{$className}} {
  {{range $mm := $el.Methods}}
      {{if eq $mm.RealName "Create"}}
          {{$buff.Clear}}
          {{/* TXXX::new  */}}

          {{$buff.Write "pub fn new("}}
          {{range $idx, $ps := $mm.Params}}
              {{if gt $idx 0}}
                  {{$buff.Write ", "}}
              {{end}}
              {{fLowCase $ps.Name|$buff.Write}}
              {{$buff.Write ": "}}
              {{if isObject $ps.Type}}
                  {{$buff.Write "&"}}
                  {{if isIntf $ps.Type}}
                      {{$buff.Write "dyn "}}
                  {{end}}
              {{end}}
              {{covType2 (getIntfName $ps.Type)|$buff.Write}}
          {{end}}
          {{$buff.Write ")"}}
          {{$buff.Writeln " -> Self {"}}

          {{$buff.Write "        "}}
          {{$buff.Write "method_Create!(" $className ", " $classN "_Create" ", "}}
		  {{range $idx, $ps := $mm.Params}}
		      {{if gt $idx 0}}
		          {{$buff.Write ", "}}
		      {{end}}
		      {{if eq $ps.Type "string"}}
		          {{$buff.Write "to_CString!(" (fLowCase $ps.Name) ")"}}
		      {{else}}
		          {{fLowCase $ps.Name|$buff.Write}}
		      {{end}}
		      {{if isObject $ps.Type}}
		          {{$buff.Write ".Instance()"}}
		      {{end}}
		  {{end}}
		  {{$buff.Writeln ");"}}

      {{$buff.ToStr}}
      }
##
      impl_As_method!({{$className}});
##
      {{else if eq $mm.RealName "Free"}}
	  impl_Free_method!({{$classN}}_Free);
##
      {{else if eq $mm.RealName "CreateForm"}}
      pub fn CreateForm(&self) -> TForm  {
	      return method_Call_2!(TForm, Application_CreateForm, self.0, false);
      }
##
      {{else}}
          {{if not $mm.IsStatic}}
              {{if not (inStrArray $mm.RealName "TextRect2")}}

                  {{$notProp := not (isProp $mm)}}
                  {{$buff.Clear}}
                  {{$buff.Write "pub fn " (getRealName2 $mm)}}
                  {{if eq $mm.Return "string"}}
                      {{$buff.Write "<'a>"}}
                  {{end}}
                  {{$buff.Write "(&self"}}


                  {{range $idx, $ps := $mm.Params}}
                      {{if gt $idx 0}}
                          {{$buff.Write ", " (fLowCase $ps.Name) ": "}}
                          {{if isObject $ps.Type}}
                              {{$buff.Write "&"}}
                              {{if isIntf $ps.Type}}
                                  {{$buff.Write "dyn "}}
                              {{end}}
                          {{end}}
                          {{if $ps.IsVar}}
                              {{$buff.Write "*mut "}}
                          {{end}}
                          {{covType2 (getIntfName $ps.Type)|$buff.Write}}
                      {{end}}
                  {{end}}
                  {{$buff.Write ")"}}
                  {{if not (isEmpty $mm.Return)}}
                      {{$buff.Write " -> "}}
                      {{if eq $mm.Return "string"}}
                          {{$buff.Write "Cow<'a, str>"}}
                      {{else}}
                          {{covType2 $mm.Return|$buff.Write}}
                      {{end}}
                  {{else}}

                  {{end}}
                  {{if $notProp}}
                      {{if isBaseMethod $el.ClassName $mm.RealName}}
                          {{$buff.Write " "}}
                      {{end}}
                  {{else}}
                      {{$buff.Write " "}}
                  {{end}}
                  {{$buff.Writeln " {"}}

                  {{/* ---------------- call func ---------------- */}}
                  {{$retIsObj := isObject $mm.Return}}
                  {{$retIsStr := eq $mm.Return "string"}}

                  {{$buff.Write "          "}}
                  {{if not (isEmpty $mm.Return)}}
                      {{$buff.Write "return "}}
                  {{end}}
                  {{if $retIsStr}}
                      {{$buff.Write "to_RustString!("}}
                  {{end}}
                  {{$buff.Write "method_Call_"}}
                  {{if $retIsObj}}
                      {{$buff.Write 2}}
                  {{else}}
                      {{$buff.Write 1}}
                  {{end}}
                  {{$buff.Write "!("}}
                  {{if $retIsObj}}
                      {{$buff.Write $mm.Return ", "}}
                  {{end}}
                  {{$buff.Write $mm.Name ", self.0"}}

		          {{range $idx, $ps := $mm.Params}}
		              {{if gt $idx 0}}
		                  {{$buff.Write ", "}}
			              {{if eq $ps.Type "string"}}
			                  {{$buff.Write "to_CString!(" (fLowCase $ps.Name) ")"}}
				          {{else}}
		                      {{fLowCase $ps.Name|$buff.Write}}
		                      {{if isObject $ps.Type}}
		                          {{$buff.Write ".Instance()"}}
		                      {{end}}
				          {{end}}
			           {{end}}
			      {{end}}
			      {{$buff.Write ")"}}
		          {{if $retIsStr}}
		              {{$buff.Write ")"}}
		          {{end}}
                  {{$buff.Writeln ";"}}

	  {{$buff.ToStr}}
      }

               {{end}}
##
           {{else}}
      // static class
	  impl_Class_method!({{$mm.Name}});

           {{end}}
      {{end}}
  {{end}}
}
##
impl_IObject!({{$className}});
{{if or (or (or (eq $baseClass "TComponent") (eq $baseClass "TControl")) (eq $baseClass "TWinControl")) (eq $className "TComponent")}}
impl_IComponent!({{$className}});
{{end}}

{{if or (or (eq $baseClass "TControl") (eq $baseClass "TWinControl")) (eq $className "TControl")}}
impl_IControl!({{$className}});
{{end}}

{{if or (eq $baseClass "TWinControl") (eq $className "TWinControl")}}
impl_IWinControl!({{$className}});
{{end}}

{{/* 所有不为TComponent和TControl和TWinControl的实现drop方法 */}}
{{if haveFree $el.Methods}}
  {{if or (eq $baseClass "TObject") (eq $className "TObject")}}
 
impl_Drop_method!({{$className}});

  {{end}}
{{end}}


{{/*循环一次结束*/}}
{{end}}

##
##
pub fn NullObject() -> TObject {
   TObject { 0: 0, 1: false, }
}
##
fn getApplication() -> TApplication {
   initLibLCLCallback();
   TApplication {
       0: unsafe { Application_Instance() }, 1: false,
   }
}
##
// ------------------------------------ global vars ------------------------------------
##
lazy_static! {
    pub static ref Application: TApplication = getApplication();
    pub static ref Screen: TScreen = TScreen::As(unsafe { Screen_Instance() });
    pub static ref Mouse: TMouse = TMouse::As(unsafe { Mouse_Instance() });
    pub static ref Clipboard: TClipboard = TClipboard::As(unsafe { Clipboard_Instance() });
    pub static ref Printer: TPrinter = TPrinter::As(unsafe { Printer_Instance() });
}
