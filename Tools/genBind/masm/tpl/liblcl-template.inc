{{/* 此模板只是用来lib导入文件，利用masm生成符合msvc stdcall修饰名的liblcl.lib */}}


; --------------------------------------------------------------------------------------------------
;                           kerbcli.inc Copyright The MASM32 SDK 1998-2010
; --------------------------------------------------------------------------------------------------

IFNDEF LIBLCL_INC
LIBLCL_INC equ {{html "<1>"}}

##
{{range $el := .Functions}}
    {{if or (eq $el.Platform "windows") (eq $el.Platform "all")}}
{{$el.Name}} PROTO STDCALL{{if ne (len $el.Params) 0}} {{end}}{{range $idx, $ps := $el.Params}}{{if gt $idx 0}},{{end}}:DWORD{{end}}
    {{end}}
{{end}}
##

##
{{range $el := .Objects}}
    {{range $fn := $el.Methods}}
{{$fn.Name}} PROTO STDCALL{{if ne (len $fn.Params) 0}} {{end}}{{range $idx, $ps := $fn.Params}}{{if gt $idx 0}},{{end}}:DWORD{{end}}
    {{end}}
{{end}}

##
ELSE
  echo ------------------------------------------
  echo WARNING duplicate include file kerbcli.inc
  echo ------------------------------------------
ENDIF