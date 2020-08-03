//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Apache License 2.0
//
//----------------------------------------

package main

import (
	"bytes"
	"html/template"
	"strings"

	"./ast"
)

func templateIsEmpty(s string) bool {
	return s == ""
}

func templateCovType(s string) template.HTML {
	if v, ok := typeDict[s]; ok {
		return template.HTML(v)
	}
	return template.HTML(s)
}

// 不包含字符串的转换
func templateCovType2(s string) template.HTML {
	if s == "string" {
		if v, ok := typeDict["string2"]; ok {
			return template.HTML(v)
		}
		return template.HTML(s)
	}
	if v, ok := typeDict[s]; ok {
		return template.HTML(v)
	}
	return template.HTML(s)
}

func templateCovKeyWord(s string) string {
	if v, ok := keyWordsDict[s]; ok {
		return v
	}
	return s
}

func templateIsObject(s string) bool {
	s = strings.TrimSpace(s)
	if s == "" {
		return false
	}
	_, ok := Objs[s]
	return ok
}

func templateDec(val int) int {
	return val - 1
}

func templateNextType(obj []ast.TType, idx int) ast.TType {
	if idx+1 < len(obj) {
		return obj[idx+1]
	}
	return ast.TType{}
}

func templatePrevType(obj []ast.TType, idx int) ast.TType {
	if idx-1 > 0 {
		//fmt.Println("index:", idx, obj[idx-1], obj[idx])
		return obj[idx-1]
	}
	return ast.TType{}
}

func templateIsBaseObject(s string) bool {
	return s == "TObject" || s == "TComponent" || s == "TControl" || s == "TWinControl"
}

func templateRMObjectT(s string) string {
	if len(s) > 0 && s[0] == 'T' {
		return s[1:]
	}
	return s
}

func templateParamsEmpty(ps []ast.TFuncParam) bool {
	return len(ps) == 0
}

func templatePropGetName(fn ast.TFunction) string {
	if !fn.IsMethod {
		if strings.HasPrefix(fn.RealName, "Get") {
			return strings.TrimPrefix(fn.RealName, "Get")
		}
	}
	return fn.RealName
}

func recursive(name, mm string) bool {
	name, ok := Objs[name] // 查基类
	if !ok {
		return true
	} else {
		if dict, ok := methodDict[name]; ok {
			if _, ok := dict[mm]; ok {
				return false
			}
		}
	}
	return recursive(Objs[name], mm)
}

func templateIsBaseMethod(className, method string) bool {
	// 这里偷下懒吧，懒得改了
	switch className {
	case "TStrings", "TStringList":
		if method == "Equals" {
			return true
		}
	}
	return recursive(className, method)
}

func templateText(s string) template.HTML {
	return template.HTML(s)
}

func templateCPsZero(params []ast.TFuncParam) string {
	if len(params) > 0 {
		ns := ""
		for i := 0; i < 12-len(params); i++ {
			ns += " ,0"
		}
		return ns
	}
	return " ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0"
}

func templateDelDChar(s string) string {
	if len(s) > 0 {
		if s[0] == 'D' && !strings.Contains(s, "_Instance") {
			return s[1:]
		}
	}
	return s
}

func templateGetLastParam(pss []ast.TFuncParam) ast.TFuncParam {
	if len(pss) == 0 {
		return ast.TFuncParam{}
	}
	return pss[len(pss)-1]
}

func templateCanOutParam(mm ast.TFunction, idx int) bool {
	if !mm.LastIsReturn {
		return true
	}
	return idx < len(mm.Params)-1
}

func templateIsProp(mm ast.TFunction) bool {
	if !mm.IsMethod {
		return true
	}
	return false
}

func templateIsGetter(mm ast.TFunction) bool {
	if !mm.IsMethod && strings.HasPrefix(mm.RealName, "Get") {
		return true
	}
	return false
}

func templateIsSetter(mm ast.TFunction) bool {
	if !mm.IsMethod && strings.HasPrefix(mm.RealName, "Set") {
		return true
	}
	return false
}

func templateGetRealName(mm ast.TFunction) string {
	if !mm.IsMethod && (strings.HasPrefix(mm.RealName, "Get") || strings.HasPrefix(mm.RealName, "Set")) {
		return mm.RealName[3:]
	}
	if mm.IsMethod && mm.IsOverload {
		return mm.OverloadName
	}
	return mm.RealName
}

func templateGetRealName2(mm ast.TFunction) string {
	if !mm.IsMethod && strings.HasPrefix(mm.RealName, "Get") {
		return mm.RealName[3:]
	}
	return mm.RealName
}

func templateInStrArray(s string, args ...string) bool {
	for _, l := range args {
		if l == s {
			return true
		}
	}
	return false
}

func templateGetConstVal2(s string) string {
	p1 := strings.Index(s, "(")
	if p1 > 0 {
		p2 := strings.Index(s, ")")
		if p2 > 0 {
			return s[p1+1 : p2]
		}
	}
	return s
}

func templateIsIntf(s string) bool {
	switch s {
	case "TObject", "TComponent", "TControl", "TWinControl", "TStrings", "TStream":
		return true
	}
	return false
}

func templateGetIntfName(s string) string {
	if templateIsIntf(s) {
		return "I" + s[1:]
	}
	return s
}

func templateHaveFree(mts []ast.TFunction) bool {
	for _, m := range mts {
		if m.RealName == "Free" {
			return true
		}
	}
	return false
}

func templateFirstLowerCase(s string) string {
	if len(s) > 0 {
		return strings.ToLower(string(s[0])) + s[1:]
	}
	return s
}

func templateNewBuffer() *TMyBuffer {
	b := new(TMyBuffer)
	b.buff = bytes.NewBuffer(nil)
	return b
}

//func templateUnescape(s string) string {
//	return html.UnescapeString(s)
//}

func templateMultiply(v1, v2 int) int {
	return v1 * v2
}

var templateFuncs = template.FuncMap{
	"isEmpty":      templateIsEmpty,
	"covType":      templateCovType,
	"covType2":     templateCovType2,
	"isObject":     templateIsObject,
	"Dec":          templateDec,
	"covKeyword":   templateCovKeyWord,
	"nextType":     templateNextType,
	"prevType":     templatePrevType,
	"isBaseObj":    templateIsBaseObject,
	"rmObjectT":    templateRMObjectT,
	"paramsEmpty":  templateParamsEmpty,
	"propGetName":  templatePropGetName,
	"isBaseMethod": templateIsBaseMethod,
	"html":         templateText,
	"cPsZero":      templateCPsZero,
	"delDChar":     templateDelDChar,
	"lastParam":    templateGetLastParam,
	"canOutParam":  templateCanOutParam,
	"hasPrefix":    strings.HasPrefix,
	"hasSuffix":    strings.HasSuffix,
	"trim":         strings.TrimSpace,
	"contains":     strings.Contains,
	"isProp":       templateIsProp,
	"isGetter":     templateIsGetter,
	"isSetter":     templateIsSetter,
	"getRealName":  templateGetRealName,
	"inStrArray":   templateInStrArray,
	"getConstVal2": templateGetConstVal2,
	"getIntfName":  templateGetIntfName,
	"isIntf":       templateIsIntf,
	"getRealName2": templateGetRealName2,
	"haveFree":     templateHaveFree,
	"fLowCase":     templateFirstLowerCase,
	"newBuffer":    templateNewBuffer,
	//"unescape":     templateUnescape,
	"multiply": templateMultiply,
}

//Add, subtract, multiply, divide
