//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Apache License 2.0
//
//----------------------------------------

package main

//func getNimBind() {
//
//	tplFileBs, err := ioutil.ReadFile("liblcl-template.nim")
//	if err != nil {
//		panic(err)
//	}
//	tpl := template.New("liblcl").Funcs(templateFuncs)
//
//	_, err = tpl.Parse(string(tplFileBs))
//	if err != nil {
//		panic(err)
//	}
//	buff := bytes.NewBuffer(nil)
//	err = tpl.Execute(buff, objectFile)
//	if err != nil {
//		panic(err)
//	}
//	err = ioutil.WriteFile("liblcl.nim", buff.Bytes(), 0774)
//	if err != nil {
//		panic(err)
//	}
//}

//var (
//	nimBaseTypeMap = map[string]string{
//		"LongBool":             "bool",
//		"PChar":                "cstring",
//		"Pointer":              "pointer",
//		"Integer":              "int32",
//		"Cardinal":             "uint32",
//		"Double":               "float64",
//		"Single":               "float32",
//		"TThreadID":            "uint",
//		"NativeInt":            "int",
//		"NativeUInt":           "uint",
//		"Boolean":              "BOOL",
//		"Longint":              "int32",
//		"UInt64":               "uint64",
//		"IInt64":               "int64",
//		"TBasicAction":         "TAction",
//		"TPersistent":          "pointer",
//		"Byte":                 "char",
//		"TBorderWidth":         "int32",
//		"TCustomImageList":     "pointer",
//		"TUnixDateTime":        "uint32",
//		"TCustomListView":      "pointer",
//		"TCustomTreeView":      "pointer",
//		"TImageIndex":          "int32",
//		"SmallInt":             "int16",
//		"TCustomForm":          "TForm",
//		"TWidth":               "int32",
//		"uint32":               "uint32",
//		"int32":                "int32",
//		"Int64":                "int64",
//		"LongWord":             "uint32",
//		"Word":                 "uint16",
//		"PPointerList":         "pointer",
//		"string":               "cstring",
//		"TCustomHeaderControl": "pointer",
//		"TCollectionItemClass": "pointer",
//		"PPoint":               "ptr TPoint",
//		"TOverlay":             "uint8",
//		"TGoForm":              "TForm",
//		"TLMessage":            "TMessage",
//		"IObject":              "pointer",
//		"IWinControl":          "pointer",
//		"IComponent":           "pointer",
//		"IControl":             "pointer",
//		"bool":                 "bool",
//		"uint16":               "uint16",
//		"TObject":              "pointer",
//		"TIcon":                "pointer",
//		"TForm":                "pointer",
//		"BOOL":                 "bool",
//
//		//"TResItem":             "",
//	}
//)
//
//type NimImportFile struct {
//	*File
//}
//
//func NewNimImportFile(fileName string) *NimImportFile {
//	f := new(NimImportFile)
//	f.File = NewFile(fileName)
//	return f
//}
//
//func (c *NimImportFile) WriteHeader() {
//	c.W(`
//
//
//`)
//}
//
//func (c *NimImportFile) WriteFooter() {
//	c.W(`
//
//
//`)
//}
//
//var (
//	nimIsPlatform bool
//)
//
//func MakeNimImport(name, returnType string, params []Param, isClass bool) {
//	isEq := func(s string) bool {
//		return s == name
//	}
//
//	if isEq("DSendMessage") || isEq("DCreateURLShortCut") {
//		nimIsPlatform = true
//		nimImportFile.WLn()
//		nimImportFile.WLn()
//		if isEq("DSendMessage") {
//			nimImportFile.W("when not defined(windows):\n")
//		} else if isEq("DCreateURLShortCut") {
//			nimImportFile.W("when defined(windows):\n")
//		}
//	}
//
//
//	// 平台开始，起始2个缩进
//	if nimIsPlatform {
//		nimImportFile.W("  ")
//	}
//	nimImportFile.W("proc ")
//	nimImportFile.W(name)
//	nimImportFile.W("*") // 全局导出标识
//	// 参数
//	nimImportFile.W("(")
//	for i, p := range params {
//		if i > 0 {
//			nimImportFile.W(", ")
//		}
//
//		nimImportFile.W(p.Name)
//		nimImportFile.W(": ")
//		if p.IsVar {
//			nimImportFile.W("var ")
//		}
//		nimImportFile.W(nimBaseTypeConvert(p.Type))
//	}
//	nimImportFile.W(")")
//	// 判断返回值
//	if returnType != "" {
//		nimImportFile.W(": ")
//		nimImportFile.W(nimBaseTypeConvert(returnType))
//	}
//	nimImportFile.W(fmt.Sprintf(" {.importc: \"%s\", dynlib: dllname.}", name))
//	nimImportFile.WLn()
//
//	// 添加结束
//	if isEq("DWindowFromPoint") || isEq("DCreateShortCut") {
//		nimImportFile.WLn()
//		nimIsPlatform = false
//	}
//}
