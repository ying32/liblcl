//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Apache License 2.0
//
//----------------------------------------

package ast

import (
	"bytes"
	"fmt"
	"strconv"

	"io/ioutil"
	"os"
	"regexp"
	"strings"
)

var (
	paramsExpr  = regexp.MustCompile(`\((.+?)\)`)
	incFileExpr = regexp.MustCompile(`\{\$I\s(MyLCL\_.+?\.inc)\}\s\s\/\/BASECLASS\:(T[a-z,A-Z]*)`) //`\{\$I\s(MyLCL\_.+?\.inc)\}`)

	functionsMap    = make(map[string]string, 0)
	classArray      = make([]string, 0)
	objsMap         = make(map[string]string)
	defClassMethods = make(map[string][]string, 0)

	objectFile TObjectFile
)

func GenAst() {

	govclPath := ""
	goPath, ok := os.LookupEnv("GOPATH")
	if !ok || goPath == "" {
		panic("未找到$GOPATH")
	}

	paths := strings.Split(goPath, ";")
	for _, path := range paths {
		path += "/src/github.com/ying32/govcl"
		if fileExists(path) {
			govclPath = path
		}
	}
	if govclPath == "" {
		panic("未在$GOPATH中到找govcl源代码目录，请go get github.com/ying32/govcl")
	}

	functionsMap["MySyscall"] = ""     // 排除此函数，手动构建
	functionsMap["DGetClassName"] = "" // 已经废弃，govcl中也没引用
	// 已经是c语言的了，则不需要这些了
	functionsMap["NSWindow_titleVisibility"] = ""
	functionsMap["NSWindow_setTitleVisibility"] = ""
	functionsMap["NSWindow_titlebarAppearsTransparent"] = ""
	functionsMap["NSWindow_setTitlebarAppearsTransparent"] = ""
	functionsMap["NSWindow_styleMask"] = ""
	functionsMap["NSWindow_setStyleMask"] = ""
	functionsMap["NSWindow_setRepresentedURL"] = ""
	functionsMap["NSWindow_release"] = ""

	// 预定义的基类
	objectFile.BaseObjects = append(objectFile.BaseObjects, []TBaseObject{
		{ClassName: "TObject", BaseClassName: ""},
		{ClassName: "TComponent", BaseClassName: "TObject"},
		{ClassName: "TControl", BaseClassName: "TComponent"},
		{ClassName: "TWinControl", BaseClassName: "TControl"},
	}...)

	// 实例类
	objectFile.InstanceObjects = append(objectFile.InstanceObjects,
		[]TInstanceObject{
			{Name: "Application", Type: "TApplication", InstanceFunc: "Application_Instance"},
			{Name: "Screen", Type: "TScreen", InstanceFunc: "Screen_Instance"},
			{Name: "Mouse", Type: "TMouse", InstanceFunc: "Mouse_Instance"},
			{Name: "Clipboard", Type: "TClipboard", InstanceFunc: "Clipboard_Instance"},
			{Name: "Printer", Type: "TPrinter", InstanceFunc: "Printer_Instance"},
		}...)

	parseFile("LazarusDef.inc", false, nil, "", "")

	// 自动生成的对象函数
	for i := 1; i <= 4; i++ {
		parseClassFiles(fmt.Sprintf("uexport%d.pas", i))
	}

	parseFile("ulinuxpatchs.pas", false, nil, "", "")
	parseFile("umacospatchs.pas", false, nil, "", "")
	parseFile("uformdesignerfile.pas", false, nil, "", "")

	// 事件
	parseEvents(govclPath + "/vcl/events.go")

	// 常量
	parseConst(govclPath + "/vcl/types/colors/colors.go")
	parseConst(govclPath + "/vcl/types/keys/keys.go")
	parseConst(govclPath + "/vcl/types/consts.go")
	parseConst(govclPath + "/vcl/types/cursors.go")

	parseEnums(govclPath + "/vcl/types/enums.go")

	parseBaseType(govclPath+"/vcl/types/types.go", "")
	parseBaseType(govclPath+"/vcl/types/pascal.go", "")
	parseBaseType(govclPath+"/vcl/types/types_386arm.go", "i386")
	parseBaseType(govclPath+"/vcl/types/types_amd64.go", "amd64")
	parseBaseType(govclPath+"/vcl/types/message.go", "i386")
	parseBaseType(govclPath+"/vcl/types/message_posix.go", "amd64")

	fixAndSortObjects()
	// 保存分析文件
	SaveObjectFile("liblcl.json", objectFile)

}

// 把基类放最新面
func fixAndSortObjects() {

	move := func(src, dest int, o TClass) {
		temp := objectFile.Objects[dest] // 备份目标位置的
		objectFile.Objects[dest] = o     // 替换目标位置的为新的
		objectFile.Objects[src] = temp   // 将原来的替换到当前位置
	}

	for i, o := range objectFile.Objects {
		// 这里先修改
		if o.BaseClassName == "TComponent" {
			o.IsComponent = true
		}
		if o.BaseClassName == "TControl" {
			o.IsComponent = true
			o.IsControl = true
		}
		if o.BaseClassName == "TWinControl" {
			o.IsComponent = true
			o.IsControl = true
			o.IsWinControl = true
		}
		objectFile.Objects[i] = o

		if o.ClassName == "TObject" {
			o.BaseClassName = ""
			move(i, 0, o)
		} else if o.ClassName == "TComponent" {
			o.BaseClassName = "TObject"
			move(i, 1, o)
		} else if o.ClassName == "TControl" {
			o.BaseClassName = "TComponent"
			move(i, 2, o)
		} else if o.ClassName == "TWinControl" {
			o.BaseClassName = "TControl"
			move(i, 3, o)
		} else if o.ClassName == "TStringList" {
			o.BaseClassName = "TStrings"
			objectFile.Objects[i] = o
		} else if o.ClassName == "TStrings" {
			o.BaseClassName = "TObject"
			objectFile.Objects[i] = o
		}

	}
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	if err == nil {
		return true
	}
	if os.IsNotExist(err) {
		return false
	}
	return false
}

func readFile(fileName string) ([]byte, error) {
	bs, err := ioutil.ReadFile("../../src/" + fileName)
	if err != nil {
		return nil, err
	}
	return bs, nil
}

func parseFile(fileName string, isClass bool, appendBytes []byte, className, baseClassName string) {
	bs, err := readFile(fileName)
	if err != nil {
		panic(err)
	}

	// {无效参数}
	bs = bytes.Replace(bs, []byte("{无效参数}"), nil, -1)
	bs = bytes.Replace(bs, []byte("\r"), nil, -1)

	// 附加进去
	if len(appendBytes) > 0 {
		bs = append(bs, appendBytes...)
	}

	cmpPrevLine := func(line1, line2, line3, line4 *string, cmpStr string, result *string) bool {
		if strings.HasPrefix(*line1, cmpStr) {
			if result != nil {
				*result = *line1
			}
			return true
		}
		if strings.HasPrefix(*line2, cmpStr) {
			if result != nil {
				*result = *line2
			}
			return true
		}
		if strings.HasPrefix(*line3, cmpStr) {
			if result != nil {
				*result = *line3
			}
			return true
		}
		if strings.HasPrefix(*line4, cmpStr) {
			if result != nil {
				*result = *line4
			}
			return true
		}
		return false
	}

	isEndFlag := func(s string) bool {
		s = strings.TrimSpace(s)
		if strings.HasPrefix(s, "end") || strings.HasPrefix(s, "function") || strings.HasPrefix(s, "procedure") {
			return true
		}
		return false
	}

	sps := bytes.Split(bs, []byte("\n"))
	for i, line := range sps {
		s := string(bytes.TrimSpace(line))
		if (strings.HasPrefix(strings.ToLower(s), "function") || strings.HasPrefix(strings.ToLower(s), "procedure")) && strings.HasSuffix(s, "extdecl;") {
			// 目前最多4行
			prevLine := ""
			prevLine2 := ""
			prevLine3 := ""
			prevLine4 := ""

			eventType := ""
			isLastReturn := false
			isMethod := false
			paramFlags := ""
			lineStr := ""
			if i > 0 {
				// 获取
				prevLine = string(bytes.TrimSpace(sps[i-1]))
				if i > 1 {
					prevLine2 = string(bytes.TrimSpace(sps[i-2]))
				}
				if i > 2 {
					prevLine3 = string(bytes.TrimSpace(sps[i-3]))
				}
				if i > 3 {
					prevLine4 = string(bytes.TrimSpace(sps[i-4]))
				}
				// 进一步处理
				if isEndFlag(prevLine) {
					prevLine = ""
				}
				if isEndFlag(prevLine2) {
					prevLine2 = ""
				}
				if isEndFlag(prevLine3) {
					prevLine3 = ""
				}
				if isEndFlag(prevLine4) {
					prevLine4 = ""
				}
				if cmpPrevLine(&prevLine, &prevLine2, &prevLine3, &prevLine4, "//EVENT_TYPE:", &lineStr) {
					eventType = strings.TrimSpace(strings.TrimPrefix(lineStr, "//EVENT_TYPE:"))
				}
				if cmpPrevLine(&prevLine, &prevLine2, &prevLine3, &prevLine4, "//RETURNISLASTPARAM:", nil) {
					isLastReturn = true
				}
				if cmpPrevLine(&prevLine, &prevLine2, &prevLine3, &prevLine4, "//CLASSMETHOD:", nil) {
					isMethod = true
				}
				if cmpPrevLine(&prevLine, &prevLine2, &prevLine3, &prevLine4, "//PARAMS:", &paramFlags) {
					paramFlags = strings.TrimSpace(strings.TrimPrefix(paramFlags, "//PARAMS:"))
				}
			}
			if !isClass {
				cs := s
				if strings.HasPrefix(s, "procedure") {
					cs = strings.TrimPrefix(cs, "procedure")
				} else if strings.HasPrefix(s, "function") {
					cs = strings.TrimPrefix(cs, "function")
				}
				cs = strings.TrimSpace(cs)
				// 起始不要D开头的，然后不要_Instance结束的
				if cs[0] != 'D' && !strings.Contains(cs, "_Instance") {
					if idx := strings.Index(cs, "_"); idx > 0 &&
						!strings.HasPrefix(cs, "GtkWidget_") &&
						!strings.HasPrefix(cs, "GdkWindow_") &&
						!strings.HasPrefix(cs, "NSWindow_") {
						name := strings.TrimSpace(cs[:idx])
						if name != "Exception" {
							name = "T" + name
						}
						mArr, _ := defClassMethods[name]
						temp := ""

						if eventType != "" || isLastReturn || isMethod {
							if strings.HasPrefix(prevLine4, "//") {
								temp += prevLine4
							}
							temp += "\r\n"
							if strings.HasPrefix(prevLine3, "//") {
								temp += prevLine3
							}
							temp += "\r\n"
							if strings.HasPrefix(prevLine2, "//") {
								temp += prevLine2
							}
							temp += "\r\n"
							if strings.HasPrefix(prevLine, "//") {
								temp += prevLine
							}
							temp += "\r\n"
						}
						temp += s + "\r\nbegin\r\n  handleExceptionBegin\r\n  handleExceptionEnd\r\nend;\r\n"
						mArr = append(mArr, temp)
						defClassMethods[name] = mArr
						continue
					}
				}
			}
			parseFunc(s, isClass, eventType, className, baseClassName, isLastReturn, isMethod, paramFlags)
		}
	}
}

func parseClassFiles(fileName string) {
	bs, err := readFile(fileName)
	if err != nil {
		panic(err)
	}
	bs = bytes.Replace(bs, []byte("\r"), nil, -1)

	matchs := incFileExpr.FindAllStringSubmatch(string(bs), -1)
	for _, match := range matchs {
		if len(match) >= 2 {
			incFileName := strings.TrimSpace(match[1])
			className := strings.TrimPrefix(incFileName, "MyLCL_")
			className = strings.TrimSuffix(className, ".inc")
			if className != "Exception" {
				className = "T" + strings.Trim(className, " ")
			} else {
				className = strings.Trim(className, " ")
			}
			baseClassName := ""
			if len(match) >= 3 {
				baseClassName = strings.TrimSpace(match[2])
			}
			// 后面事件判断是否为类的
			objsMap[className] = className
			// 生成类型定义用的
			classArray = append(classArray, className)
			//fmt.Println(incFileName)

			appendStr := ""
			if mArr, ok := defClassMethods[className]; ok {
				for _, ar := range mArr {
					appendStr += ar
				}
			}
			//rustFile.W("\r\n")
			//rustFile.W("    // " + className)
			//rustFile.W("\r\n")

			parseFile(incFileName, true, []byte(appendStr), className, baseClassName)
			// 当前是TMemoryStream，添加一个TStream的空类型
			if className == "TMemoryStream" {
				objectFile.Objects = append(objectFile.Objects, TClass{ClassName: "TStream", BaseClassName: "TObject"})
			}
			objectFile.Objects = append(objectFile.Objects, currentClass)
		}
	}

}

func isObj(s string) bool {
	if _, ok := objsMap[strings.TrimSpace(s)]; ok {
		return true
	}
	return false
}

var (
	nonWinFunc   bool
	currentClass TClass
)

func parseFunc(s string, isClass bool, eventType, className, baseClassName string, isLastReturn, isMethod bool, paramFlags string) {
	isFunc := strings.HasPrefix(strings.ToLower(s), "function")
	if isFunc {
		s = strings.TrimPrefix(s, "function")
	} else {
		s = strings.TrimPrefix(s, "procedure")
	}
	s = strings.TrimSpace(strings.TrimSuffix(s, "extdecl;"))

	haveParams := strings.Index(s, "(") != -1
	paramsStr := ""
	if haveParams {
		ms := paramsExpr.FindStringSubmatch(s)
		if len(ms) >= 2 {
			paramsStr = strings.TrimSpace(ms[1])

		}
	}

	funcName := ""
	n := strings.Index(s, "(")
	if n == -1 {
		n = strings.Index(s, ":")
	}
	if n != -1 {
		funcName = strings.TrimSpace(s[:n])
	}

	returnType := ""
	if isFunc {
		n := strings.Index(s, ")")
		if n == -1 {
			n = strings.Index(s, ":")
		}
		if n != -1 {
			returnType = strings.TrimSpace(s[n+1:])
			returnType = strings.TrimSuffix(returnType, ";")
			returnType = strings.TrimPrefix(returnType, ":")
			returnType = strings.TrimSpace(returnType)
		}
	}

	params := parseParams(paramsStr, eventType)

	if _, ok := functionsMap[funcName]; ok {
		return
	}
	functionsMap[funcName] = ""

	nameSuffix := func(sName string) bool {
		return strings.HasSuffix(funcName, sName)
	}

	namePreSuffix := func(sName string) bool {
		return strings.HasPrefix(funcName, sName)
	}

	nameEq := func(sName string) bool {
		return funcName == sName
	}

	item := TFunction{}
	item.Name = funcName
	item.IsStatic = !isClass || nameSuffix("_Instance") || nameSuffix("_ClassType")
	item.Return = GetTypes(returnType)
	item.LastIsReturn = isLastReturn
	item.IsMethod = isMethod

	if namePreSuffix("GtkWidget_") || namePreSuffix("GdkWindow_") {
		item.Platform = "linux"
	} else if namePreSuffix("NSWindow_") {
		item.Platform = "macos"
	} else if nameEq("DCreateURLShortCut") || nameEq("DCreateShortCut") {
		item.Platform = "windows"
	} else {
		if nameEq("DSendMessage") {
			nonWinFunc = true
		} else if nameEq("SetEventCallback") { //"DWindowFromPoint") {
			nonWinFunc = false
		}

		if !nonWinFunc {
			item.Platform = "all"
		} else {
			item.Platform = "linux,macos"
		}

	}
	item.Params = params
	// delphi中因为处理了，所以这里不这样返回了
	if item.Return == "datetime" {
		item.LastIsReturn = false
	}
	if paramFlags != "" {
		for _, a := range strings.Split(paramFlags, ",") {
			as := strings.Split(a, "=")
			if len(as) == 2 {
				if idx, err := strconv.Atoi(strings.TrimSpace(as[0])); err == nil {
					item.Params[idx-1].Flag = strings.TrimSpace(as[1])
				}
			}
		}
	}

	// 添加到对象文件中
	if !isClass {
		objectFile.Functions = append(objectFile.Functions, item)
	} else {
		if currentClass.ClassName != className && className != "" {
			currentClass.ClassName = className
			currentClass.BaseClassName = baseClassName
			currentClass.Methods = make([]TFunction, 0)
		}
		temps := className
		if temps != "" {
			if temps[0] == 'T' {
				temps = temps[1:]
			}
			item.RealName = strings.TrimPrefix(item.Name, temps+"_")
		}
		if item.RealName == "StaticClassType" {
			item.IsStatic = true
			item.IsMethod = false
			item.Params = make([]TFuncParam, 0)
		} else if item.RealName == "ClassType" {
			item.IsStatic = false // 这里不知道为啥生成错误
		}

		if item.IsMethod && item.RealName != "" {
			nIdx := item.RealName[len(item.RealName)-1]
			item.IsOverload = nIdx >= '0' && nIdx <= '9'
			if item.IsOverload {
				item.OverloadName = strings.Trim(item.RealName, string(nIdx))
			}
		}
		currentClass.Methods = append(currentClass.Methods, item)
	}
}

func parseParams(s string, eventType string) []TFuncParam {
	if s == "" {
		return nil
	}
	ps := make([]TFuncParam, 0)
	pss := strings.Split(s, ";")
	for _, p := range pss {
		subps := strings.Split(strings.TrimSpace(p), ":")
		if len(subps) >= 2 {

			name := strings.TrimSpace(subps[0])
			typeStr := strings.TrimSpace(subps[1])
			isVar := strings.HasPrefix(name, "var ") || strings.HasPrefix(name, "out ")
			if isVar {
				name = strings.TrimPrefix(name, "var ")
				name = strings.TrimPrefix(name, "out ")

			}
			name = strings.TrimPrefix(name, "const ")
			name = strings.TrimSpace(name)
			// 共用类型的参数
			if strings.Index(name, ",") != -1 {
				ssubps := strings.Split(name, ",")
				for _, sps := range ssubps {
					var item TFuncParam
					item.IsVar = isVar
					item.Name = strings.TrimSpace(sps)
					item.Type = GetTypes(typeStr)
					ps = append(ps, item)
				}

			} else {
				var item TFuncParam
				item.Name = name
				if eventType != "" && name == "AEventId" {
					item.Type = eventType
				} else {
					item.Type = GetTypes(typeStr)
				}
				item.IsVar = isVar
				ps = append(ps, item)
			}
		}
	}
	return ps
}

func parseEnums(fileName string) {

	_, lines, err := readFileLines(fileName)
	if err != nil {
		panic(err)
	}

	i := 0
	for i < len(lines) {
		s := string(bytes.TrimSpace(lines[i]))
		if strings.HasPrefix(s, "type") {

			sp := strings.Split(s, " ")
			prevLine := ""
			if i > 0 {
				prevLine = string(bytes.TrimSpace(lines[i-1]))
			}
			if len(sp) >= 3 {
				item := TType{}
				item.Name = strings.TrimSpace(sp[1])

				if strings.HasPrefix(prevLine, "//ENUM:") {
					item.Kind = "enum"
					//item.Type = strings.TrimSpace(sp[2])
					i++
					find := false
					for i < len(lines) {
						s = string(bytes.TrimSpace(lines[i]))
						if find && s == ")" {
							break
						}

						if strings.HasPrefix(s, "const (") {
							i++
							find = true
							continue
						}

						if s == "" || s == "//" || strings.HasPrefix(s, "{$") || strings.HasPrefix(s, "//") ||
							strings.HasPrefix(s, "/*") {
							i++
							continue
						}

						if find {

							eqPos := strings.Index(s, " = ")
							cPos := strings.Index(s, "//")

							eItem := TEnum{}

							// 找到一个等号，并且不在注释中
							if eqPos != -1 && (eqPos < cPos || cPos == -1) {
								sArr := strings.Split(s, " = ")

								eItem.Name = strings.TrimSpace(sArr[0])
								if cPos != -1 {
									sArr = strings.Split(sArr[1], "//")
									eItem.Value = strings.TrimSpace(sArr[0])
									eItem.Comment = strings.TrimSpace(sArr[1])
								} else {
									eItem.Value = strings.TrimSpace(sArr[1])
								}

							} else {
								if cPos != -1 {
									sArr := strings.Split(s, "//")
									eItem.Name = strings.TrimSpace(sArr[0])
									eItem.Comment = strings.TrimSpace(sArr[1])
								} else {
									eItem.Name = strings.TrimSpace(s)
								}
							}
							eItem.Name = firstLowerChar(eItem.Name)
							if eItem.Value == "iota + 0" {
								eItem.Value = ""
							}
							pi := strings.Index(eItem.Value, "(")
							if pi != -1 {
								eItem.Value = eItem.Value[pi+1:]
								pi = strings.Index(eItem.Value, ")")
								if pi != -1 {
									eItem.Value = eItem.Value[:pi]
								}
							}
							item.Enums = append(item.Enums, eItem)

						}
						i++
					}

				} else if strings.HasPrefix(prevLine, "//SET:") {
					item.Kind = "set"
					item.SetOf = strings.TrimSpace(strings.TrimPrefix(prevLine, "//SET:"))
				} else {
					item.Kind = "type"
					item.Type = strings.TrimSpace(sp[2])
					if item.Type == "=" {
						item.Type = strings.TrimSpace(sp[3])
					}
				}
				// 转换类型
				item.Type = GetTypes(item.Type)

				objectFile.Types = append(objectFile.Types, item)
			}
		}
		i++
	}

}

func parseEvents(fileName string) {
	_, lines, err := readFileLines(fileName)
	if err != nil {
		panic(err)
	}

	eventTypeIsPtr := func(tt string) bool {
		if tt == "TRect" || tt == "TPoint" || tt == "TSize" {
			return true
		}
		return false
	}

	for _, line := range lines {
		s := string(bytes.TrimSpace(line))
		if strings.HasPrefix(s, "type ") {
			s = strings.TrimPrefix(s, "type ")
			// 干掉注释
			if i := strings.Index(s, "//"); i != -1 {
				s = s[:i]
			}
			if i := strings.Index(s, "/*"); i != -1 {
				s = s[:i]
			}
			s = strings.TrimSpace(s)
			isFunc := false
			// 检测是否有返回值，有则移到参数后面
			if i := strings.Index(s, ")"); i != -1 {
				retVal := strings.TrimSpace(s[i+1:])
				if len(retVal) > 3 {
					s = s[:i] + ", result " + retVal + ")" // 重新处理这个返回值
					isFunc = true
				}
			}

			// type TLVOwnerDataHintEvent = TLVDataHintEvent
			// 处理上面这种情况
			if strings.Index(s, "=") != -1 {
				sp := strings.Split(s, "=")
				item := TEvent{}
				item.Name = strings.TrimSpace(sp[0])
				item.ReDefine = strings.TrimSpace(sp[1])
				objectFile.Events = append(objectFile.Events, item)
				continue
			}

			idx := strings.Index(s, " ")
			name := s[:idx]
			body := strings.TrimRight(strings.TrimPrefix(s[idx+1:], "func("), ")")
			params := make([]TFuncParam, 0)
			for _, ps := range strings.Split(body, ",") {
				ps = strings.TrimSpace(ps)
				subps := strings.Split(ps, " ")
				item := TFuncParam{}
				if len(subps) >= 1 {
					item.Name = strings.TrimSpace(subps[0])
				}
				if len(subps) >= 2 {
					item.Type = strings.Trim(strings.TrimSpace(subps[1]), "*")
					starCount := strings.Count(strings.TrimSpace(subps[1]), "*")
					if starCount > 0 {
						item.IsVar = true
					}
					if isObj(item.Type) && starCount < 2 {
						item.IsVar = false
					}
					//item.IsVar = strings.HasPrefix(strings.TrimSpace(subps[1]), "*")
					item.IsArray = strings.HasPrefix(strings.TrimSpace(subps[1]), "[]")
				}
				if item.IsArray {
					item.Type = GetTypes("pointer") //strings.TrimPrefix(item.Type, "[]")
				}
				// 转换类型
				item.Type = GetTypes(item.Type)

				if item.Name != "" {
					params = append(params, item)
				}

				// 如果上个参数是一个数组，则添加一个数组长度参数
				if item.IsArray {
					item := TFuncParam{}
					item.Name = "len"
					// 转换类型
					item.Type = GetTypes("int")
					params = append(params, item)
				}
			}
			// 处理参数，将所有参数的类型都补上
			lastType := ""
			lastIsVar := false
			for i := len(params) - 1; i >= 0; i-- {
				item := params[i]
				if item.Type == "" {
					item.IsVar = lastIsVar
					item.Type = lastType
					params[i] = item
				}
				// 处理一些，有些是用指针传递的，但实际使用中不以指针表示
				if !item.IsVar && eventTypeIsPtr(item.Type) {
					item.IsVar = true
					params[i] = item
				}
				lastType = item.Type
				lastIsVar = item.IsVar
			}

			item := TEvent{}
			item.Name = name
			item.Params = params
			item.IsFunc = isFunc

			objectFile.Events = append(objectFile.Events, item)

		}
	}
}

func parseConst(filename string) {

	_, lines, err := readFileLines(filename)
	if err != nil {
		panic(err)
	}
	i := 0

	addComment := func(s string) {
		// 注释也收集
		if strings.HasPrefix(s, "//") || strings.HasPrefix(s, "/*") {

			item := TConst{}
			item.Name = ""
			item.Value = ""
			item.Comment = strings.Replace(s, "//", "", -1)
			item.Comment = strings.Replace(item.Comment, "/*", "", -1)
			item.Comment = strings.TrimSpace(strings.Replace(item.Comment, "*/", "", -1))
			objectFile.Consts = append(objectFile.Consts, item)

		}
	}

	for i < len(lines) {
		s := string(bytes.TrimSpace(lines[i]))

		if strings.HasPrefix(s, "const (") {
			addComment(string(bytes.TrimSpace(lines[i-1])))
			i++
			for s != ")" {
				s = string(bytes.TrimSpace(lines[i]))
				// 注释也收集
				if strings.HasPrefix(s, "//") || strings.HasPrefix(s, "/*") {
					addComment(s)

				} else {
					ss := strings.Split(s, "=")
					if len(ss) == 2 {
						item := TConst{}
						item.Name = strings.TrimSpace(ss[0])
						if !strings.HasPrefix(item.Name, "CF_") {
							item.Name = firstLowerChar(item.Name)
						}
						ssArr := strings.Split(ss[1], "//")
						if len(ssArr) >= 2 {
							item.Value = strings.TrimSpace(ssArr[0])
							item.Comment = strings.TrimSpace(ssArr[1])
						} else {
							item.Value = strings.TrimSpace(ss[1])
						}
						if strings.Contains(item.Value, "+") {
							ssArr = strings.Split(item.Value, "+")
							item.Value = firstLowerChar(strings.TrimSpace(ssArr[0]))
							item.Value2 = strings.TrimSpace(ssArr[1])
						} else {
							if !strings.Contains(item.Value, "(") && !strings.Contains(item.Value, ")") {
								item.Value = firstLowerChar(item.Value)
							}

						}
						objectFile.Consts = append(objectFile.Consts, item)
					}
				}
				i++
			}
			continue
		}
		i++
	}
}

func parseBaseType(filename, arch string) {

	_, lines, err := readFileLines(filename)
	if err != nil {
		panic(err)
	}
	i := 0
	for i < len(lines) {
		s := string(bytes.TrimSpace(lines[i]))
		if strings.HasPrefix(s, "type") {

			sArr := strings.Split(strings.TrimPrefix(s, "type "), " ")
			item := TType{}
			item.Kind = "type"
			item.FieldArch = arch
			if len(sArr) >= 2 {
				item.Name = strings.TrimSpace(sArr[0])
				s2 := strings.TrimSpace(sArr[1])
				if s2 == "struct" {
					item.Kind = "struct"
					// 解析结构
					i++
					for i < len(lines) {
						s = string(bytes.TrimSpace(lines[i]))
						if s == "}" {
							break
						}
						if s == "" || strings.HasPrefix(s, "//") || strings.HasPrefix(s, "/*") {
							i++
							continue
						}
						sArr2 := strings.Split(s, " ")
						if len(sArr2) >= 2 {
							field := TField{}
							field.Name = firstLowerChar(strings.TrimSpace(sArr2[0]))
							pi := strings.Index(sArr2[1], "//")
							if pi != -1 {
								field.Type = strings.TrimSpace(sArr2[1][:pi])
								field.Comment = strings.TrimSpace(sArr2[1][pi+3:])
							} else {
								field.Type = strings.TrimSpace(sArr2[len(sArr2)-1])
							}
							// 判断是否数组
							field.IsArr = strings.HasPrefix(field.Type, "[")
							if field.IsArr {
								p1, p2 := strings.Index(field.Type, "["), strings.Index(field.Type, "]")
								field.ArrLength, _ = strconv.Atoi(field.Type[p1+1 : p2])
								field.Type = field.Type[p2+1:]
							}
							// 转换类型
							field.Type = GetTypes(field.Type)

							item.Fields = append(item.Fields, field)
						}
						i++
					}

				} else {
					if s2 == "=" {
						item.Type = strings.TrimSpace(sArr[2])
					} else {
						item.Type = s2
					}
				}
			}
			// 转换类型
			item.Type = GetTypes(item.Type)

			if i > 0 && item.Kind != "struct" {
				prevLine := string(bytes.TrimSpace(lines[i-1]))
				if strings.HasPrefix(prevLine, "//") {
					item.Comment = strings.TrimSpace(strings.TrimPrefix(prevLine, "//"))
				}
			}
			if item.Name != "" {
				objectFile.BaseTypes = append(objectFile.BaseTypes, item)
			}
		}
		i++
	}
}

func firstLowerChar(sx string) string {
	sx = strings.TrimSpace(sx)
	if len(sx) > 0 {
		sx = strings.ToLower(string(sx[0])) + string(sx[1:])
	}

	return sx
}

func readFileLines(filename string) (*bytes.Buffer, [][]byte, error) {
	buff := bytes.NewBuffer(nil)
	buff.WriteString("//" + filename)
	buff.WriteString("\n")
	bs, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Println(err)
		return nil, nil, err
	}
	return buff, bytes.Split(bs, []byte("\n")), nil
}
