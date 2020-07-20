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
	"encoding/json"
	"flag"
	"html/template"
	"io/ioutil"
	"strings"

	"./ast"
)

type TFile struct {
	TemplateFileName string `json:"template_file_name"`
	SaveFileName     string `json:"save_file_name"`
}

type TTypeDict struct {
	Name string `json:"name"`
	Val  string `json:"val"`
}

type TKeywordDict struct {
	Name string `json:"name"`
	Val  string `json:"val"`
}

type TLanguages struct {
	Enabled     bool              `json:"enabled"`
	LineBreak   string            `json:"line_break"`
	TypeDict    map[string]string `json:"type_dict"`
	KeywordDict map[string]string `json:"keyword_dict"`
	Files       []TFile           `json:"files"`
}

type TConfig struct {
	ASTFileName string       `json:"ast_file_name"`
	Languages   []TLanguages `json:"languages"`
}

var (
	typeDict     = map[string]string{}
	keyWordsDict = map[string]string{}
	// 方法字典，用于判断当前方法是基类是否有的，会递归一直找到TObject
	methodDict = map[string]map[string]string{}
	// 用作导入时判断类型是否为对象的
	Objs = map[string]string{}

	// 命令行
	genAST       = flag.Bool("ast", false, "生成AST文件'liblcl.json'。")
	afterGenBind = flag.Bool("bind", false, "生成AST后接着生成绑定代码，需要与'--ast'一起使用才生效。")
)

func main() {
	flag.Parse()
	if *genAST {
		ast.GenAst()
		if !*afterGenBind {
			return
		}
	}
	// 读配置
	bs, err := ioutil.ReadFile("app.json")
	var conf TConfig
	if err != nil {
		panic(err)
	}
	err = json.Unmarshal(bs, &conf)
	if err != nil {
		panic(err)
	}

	// 读AST文件
	var objectFile ast.TObjectFile
	err = ast.ReadObjectFile(conf.ASTFileName, &objectFile)
	if err != nil {
		panic(err)
	}
	// 添加对象
	for _, o := range objectFile.Objects {
		Objs[o.ClassName] = o.BaseClassName
	}
	for _, o := range objectFile.Objects {
		dict := make(map[string]string, 0)
		for _, m := range o.Methods {
			dict[m.RealName] = ""
		}
		methodDict[o.ClassName] = dict
	}

	for _, lang := range conf.Languages {
		if lang.Enabled {
			typeDict = lang.TypeDict
			keyWordsDict = lang.KeywordDict
			for _, file := range lang.Files {
				execTemplate(objectFile, file, lang.LineBreak)
			}
		}
	}
}

func templateIsEmpty(s string) bool {
	return s == ""
}

func templateCovType(s string) string {
	if v, ok := typeDict[s]; ok {
		return v
	}
	return s
}

// 不包含字符串的转换
func templateCovType2(s string) string {
	if s == "string" {
		return s
	}
	if v, ok := typeDict[s]; ok {
		return v
	}
	return s
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

func templateGetPropRealName(mm ast.TFunction) string {
	if !mm.IsMethod && (strings.HasPrefix(mm.RealName, "Get") || strings.HasPrefix(mm.RealName, "Set")) {
		return mm.RealName[3:]
	}
	return mm.RealName
}

var templateFuncs = template.FuncMap{
	"isEmpty":         templateIsEmpty,
	"covType":         templateCovType,
	"covType2":        templateCovType2,
	"isObject":        templateIsObject,
	"Dec":             templateDec,
	"covKeyword":      templateCovKeyWord,
	"nextType":        templateNextType,
	"prevType":        templatePrevType,
	"isBaseObj":       templateIsBaseObject,
	"rmObjectT":       templateRMObjectT,
	"paramsEmpty":     templateParamsEmpty,
	"propGetName":     templatePropGetName,
	"isBaseMethod":    templateIsBaseMethod,
	"html":            templateText,
	"cPsZero":         templateCPsZero,
	"delDChar":        templateDelDChar,
	"lastParam":       templateGetLastParam,
	"canOutParam":     templateCanOutParam,
	"hasPrefix":       strings.HasPrefix,
	"hasSuffix":       strings.HasSuffix,
	"trim":            strings.TrimSpace,
	"isProp":          templateIsProp,
	"isGetter":        templateIsGetter,
	"isSetter":        templateIsSetter,
	"getPropRealName": templateGetPropRealName,
}

func execTemplate(objFile ast.TObjectFile, file TFile, lineBreak string) {

	tplFileBs, err := ioutil.ReadFile(file.TemplateFileName)
	if err != nil {
		panic(err)
	}
	tpl := template.New("").Funcs(templateFuncs)

	_, err = tpl.Parse(string(tplFileBs))
	if err != nil {
		panic(err)
	}
	buff := bytes.NewBuffer(nil)
	err = tpl.Execute(buff, objFile)
	if err != nil {
		panic(err)
	}
	spFlag := []byte("##")
	bsArr := bytes.Split(bytes.Replace(buff.Bytes(), []byte("\r"), nil, -1), []byte("\n"))
	buff.Reset()

	for i := 0; i < len(bsArr); i++ {
		s := bsArr[i]
		if len(bytes.TrimSpace(s)) != 0 {
			// 是##标识，替换为空的
			if bytes.Compare(bytes.TrimSpace(s), spFlag) == 0 {
				buff.WriteByte('\n')
			} else {
				buff.Write(s)
				buff.WriteByte('\n')
			}
		}
	}
	bs := buff.Bytes()
	if lineBreak != "" {
		bs = bytes.Replace(bs, []byte("\n"), []byte(lineBreak), -1)
	}
	err = ioutil.WriteFile(file.SaveFileName, bs, 0774)
	if err != nil {
		panic(err)
	}
}
