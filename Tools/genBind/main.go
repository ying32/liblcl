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
	"html/template"
	"io/ioutil"
	"strings"

	"../genAST/define"
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
)

func main() {
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
	var objectFile define.TObjectFile
	err = define.ReadObjectFile(conf.ASTFileName, &objectFile)
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

		typeDict = lang.TypeDict
		keyWordsDict = lang.KeywordDict

		for _, file := range lang.Files {
			execTemplate(objectFile, file)
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
	_, ok := Objs[strings.TrimSpace(s)]
	return ok
}

func templateDec(val int) int {
	return val - 1
}

func templateNextType(obj []define.TType, idx int) define.TType {
	if idx+1 < len(obj) {
		return obj[idx+1]
	}
	return define.TType{}
}

func templatePrevType(obj []define.TType, idx int) define.TType {
	if idx-1 > 0 {
		//fmt.Println("index:", idx, obj[idx-1], obj[idx])
		return obj[idx-1]
	}
	return define.TType{}
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

func templateParamsEmpty(ps []define.TFuncParam) bool {
	return len(ps) == 0
}

func templatePropGetName(fn define.TFunction) string {
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
}

func execTemplate(objFile define.TObjectFile, file TFile) {

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
	endFlag := []byte("#--end--")
	spFlag := []byte("##")
	bsArr := bytes.Split(bytes.Replace(buff.Bytes(), []byte("\r"), nil, -1), []byte("\n"))
	buff.Reset()

	for i := 0; i < len(bsArr); i++ {
		s := bsArr[i]
		if bytes.HasSuffix(bytes.TrimSpace(s), endFlag) {
			buff.WriteByte('\n')
		}
		if len(bytes.TrimSpace(s)) != 0 {
			// 是##标识，替换为空的
			if bytes.Compare(bytes.TrimSpace(s), spFlag) == 0 {
				buff.WriteByte('\n')
			} else {
				buff.Write(bytes.Replace(s, endFlag, nil, 1))
				buff.WriteByte('\n')
			}
		}
	}

	err = ioutil.WriteFile(file.SaveFileName, buff.Bytes(), 0774)
	if err != nil {
		panic(err)
	}
}
