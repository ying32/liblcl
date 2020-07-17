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
		Objs[o.ClassName] = ""
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

var templateFuncs = template.FuncMap{
	"isEmpty":    templateIsEmpty,
	"covType":    templateCovType,
	"isObject":   templateIsObject,
	"Dec":        templateDec,
	"covKeyword": templateCovKeyWord,
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
	err = ioutil.WriteFile(file.SaveFileName, buff.Bytes(), 0774)
	if err != nil {
		panic(err)
	}
}
