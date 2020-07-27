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
	"fmt"
	"html/template"
	"io/ioutil"

	"./ast"
)

type TFile struct {
	TemplateFileName string `json:"template_file_name"`
	SaveFileName     string `json:"save_file_name"`
	Enabled          bool   `json:"enabled"`
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
	Name        string            `json:"name"`
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
	// 添加一个
	Objs["TStream"] = "TObject"
	for _, o := range objectFile.Objects {
		dict := make(map[string]string, 0)
		for _, m := range o.Methods {
			dict[m.RealName] = ""
		}
		methodDict[o.ClassName] = dict
	}

	for _, lang := range conf.Languages {
		if !lang.Enabled {
			fmt.Println(lang.Name, "语言未启用，跳过生成。")
		} else {
			fmt.Println("正在生成", lang.Name, "语言的代码...")
		}
		if lang.Enabled {
			typeDict = lang.TypeDict
			keyWordsDict = lang.KeywordDict
			for _, file := range lang.Files {
				if !file.Enabled {
					fmt.Println("模板文件：", file.TemplateFileName, " 未启用，跳过...")
					continue
				}
				fmt.Println("执行模板文件：", file.TemplateFileName, "，保存目标文件：", file.SaveFileName)
				execTemplate(objectFile, file, lang.LineBreak)
			}
		}
	}
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
