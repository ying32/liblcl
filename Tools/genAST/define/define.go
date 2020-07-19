//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Apache License 2.0
//
//----------------------------------------

package define

import (
	"encoding/json"
	"io/ioutil"
	"strings"
)

type (
	// 参数定义
	TFuncParam struct {
		Name    string `json:"name"`
		Type    string `json:"type"`
		IsVar   bool   `json:"is_var"`
		IsArray bool   `json:"is_array"`
	}

	// 函数定义
	TFunction struct {
		Name         string       `json:"name"`
		RealName     string       `json:"real_name"`
		Return       string       `json:"return"`
		Params       []TFuncParam `json:"params"`
		IsStatic     bool         `json:"is_static"`
		Platform     string       `json:"platform"`
		LastIsReturn bool         `json:"last_is_return"`
		IsMethod     bool         `json:"is_method"` // 不是方法就是属性
	}

	TClass struct {
		ClassName     string      `json:"class_name"`
		BaseClassName string      `json:"base_class_name"`
		Methods       []TFunction `json:"functions"`
	}

	TEvent struct {
		Name     string       `json:"name"`
		ReDefine string       `json:"re_define"`
		Params   []TFuncParam `json:"params"`
	}

	TConst struct {
		Name    string `json:"name"`
		Value   string `json:"value"`
		Value2  string `json:"value_2"`
		Comment string `json:"comment"`
	}

	TEnum struct {
		Name    string `json:"name"`
		Value   string `json:"value"`
		Comment string `json:"comment"`
	}

	TField struct {
		Name      string `json:"name"`
		Type      string `json:"type"`
		IsArr     bool   `json:"is_arr"`
		ArrLength int    `json:"arr_length"`
		Comment   string `json:"comment"`
	}

	TType struct {
		Name      string   `json:"name"`
		Type      string   `json:"type"`
		Kind      string   `json:"kind"`
		Enums     []TEnum  `json:"enums"`
		SetOf     string   `json:"set_of"`
		Fields    []TField `json:"fields"`
		FieldArch string   `json:"field_arch"`
		Comment   string   `json:"comment"`
	}

	TBaseObject struct {
		ClassName     string `json:"class_name"`
		BaseClassName string `json:"base_class_name"`
	}

	TObjectFile struct {
		Functions   []TFunction   `json:"functions"`
		Objects     []TClass      `json:"objects"`
		Events      []TEvent      `json:"events"`
		Consts      []TConst      `json:"consts"`
		Types       []TType       `json:"types"`
		BaseTypes   []TType       `json:"base_types"`
		BaseObjects []TBaseObject `json:"base_objects"`
	}
)

func SaveObjectFile(fileName string, obj TObjectFile) error {
	bs, err := json.MarshalIndent(obj, "", "\t")
	if err != nil {
		return err
	}
	return ioutil.WriteFile(fileName, bs, 0665)
}

func ReadObjectFile(fileName string, out interface{}) error {
	bs, err := ioutil.ReadFile(fileName)
	if err != nil {
		return err
	}
	return json.Unmarshal(bs, out)
}

// 统一基础类型，这里基本采用rust的类型规则，但也不是全部
var baseTypes = map[string]string{
	"Cardinal":             "u32",
	"PChar":                "string",
	"Boolean":              "bool",
	"LongBool":             "bool", // u32
	"Pointer":              "pointer",
	"NativeInt":            "isize", //x64=8, x86=4
	"NativeUInt":           "usize",
	"Integer":              "i32",
	"Longint":              "i32",
	"int32":                "i32",
	"uintptr":              "usize",
	"uint64":               "u64",
	"UInt64":               "u64",
	"int64":                "i64",
	"Int64":                "i64",
	"Single":               "f32",
	"Double":               "f64",
	"float32":              "f32",
	"float64":              "f64",
	"uint32":               "u32",
	"uint16":               "u16",
	"int16":                "i16",
	"int8":                 "i8",
	"uint8":                "u8",
	"Word":                 "u16",
	"IObject":              "TObject",
	"IControl":             "TControl",
	"IWinControl":          "TWinControl",
	"IComponent":           "TComponent",
	"TBasicAction":         "TAction",
	"TPersistent":          "pointer",
	"Byte":                 "i8",
	"TBorderWidth":         "i32",
	"TCustomImageList":     "TImageList",
	"TUnixDateTime":        "datetime",
	"TCustomListView":      "TListView",
	"TCustomTreeView":      "TTreeView",
	"TImageIndex":          "i32",
	"SmallInt":             "i16",
	"TCustomForm":          "TForm",
	"TWidth":               "i32",
	"LongWord":             "u32",
	"PPointerList":         "pointer",
	"TCustomHeaderControl": "THeaderControl",
	"TCollectionItemClass": "pointer",
	"TOverlay":             "u8",
	"TGoForm":              "TForm",
	"TLMessage":            "TMessage",
	"BOOL":                 "bool",
	"TStream":              "TObject",
	"WParam":               "WPARAM",
	"LParam":               "LPARAM",
	"LResult":              "LRESULT",
	"NaturalNumber":        "i32",
	"TCustomGrid":          "TStringGrid",
}

func GetTypes(s string) string {
	s = strings.TrimSpace(s)
	if v, ok := baseTypes[s]; ok {
		return v
	}
	return s
}
