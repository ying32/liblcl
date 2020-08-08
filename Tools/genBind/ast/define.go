//----------------------------------------
//
// Copyright © ying32. All Rights Reserved.
//
// Licensed under Apache License 2.0
//
//----------------------------------------

package ast

import (
	"encoding/json"
	"io/ioutil"
	"strings"
)

type (
	// 参数定义
	TFuncParam struct {
		Name    string `json:"name"`     // 参数名
		Type    string `json:"type"`     // 参数类型
		IsVar   bool   `json:"is_var"`   // 是否为一个var，或者说是指针类型
		IsArray bool   `json:"is_array"` // 类型是否为一个数组
		Flag    string `json:"flag"`     // 参数标识： nonPtr：当isvar为true是，如果此标识为nonPtr则表示方法上不需要生成带有指针的
		IsEvent bool   `json:"is_event"` // 此参数类型是否为一个事件的
	}

	// 函数定义
	TFunction struct {
		Name         string       `json:"name"`           // 函数全名，如：Application_SetTitle
		RealName     string       `json:"real_name"`      // 实际名称，比如类中的 Application_SetTitle，则RealName为 SetTitle
		Return       string       `json:"return"`         // 返回值，如果没有返回值则为空
		Params       []TFuncParam `json:"params"`         // 参数列表
		IsStatic     bool         `json:"is_static"`      // 是否静态，如果是类相关的，则不需要Obj
		Platform     string       `json:"platform"`       // 此函数可应用的平台 all  linux windows macos
		LastIsReturn bool         `json:"last_is_return"` // 是否使用最后一个参数作为返回值
		IsMethod     bool         `json:"is_method"`      // 是否为一个方法
		IsProp       bool         `json:"is_prop"`        // 是否为一个属性
		IsSetter     bool         `json:"is_setter"`      // 属性的Setter
		IsGetter     bool         `json:"is_getter"`      // 属性的Getter
		IsSetEvent   bool         `json:"is_set_event"`   // 是否为一个SetOn的方法
		IsOverload   bool         `json:"is_overload"`    // 是否为重载函数
		OverloadName string       `json:"overload_name"`  // 重载函数实际名，没有重载索引的
		PropName     string       `json:"prop_name"`      // 当IsProp时，此名称为除掉起始Set或者Get
	}

	TClass struct {
		ClassName     string      `json:"class_name"`      // 类名
		BaseClassName string      `json:"base_class_name"` // 继承自指定类名
		IsComponent   bool        `json:"is_component"`    // 是否继承自TComponent
		IsControl     bool        `json:"is_control"`      // 是否继承自TControl
		IsWinControl  bool        `json:"is_win_control"`  // 是否继承自TWinControl
		Methods       []TFunction `json:"functions"`       // 这个类中的方法、属性列表
	}

	TEvent struct {
		Name     string       `json:"name"`      // 事件名
		ReDefine string       `json:"re_define"` // 被重定义的事件名， XXX = XX
		IsFunc   bool         `json:"is_func"`   // 是否为一个函数，为一个函数要展开为一个过程，看情况了
		Params   []TFuncParam `json:"params"`    // 参数列表
	}

	TConst struct {
		Name    string `json:"name"`    // 常量名
		Value   string `json:"value"`   // 常量的值
		Value2  string `json:"value_2"` // 第二个值 xx + xx
		Comment string `json:"comment"` // 注释
	}

	TEnum struct {
		Name    string `json:"name"`    // 枚举名
		Value   string `json:"value"`   // 枚举值
		Comment string `json:"comment"` // 注释
	}

	TField struct {
		Name      string `json:"name"`       // 结构的成员名
		Type      string `json:"type"`       // 类型
		IsArr     bool   `json:"is_arr"`     // 是否为数组
		ArrLength int    `json:"arr_length"` // 数组长度
		Comment   string `json:"comment"`    // 注释
	}

	TType struct {
		Name      string   `json:"name"`       // 类型的名称
		Type      string   `json:"type"`       // 类型
		Kind      string   `json:"kind"`       // 分类，set const enum struct
		Enums     []TEnum  `json:"enums"`      // 当Kind为enum时
		SetOf     string   `json:"set_of"`     // 当Kind为set时，SetOf为目标值，
		Fields    []TField `json:"fields"`     // 当Kind为struct时，成员列表
		FieldArch string   `json:"field_arch"` // 类型可用于哪种arch的： i386  amd64
		Comment   string   `json:"comment"`    // 注释
	}

	TBaseObject struct {
		ClassName     string `json:"class_name"`      // 类名
		BaseClassName string `json:"base_class_name"` // 继承自
	}

	TInstanceObject struct {
		Name         string `json:"name"`          // 实例名
		Type         string `json:"type"`          // 实列类型
		InstanceFunc string `json:"instance_func"` // 获取实例的名称
	}

	TObjectFile struct {
		Functions       []TFunction       `json:"functions"`
		Objects         []TClass          `json:"objects"`
		Events          []TEvent          `json:"events"`
		Consts          []TConst          `json:"consts"`
		Types           []TType           `json:"types"`
		BaseTypes       []TType           `json:"base_types"`
		BaseObjects     []TBaseObject     `json:"base_objects"`
		InstanceObjects []TInstanceObject `json:"instance_objects"`
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
	"TPersistent":          "TObject",
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
	"TStream":              "TStream",
	"IStream":              "TStream",
	"TStrings":             "TStrings",
	"IStrings":             "TStrings",
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
