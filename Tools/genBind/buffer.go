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
	"fmt"
	"html/template"
)

type TMyBuffer struct {
	buff *bytes.Buffer
}

func (b *TMyBuffer) Write(ss ...interface{}) string {
	fmt.Fprint(b.buff, ss...)
	return ""
}

func (b *TMyBuffer) Writeln(ss ...interface{}) string {
	b.Write(ss...)
	b.buff.WriteByte('\r')
	b.buff.WriteByte('\n')
	return ""
}

func (b *TMyBuffer) ToStr() template.HTML {
	return template.HTML(b.buff.String())
}

func (b *TMyBuffer) Clear() string {
	b.buff.Reset()
	return ""
}
