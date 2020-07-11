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
	"io/ioutil"
)

type File struct {
	buff     *bytes.Buffer
	fileName string
	replaces []struct {
		name string
		data []byte
	}
}

func NewFile(fileName string) *File {
	f := new(File)
	f.buff = bytes.NewBuffer(nil)
	f.fileName = fileName
	return f
}

func (c *File) W(s string) {
	c.buff.WriteString(s)
}

func (c *File) WLn() {
	c.buff.WriteString("\n")
}

func (c *File) String() string {
	return c.buff.String()
}

func (c *File) WComment(s string) {
	c.WLn()
	c.W("// " + s)
	c.WLn()
}

func (c *File) Save() error {
	bs := c.buff.Bytes()
	for _, x := range c.replaces {
		bs = bytes.Replace(bs, []byte(x.name), x.data, 1)
	}
	bs = bytes.Replace(bs, []byte("\n"), []byte("\r\n"), -1)
	return ioutil.WriteFile(c.fileName, bs, 0664)

}

func (c *File) AddReplaceFlag(flag string, data []byte) {
	c.replaces = append(c.replaces, struct {
		name string
		data []byte
	}{name: fmt.Sprintf("<%%%s%%>", flag), data: data})
}
