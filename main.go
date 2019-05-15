package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/iawia002/godoc/formatter/markdown"
	"github.com/iawia002/godoc/generator"
)

func fatal(err error) {
	fmt.Println(err)
	os.Exit(1)
}

func main() {
	flag.Parse()
	args := flag.Args()

	for _, path := range args {
		pkg, err := generator.NewPackage(path)
		if err != nil {
			fatal(err)
		}

		formatter := markdown.New(pkg)
		s, err := formatter.Format()
		if err != nil {
			fatal(err)
		}
		if err := ioutil.WriteFile(fmt.Sprintf("%s.md", pkg.BuildPackage.Name), []byte(s), 0644); err != nil {
			fatal(err)
		}
	}
}
