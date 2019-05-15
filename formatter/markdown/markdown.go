package markdown

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/doc"
	"log"

	"github.com/iawia002/godoc/formatter"
	"github.com/iawia002/godoc/generator"
)

const (
	punchedCardWidth = 80 // These things just won't leave us alone.
	indentedWidth    = punchedCardWidth - len(indent)
	indent           = "    "
)

type markdown struct {
	pkg *generator.Package
	buf bytes.Buffer
}

func New(pkg *generator.Package) *markdown {
	return &markdown{
		pkg: pkg,
	}
}

func (f *markdown) printf(format string, args ...interface{}) {
	fmt.Fprintf(&f.buf, format, args...)
}

var newlineBytes = []byte("\n\n") // We never ask for more than 2.

// newlines guarantees there are n newlines at the end of the buffer.
func (f *markdown) newlines(n int) {
	for !bytes.HasSuffix(f.buf.Bytes(), newlineBytes[:n]) {
		f.buf.WriteRune('\n')
	}
}

// emit prints the node. If showSrc is true, it ignores the provided comment,
// assuming the comment is in the node itself. Otherwise, the go/doc package
// clears the stuff we don't want to print anyway. It's a bit of a magic trick.
func (f *markdown) emit(comment string, node ast.Node) error {
	nodeString, err := f.pkg.Emit(node)
	if err != nil {
		return err
	}

	f.printf("```go\n%s\n```\n\n", nodeString)
	if comment != "" {
		f.newlines(1)
		doc.ToText(&f.buf, comment, "", indent, indentedWidth)
		f.newlines(2) // Blank line after comment to separate from next item.
	} else {
		f.newlines(1)
	}
	return nil
}

// Format prints formated docs for the package.
func (f *markdown) Format() (string, error) {
	// head
	f.printf("# package %s\n\n", f.pkg.BuildPackage.Name)
	f.printf("import %q\n\n", f.pkg.Path)

	// package doc
	doc.ToText(&f.buf, f.pkg.Doc.Doc, "", indent, indentedWidth)
	f.newlines(1)

	printed := make(map[*ast.GenDecl]bool)

	hdr := ""
	printHdr := func(s string) {
		if hdr != s {
			f.printf("\n%s\n\n", s)
			hdr = s
		}
	}

	// Constants.
	for _, value := range f.pkg.Doc.Consts {
		// Constants and variables come in groups, and valueDoc prints
		// all the items in the group. We only need to find one exported symbol.
		for _, name := range value.Names {
			if formatter.IsExported(name) && !f.pkg.TypedValue[value] {
				printHdr("## Constants")
				if err := f.valueDoc(value, printed); err != nil {
					return "", err
				}
				break
			}
		}
	}

	// Variables.
	for _, value := range f.pkg.Doc.Vars {
		// Constants and variables come in groups, and valueDoc prints
		// all the items in the group. We only need to find one exported symbol.
		for _, name := range value.Names {
			if formatter.IsExported(name) && !f.pkg.TypedValue[value] {
				printHdr("## Variables")
				if err := f.valueDoc(value, printed); err != nil {
					return "", err
				}
				break
			}
		}
	}

	// Functions.
	for _, fun := range f.pkg.Doc.Funcs {
		if formatter.IsExported(fun.Name) && !f.pkg.Constructor[fun] {
			printHdr("## Functions")
			f.printf("### func %s\n\n", fun.Name)
			if err := f.emit(fun.Doc, fun.Decl); err != nil {
				return "", err
			}
		}
	}

	// Types.
	for _, typ := range f.pkg.Doc.Types {
		if formatter.IsExported(typ.Name) {
			printHdr("## Types")
			f.printf("### type %s\n\n", typ.Name)
			if err := f.typeDoc(typ); err != nil {
				return "", err
			}
		}
	}

	return f.buf.String(), nil
}

// valueDoc prints the docs for a constant or variable.
func (f *markdown) valueDoc(value *doc.Value, printed map[*ast.GenDecl]bool) error {
	if printed[value.Decl] {
		return nil
	}
	// Print each spec only if there is at least one exported symbol in it.
	// (See issue 11008.)
	// TODO: Should we elide unexported symbols from a single spec?
	// It's an unlikely scenario, probably not worth the trouble.
	// TODO: Would be nice if go/doc did this for us.
	specs := make([]ast.Spec, 0, len(value.Decl.Specs))
	var typ ast.Expr
	for _, spec := range value.Decl.Specs {
		vspec := spec.(*ast.ValueSpec)

		// The type name may carry over from a previous specification in the
		// case of constants and iota.
		if vspec.Type != nil {
			typ = vspec.Type
		}

		for _, ident := range vspec.Names {
			if formatter.IsExported(ident.Name) {
				if vspec.Type == nil && vspec.Values == nil && typ != nil {
					// This a standalone identifier, as in the case of iota usage.
					// Thus, assume the type comes from the previous type.
					vspec.Type = &ast.Ident{
						Name:    f.pkg.OneLineNode(typ),
						NamePos: vspec.End() - 1,
					}
				}

				specs = append(specs, vspec)
				typ = nil // Only inject type on first exported identifier
				break
			}
		}
	}
	if len(specs) == 0 {
		return nil
	}
	value.Decl.Specs = specs
	if err := f.emit(value.Doc, value.Decl); err != nil {
		return err
	}
	printed[value.Decl] = true
	return nil
}

// findTypeSpec returns the ast.TypeSpec within the declaration that defines the symbol.
// The name must match exactly.
func (f *markdown) findTypeSpec(decl *ast.GenDecl, symbol string) *ast.TypeSpec {
	for _, spec := range decl.Specs {
		typeSpec := spec.(*ast.TypeSpec) // Must succeed.
		if symbol == typeSpec.Name.Name {
			return typeSpec
		}
	}
	return nil
}

// typeDoc prints the docs for a type, including constructors and other items
// related to it.
func (f *markdown) typeDoc(typ *doc.Type) error {
	decl := typ.Decl
	spec := f.findTypeSpec(decl, typ.Name)
	trimUnexportedElems(spec)
	// If there are multiple types defined, reduce to just this one.
	if len(decl.Specs) > 1 {
		decl.Specs = []ast.Spec{spec}
	}
	// the comment of the Type
	doc.ToText(&f.buf, typ.Doc, "", indent, indentedWidth)
	f.newlines(2)
	// the declaration of the Type
	if err := f.emit("", decl); err != nil {
		return err
	}
	f.newlines(2)
	// Show associated methods, constants, etc.
	printed := make(map[*ast.GenDecl]bool)
	// We can use append here to print consts, then vars. Ditto for funcs and methods.
	values := typ.Consts
	values = append(values, typ.Vars...)
	for _, value := range values {
		for _, name := range value.Names {
			if formatter.IsExported(name) {
				if err := f.valueDoc(value, printed); err != nil {
					return err
				}
				break
			}
		}
	}
	funcs := typ.Funcs
	funcs = append(funcs, typ.Methods...)
	for _, fun := range funcs {
		if formatter.IsExported(fun.Name) {
			f.printf("#### func %s\n\n", fun.Name)
			if err := f.emit(fun.Doc, fun.Decl); err != nil {
				return err
			}
		}
	}
	return nil
}

// trimUnexportedElems modifies spec in place to elide unexported fields from
// structs and methods from interfaces (unless the unexported flag is set or we
// are asked to show the original source).
func trimUnexportedElems(spec *ast.TypeSpec) {
	switch typ := spec.Type.(type) {
	case *ast.StructType:
		typ.Fields = trimUnexportedFields(typ.Fields, false)
	case *ast.InterfaceType:
		typ.Methods = trimUnexportedFields(typ.Methods, true)
	}
}

// trimUnexportedFields returns the field list trimmed of unexported fields.
func trimUnexportedFields(fields *ast.FieldList, isInterface bool) *ast.FieldList {
	what := "methods"
	if !isInterface {
		what = "fields"
	}

	trimmed := false
	list := make([]*ast.Field, 0, len(fields.List))
	for _, field := range fields.List {
		names := field.Names
		if len(names) == 0 {
			// Embedded type. Use the name of the type. It must be of the form ident or
			// pkg.ident (for structs and interfaces), or *ident or *pkg.ident (structs only).
			// Nothing else is allowed.
			ty := field.Type
			if se, ok := field.Type.(*ast.StarExpr); !isInterface && ok {
				// The form *ident or *pkg.ident is only valid on
				// embedded types in structs.
				ty = se.X
			}
			switch ident := ty.(type) {
			case *ast.Ident:
				if isInterface && ident.Name == "error" && ident.Obj == nil {
					// For documentation purposes, we consider the builtin error
					// type special when embedded in an interface, such that it
					// always gets shown publicly.
					list = append(list, field)
					continue
				}
				names = []*ast.Ident{ident}
			case *ast.SelectorExpr:
				// An embedded type may refer to a type in another package.
				names = []*ast.Ident{ident.Sel}
			}
			if names == nil {
				// Can only happen if AST is incorrect. Safe to continue with a nil list.
				log.Print("invalid program: unexpected type for embedded field")
			}
		}
		// Trims if any is unexported. Good enough in practice.
		ok := true
		for _, name := range names {
			if !formatter.IsExported(name.Name) {
				trimmed = true
				ok = false
				break
			}
		}
		if ok {
			list = append(list, field)
		}
	}
	if !trimmed {
		return fields
	}
	unexportedField := &ast.Field{
		Type: &ast.Ident{
			// Hack: printer will treat this as a field with a named type.
			// Setting Name and NamePos to ("", fields.Closing-1) ensures that
			// when Pos and End are called on this field, they return the
			// position right before closing '}' character.
			Name:    "",
			NamePos: fields.Closing - 1,
		},
		Comment: &ast.CommentGroup{
			List: []*ast.Comment{{Text: fmt.Sprintf("// Has unexported %s.\n", what)}},
		},
	}
	return &ast.FieldList{
		Opening: fields.Opening,
		List:    append(list, unexportedField),
		Closing: fields.Closing,
	}
}
