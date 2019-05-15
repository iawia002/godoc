package formatter

import (
	"unicode"
	"unicode/utf8"
)

// IsExported reports whether the name is an exported identifier.
func IsExported(name string) bool {
	ch, _ := utf8.DecodeRuneInString(name)
	return unicode.IsUpper(ch)
}
