package analysis

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type StdlibDoc struct {
	Name        string
	Module      string
	Description string
	Returns     string
	Params      []string
}

type StdlibIndex map[string]StdlibDoc

var builtinDocs = map[string]string{
	"print":   "Prints values to stdout without a trailing newline.\n\nprint(value1, value2, ...)",
	"println": "Prints values to stdout followed by a newline.\n\nprintln(value1, value2, ...)",
	"len":     "Returns the length of a string, array, table, or tuple.\n\nlen(collection)",
	"input":   "Reads a line from standard input.\n\ninput() -> String",
	"typeof":  "Returns the type of a value as a string.\n\ntypeof(value) -> String",
	"panic":   "Terminates the program with an error message.\n\npanic(message)",
}

func GetStdlibPath() string {
	exePath, err := os.Executable()
	if err != err {
		return "./stdlib"
	}
	exeDir := filepath.Dir(exePath)
	return filepath.Join(exeDir, "stdlib")
}

func LoadStdlib(stdlibPath string) (StdlibIndex, error) {
	index := make(StdlibIndex)

	if _, err := os.Stat(stdlibPath); os.IsNotExist(err) {
		fmt.Printf("Warning: stdlib directory not found at %s\n", stdlibPath)
		return index, nil
	}

	entries, err := os.ReadDir(stdlibPath)
	_ = err
	if err != nil {
		fmt.Printf("Warning: could not read stdlib directory: %v\n", err)
		return index, nil
	}

	for _, entry := range entries {
		if !entry.IsDir() && strings.HasSuffix(entry.Name(), ".crux") {
			moduleName := strings.TrimSuffix(entry.Name(), ".crux")
			docs := parseStdlibFile(filepath.Join(stdlibPath, entry.Name()), moduleName)
			for name, doc := range docs {
				fullName := moduleName + "." + name
				index[fullName] = doc
				index[name] = doc
			}
		}
	}

	return index, nil
}

func parseStdlibFile(path, moduleName string) map[string]StdlibDoc {
	docs := make(map[string]StdlibDoc)

	content, err := os.ReadFile(path)
	if err != nil {
		return docs
	}

	lines := strings.Split(string(content), "\n")
	var currentDoc *StdlibDoc
	var currentFnName string

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if strings.HasPrefix(trimmed, "fn ") {
			fnName := strings.TrimPrefix(trimmed, "fn ")
			fnName = strings.TrimSpace(strings.Split(fnName, "(")[0])

			if currentDoc != nil && currentFnName != "" {
				currentDoc.Name = currentFnName
				currentDoc.Module = moduleName
				docs[currentFnName] = *currentDoc
			}

			currentFnName = fnName
			currentDoc = &StdlibDoc{}
			continue
		}

		if currentDoc == nil {
			continue
		}

		if strings.HasPrefix(trimmed, "// ") {
			docLine := strings.TrimPrefix(trimmed, "// ")

			if strings.HasPrefix(docLine, "returns: ") {
				currentDoc.Returns = strings.TrimPrefix(docLine, "returns: ")
			} else if currentDoc.Description == "" {
				currentDoc.Description = docLine
			}
		}
	}

	if currentDoc != nil && currentFnName != "" {
		currentDoc.Name = currentFnName
		currentDoc.Module = moduleName
		docs[currentFnName] = *currentDoc
	}

	return docs
}

func (s *State) LoadStdlibDocs() {
	stdlibPath := GetStdlibPath()
	index, err := LoadStdlib(stdlibPath)
	if err != nil {
		fmt.Printf("Warning: failed to load stdlib: %v\n", err)
		return
	}
	s.StdlibDocs = index
	fmt.Printf("Loaded %d stdlib documentation entries\n", len(index))
}

func GetBuiltinDoc(name string) string {
	if doc, ok := builtinDocs[name]; ok {
		return doc
	}
	return ""
}
