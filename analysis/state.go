package analysis

import (
	"educationalsp/lsp"
	"strings"
)

type Symbol struct {
	Name     string
	Kind     string
	Location lsp.Location
	Line     int
	Column   int
}

type State struct {
	Documents map[string]string
	Symbols   map[string][]Symbol
}

func NewState() State {
	return State{
		Documents: map[string]string{},
		Symbols:   map[string][]Symbol{},
	}
}

func (s *State) parseDocument(uri, text string) ([]Statement, []Diagnostic) {
	scanner := NewScanner(text)
	tokens := scanner.ScanAll()
	parser := NewParser(tokens)
	return parser.Parse()
}

func (s *State) findSymbols(statements []Statement, uri string) []Symbol {
	symbols := []Symbol{}

	var collectSymbols func(stmts []Statement)
	collectSymbols = func(stmts []Statement) {
		for _, stmt := range stmts {
			switch v := stmt.(type) {
			case *VarStmt:
				symbols = append(symbols, Symbol{
					Name: v.Name,
					Kind: "variable",
					Location: lsp.Location{
						URI: uri,
						Range: lsp.Range{
							Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
							End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
						},
					},
					Line:   v.Line,
					Column: v.Column,
				})
			case *FnStmt:
				symbols = append(symbols, Symbol{
					Name: v.Name,
					Kind: "function",
					Location: lsp.Location{
						URI: uri,
						Range: lsp.Range{
							Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
							End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
						},
					},
					Line:   v.Line,
					Column: v.Column,
				})
			case *StructStmt:
				symbols = append(symbols, Symbol{
					Name: v.Name,
					Kind: "struct",
					Location: lsp.Location{
						URI: uri,
						Range: lsp.Range{
							Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
							End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
						},
					},
					Line:   v.Line,
					Column: v.Column,
				})
			case *BlockStmt:
				collectSymbols(v.Statements)
			case *IfStmt:
				if v.ThenBranch != nil {
					switch tb := v.ThenBranch.(type) {
					case *BlockStmt:
						collectSymbols(tb.Statements)
					}
				}
				if v.ElseBranch != nil {
					switch eb := v.ElseBranch.(type) {
					case *BlockStmt:
						collectSymbols(eb.Statements)
					case *IfStmt:
						collectSymbols([]Statement{eb})
					}
				}
			case *ForStmt:
				if fb, ok := v.Body.(*BlockStmt); ok {
					collectSymbols(fb.Statements)
				}
			case *WhileStmt:
				if wb, ok := v.Body.(*BlockStmt); ok {
					collectSymbols(wb.Statements)
				}
			}
		}
	}

	collectSymbols(statements)
	return symbols
}

func getDiagnosticsForFile(text string, uri string) ([]lsp.Diagnostic, []Symbol) {
	diagnostics := []lsp.Diagnostic{}
	symbols := []Symbol{}

	scanner := NewScanner(text)
	tokens := scanner.ScanAll()

	hasError := false
	for _, token := range tokens {
		if token.Type == TOKEN_ERROR {
			hasError = true
			diagnostics = append(diagnostics, lsp.Diagnostic{
				Range: lsp.Range{
					Start: lsp.Position{Line: token.Line - 1, Character: token.Column},
					End:   lsp.Position{Line: token.Line - 1, Character: token.Column + 1},
				},
				Severity: 1,
				Source:   "crux scanner",
				Message:  token.Literal,
			})
		}
	}

	if !hasError {
		parser := NewParser(tokens)
		statements, diags := parser.Parse()

		for _, diag := range diags {
			diagnostics = append(diagnostics, lsp.Diagnostic{
				Range:    diag.Range,
				Severity: diag.Severity,
				Source:   "crux parser",
				Message:  diag.Message,
			})
		}

		var collectSymbols func(stmts []Statement)
		collectSymbols = func(stmts []Statement) {
			for _, stmt := range stmts {
				switch v := stmt.(type) {
				case *VarStmt:
					symbols = append(symbols, Symbol{
						Name: v.Name,
						Kind: "variable",
						Location: lsp.Location{
							URI: uri,
							Range: lsp.Range{
								Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
								End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
							},
						},
						Line:   v.Line,
						Column: v.Column,
					})
				case *FnStmt:
					symbols = append(symbols, Symbol{
						Name: v.Name,
						Kind: "function",
						Location: lsp.Location{
							URI: uri,
							Range: lsp.Range{
								Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
								End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
							},
						},
						Line:   v.Line,
						Column: v.Column,
					})
				case *StructStmt:
					symbols = append(symbols, Symbol{
						Name: v.Name,
						Kind: "struct",
						Location: lsp.Location{
							URI: uri,
							Range: lsp.Range{
								Start: lsp.Position{Line: v.Line - 1, Character: v.Column},
								End:   lsp.Position{Line: v.Line - 1, Character: v.Column + len(v.Name)},
							},
						},
						Line:   v.Line,
						Column: v.Column,
					})
				case *BlockStmt:
					collectSymbols(v.Statements)
				case *IfStmt:
					if v.ThenBranch != nil {
						if tb, ok := v.ThenBranch.(*BlockStmt); ok {
							collectSymbols(tb.Statements)
						}
					}
					if v.ElseBranch != nil {
						if eb, ok := v.ElseBranch.(*BlockStmt); ok {
							collectSymbols(eb.Statements)
						} else if ei, ok := v.ElseBranch.(*IfStmt); ok {
							collectSymbols([]Statement{ei})
						}
					}
				case *ForStmt:
					if fb, ok := v.Body.(*BlockStmt); ok {
						collectSymbols(fb.Statements)
					}
				case *WhileStmt:
					if wb, ok := v.Body.(*BlockStmt); ok {
						collectSymbols(wb.Statements)
					}
				}
			}
		}

		collectSymbols(statements)
	}

	return diagnostics, symbols
}

func (s *State) OpenDocument(uri, text string) []lsp.Diagnostic {
	s.Documents[uri] = text

	diagnostics, symbols := getDiagnosticsForFile(text, uri)
	s.Symbols[uri] = symbols

	return diagnostics
}

func (s *State) UpdateDocument(uri, text string) []lsp.Diagnostic {
	s.Documents[uri] = text

	diagnostics, symbols := getDiagnosticsForFile(text, uri)
	s.Symbols[uri] = symbols

	return diagnostics
}

func (s *State) Hover(id int, uri string, position lsp.Position) lsp.HoverResponse {
	text := s.Documents[uri]
	symbols := s.Symbols[uri]

	scanner := NewScanner(text)
	tokens := scanner.ScanAll()

	adjustedLine := position.Line
	adjustedCol := position.Character

	for _, token := range tokens {
		if token.Type == TOKEN_IDENTIFIER && token.Line-1 == adjustedLine {
			tokenStartCol := token.Column
			tokenEndCol := token.Column + len(token.Literal)

			if adjustedCol >= tokenStartCol && adjustedCol <= tokenEndCol {
				for _, sym := range symbols {
					if sym.Name == token.Literal {
						return lsp.HoverResponse{
							Response: lsp.Response{
								RPC: "2.0",
								ID:  &id,
							},
							Result: lsp.HoverResult{
								Contents: sym.Kind + " " + sym.Name,
							},
						}
					}
				}

				for _, tok := range tokens {
					if tok.Type == TOKEN_FN && tok.Line == token.Line+1 {
						continue
					}
					if tok.Type == TOKEN_LET && tok.Line == token.Line+1 {
						continue
					}
				}

				return lsp.HoverResponse{
					Response: lsp.Response{
						RPC: "2.0",
						ID:  &id,
					},
					Result: lsp.HoverResult{
						Contents: "identifier " + token.Literal,
					},
				}
			}
		}
	}

	return lsp.HoverResponse{
		Response: lsp.Response{
			RPC: "2.0",
			ID:  &id,
		},
		Result: lsp.HoverResult{
			Contents: "No symbol found",
		},
	}
}

func (s *State) Definition(id int, uri string, position lsp.Position) lsp.DefinitionResponse {
	text := s.Documents[uri]
	symbols := s.Symbols[uri]

	scanner := NewScanner(text)
	tokens := scanner.ScanAll()

	adjustedLine := position.Line
	adjustedCol := position.Character

	for _, token := range tokens {
		if token.Type == TOKEN_IDENTIFIER && token.Line-1 == adjustedLine {
			tokenStartCol := token.Column
			tokenEndCol := token.Column + len(token.Literal)

			if adjustedCol >= tokenStartCol && adjustedCol <= tokenEndCol {
				for _, sym := range symbols {
					if sym.Name == token.Literal {
						loc := sym.Location
						return lsp.DefinitionResponse{
							Response: lsp.Response{
								RPC: "2.0",
								ID:  &id,
							},
							Result: &loc,
						}
					}
				}
			}
		}
	}

	return lsp.DefinitionResponse{
		Response: lsp.Response{
			RPC: "2.0",
			ID:  &id,
		},
		Result: &lsp.Location{},
	}
}

func (s *State) TextDocumentCodeAction(id int, uri string) lsp.TextDocumentCodeActionResponse {
	text := s.Documents[uri]

	actions := []lsp.CodeAction{}
	for row, line := range strings.Split(text, "\n") {
		idx := strings.Index(line, "VS Code")
		if idx >= 0 {
			replaceChange := map[string][]lsp.TextEdit{}
			replaceChange[uri] = []lsp.TextEdit{
				{
					Range:   LineRange(row, idx, idx+len("VS Code")),
					NewText: "Neovim",
				},
			}

			actions = append(actions, lsp.CodeAction{
				Title: "Replace VS C*de with a superior editor",
				Edit:  &lsp.WorkspaceEdit{Changes: replaceChange},
			})
		}
	}

	response := lsp.TextDocumentCodeActionResponse{
		Response: lsp.Response{
			RPC: "2.0",
			ID:  &id,
		},
		Result: actions,
	}

	return response
}

func (s *State) TextDocumentCompletion(id int, uri string) lsp.CompletionResponse {
	symbols := s.Symbols[uri]

	keywordsList := []string{
		"let", "fn", "if", "else", "while", "for", "return", "break", "continue",
		"struct", "match", "use", "from", "pub", "as", "and", "or", "not",
		"true", "false", "nil", "new", "give", "typeof", "panic",
	}

	items := []lsp.CompletionItem{}

	seen := make(map[string]bool)
	for _, sym := range symbols {
		if !seen[sym.Name] {
			seen[sym.Name] = true
			detail := "variable"
			if sym.Kind == "function" {
				detail = "function"
			} else if sym.Kind == "struct" {
				detail = "struct"
			}
			items = append(items, lsp.CompletionItem{
				Label:         sym.Name,
				Detail:        detail,
				Documentation: sym.Kind + " " + sym.Name,
			})
		}
	}

	for _, kw := range keywordsList {
		if !seen[kw] {
			items = append(items, lsp.CompletionItem{
				Label:         kw,
				Detail:        "keyword",
				Documentation: "Crux keyword: " + kw,
			})
		}
	}

	response := lsp.CompletionResponse{
		Response: lsp.Response{
			RPC: "2.0",
			ID:  &id,
		},
		Result: items,
	}

	return response
}

func LineRange(line, start, end int) lsp.Range {
	return lsp.Range{
		Start: lsp.Position{
			Line:      line,
			Character: start,
		},
		End: lsp.Position{
			Line:      line,
			Character: end,
		},
	}
}
