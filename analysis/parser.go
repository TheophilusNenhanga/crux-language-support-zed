package analysis

import (
	"educationalsp/lsp"
)

type Diagnostic = lsp.Diagnostic
type Range = lsp.Range
type Position = lsp.Position

type Node interface {
	GetLine() int
	GetColumn() int
}

type Expr interface {
	GetLine() int
	GetColumn() int
	exprNode()
}

type IdentifierExpr struct {
	Name   string
	Line   int
	Column int
}

func (e *IdentifierExpr) GetLine() int   { return e.Line }
func (e *IdentifierExpr) GetColumn() int { return e.Column }
func (e *IdentifierExpr) exprNode()      {}

type NumberExpr struct {
	Value  string
	IsInt  bool
	Line   int
	Column int
}

func (e *NumberExpr) GetLine() int   { return e.Line }
func (e *NumberExpr) GetColumn() int { return e.Column }
func (e *NumberExpr) exprNode()      {}

type StringExpr struct {
	Value  string
	Line   int
	Column int
}

func (e *StringExpr) GetLine() int   { return e.Line }
func (e *StringExpr) GetColumn() int { return e.Column }
func (e *StringExpr) exprNode()      {}

type BooleanExpr struct {
	Value  bool
	Line   int
	Column int
}

func (e *BooleanExpr) GetLine() int   { return e.Line }
func (e *BooleanExpr) GetColumn() int { return e.Column }
func (e *BooleanExpr) exprNode()      {}

type NilExpr struct {
	Line   int
	Column int
}

func (e *NilExpr) GetLine() int   { return e.Line }
func (e *NilExpr) GetColumn() int { return e.Column }
func (e *NilExpr) exprNode()      {}

type BinaryExpr struct {
	Left   Expr
	Op     TokenType
	Right  Expr
	Line   int
	Column int
}

func (e *BinaryExpr) GetLine() int   { return e.Line }
func (e *BinaryExpr) GetColumn() int { return e.Column }
func (e *BinaryExpr) exprNode()      {}

type UnaryExpr struct {
	Op     TokenType
	Right  Expr
	Line   int
	Column int
}

func (e *UnaryExpr) GetLine() int   { return e.Line }
func (e *UnaryExpr) GetColumn() int { return e.Column }
func (e *UnaryExpr) exprNode()      {}

type CallExpr struct {
	Callee Expr
	Args   []Expr
	Line   int
	Column int
}

func (e *CallExpr) GetLine() int   { return e.Line }
func (e *CallExpr) GetColumn() int { return e.Column }
func (e *CallExpr) exprNode()      {}

type GroupingExpr struct {
	Inner  Expr
	Line   int
	Column int
}

func (e *GroupingExpr) GetLine() int   { return e.Line }
func (e *GroupingExpr) GetColumn() int { return e.Column }
func (e *GroupingExpr) exprNode()      {}

type ArrayExpr struct {
	Elements []Expr
	Line     int
	Column   int
}

func (e *ArrayExpr) GetLine() int   { return e.Line }
func (e *ArrayExpr) GetColumn() int { return e.Column }
func (e *ArrayExpr) exprNode()      {}

type IndexExpr struct {
	Object Expr
	Index  Expr
	Line   int
	Column int
}

func (e *IndexExpr) GetLine() int   { return e.Line }
func (e *IndexExpr) GetColumn() int { return e.Column }
func (e *IndexExpr) exprNode()      {}

type TableExpr struct {
	Pairs  []TablePair
	Line   int
	Column int
}

type TablePair struct {
	Key   Expr
	Value Expr
}

func (e *TableExpr) GetLine() int   { return e.Line }
func (e *TableExpr) GetColumn() int { return e.Column }
func (e *TableExpr) exprNode()      {}

type StructInstantiation struct {
	Name   string
	Fields []FieldPair
	Line   int
	Column int
}

type FieldPair struct {
	Name  string
	Value Expr
}

func (e *StructInstantiation) GetLine() int   { return e.Line }
func (e *StructInstantiation) GetColumn() int { return e.Column }
func (e *StructInstantiation) exprNode()      {}

type TypeofExpr struct {
	Expr   Expr
	Line   int
	Column int
}

func (e *TypeofExpr) GetLine() int   { return e.Line }
func (e *TypeofExpr) GetColumn() int { return e.Column }
func (e *TypeofExpr) exprNode()      {}

type FnExpr struct {
	Params []string
	Body   *BlockStmt
	Line   int
	Column int
}

func (e *FnExpr) GetLine() int   { return e.Line }
func (e *FnExpr) GetColumn() int { return e.Column }
func (e *FnExpr) exprNode()      {}

type Statement interface {
	Node
	stmtNode()
}

type VarStmt struct {
	Name        string
	Initializer Expr
	Line        int
	Column      int
	IsPublic    bool
}

func (s *VarStmt) GetLine() int   { return s.Line }
func (s *VarStmt) GetColumn() int { return s.Column }
func (s *VarStmt) stmtNode()      {}

type BlockStmt struct {
	Statements []Statement
	Line       int
	Column     int
}

func (s *BlockStmt) GetLine() int   { return s.Line }
func (s *BlockStmt) GetColumn() int { return s.Column }
func (s *BlockStmt) stmtNode()      {}

type IfStmt struct {
	Condition  Expr
	ThenBranch Statement
	ElseBranch Statement
	Line       int
	Column     int
}

func (s *IfStmt) GetLine() int   { return s.Line }
func (s *IfStmt) GetColumn() int { return s.Column }
func (s *IfStmt) stmtNode()      {}

type ForStmt struct {
	Initializer Expr
	Condition   Expr
	Increment   Expr
	Body        Statement
	Line        int
	Column      int
}

func (s *ForStmt) GetLine() int   { return s.Line }
func (s *ForStmt) GetColumn() int { return s.Column }
func (s *ForStmt) stmtNode()      {}

type WhileStmt struct {
	Condition Expr
	Body      Statement
	Line      int
	Column    int
}

func (s *WhileStmt) GetLine() int   { return s.Line }
func (s *WhileStmt) GetColumn() int { return s.Column }
func (s *WhileStmt) stmtNode()      {}

type ReturnStmt struct {
	Value  Expr
	Line   int
	Column int
}

func (s *ReturnStmt) GetLine() int   { return s.Line }
func (s *ReturnStmt) GetColumn() int { return s.Column }
func (s *ReturnStmt) stmtNode()      {}

type FnStmt struct {
	Name     string
	Params   []string
	Body     *BlockStmt
	Line     int
	Column   int
	IsPublic bool
}

func (s *FnStmt) GetLine() int   { return s.Line }
func (s *FnStmt) GetColumn() int { return s.Column }
func (s *FnStmt) stmtNode()      {}

type BreakStmt struct {
	Line   int
	Column int
}

func (s *BreakStmt) GetLine() int   { return s.Line }
func (s *BreakStmt) GetColumn() int { return s.Column }
func (s *BreakStmt) stmtNode()      {}

type ContinueStmt struct {
	Line   int
	Column int
}

func (s *ContinueStmt) GetLine() int   { return s.Line }
func (s *ContinueStmt) GetColumn() int { return s.Column }
func (s *ContinueStmt) stmtNode()      {}

type StructStmt struct {
	Name   string
	Fields []string
	Line   int
	Column int
}

func (s *StructStmt) GetLine() int   { return s.Line }
func (s *StructStmt) GetColumn() int { return s.Column }
func (s *StructStmt) stmtNode()      {}

type MatchStmt struct {
	Expr    Expr
	Cases   []MatchCase
	Default Statement
	Line    int
	Column  int
}

type MatchCase struct {
	Values []Expr
	Result Statement
}

func (s *MatchStmt) GetLine() int   { return s.Line }
func (s *MatchStmt) GetColumn() int { return s.Column }
func (s *MatchStmt) stmtNode()      {}

type ExprStmt struct {
	Expression Expr
	Line       int
	Column     int
}

func (s *ExprStmt) GetLine() int   { return s.Line }
func (s *ExprStmt) GetColumn() int { return s.Column }
func (s *ExprStmt) stmtNode()      {}

type Parser struct {
	tokens      []Token
	current     int
	diagnostics []Diagnostic
}

func NewParser(tokens []Token) *Parser {
	return &Parser{
		tokens:      tokens,
		current:     0,
		diagnostics: []Diagnostic{},
	}
}

func (p *Parser) currentToken() Token {
	if p.current >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.current]
}

func (p *Parser) previousToken() Token {
	if p.current == 0 {
		return p.tokens[0]
	}
	return p.tokens[p.current-1]
}

func (p *Parser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previousToken()
}

func (p *Parser) isAtEnd() bool {
	return p.currentToken().Type == TOKEN_EOF || p.currentToken().Type == TOKEN_ERROR
}

func (p *Parser) check(tokenType TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.currentToken().Type == tokenType
}

func (p *Parser) match(tokenTypes ...TokenType) bool {
	for _, tt := range tokenTypes {
		if p.check(tt) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) consume(tokenType TokenType, message string) Token {
	if p.check(tokenType) {
		return p.advance()
	}
	p.addError(message, p.currentToken().Line, p.currentToken().Column)
	return p.previousToken()
}

func (p *Parser) addError(message string, line, column int) {
	p.diagnostics = append(p.diagnostics, Diagnostic{
		Range: Range{
			Start: Position{Line: line - 1, Character: column},
			End:   Position{Line: line - 1, Character: column + 1},
		},
		Severity: 1,
		Message:  message,
	})
}

func (p *Parser) Parse() ([]Statement, []Diagnostic) {
	statements := []Statement{}
	for !p.isAtEnd() {
		stmt := p.declaration()
		if stmt != nil {
			statements = append(statements, stmt)
		}
	}
	return statements, p.diagnostics
}

func (p *Parser) declaration() Statement {
	if p.match(TOKEN_LET) {
		return p.varDeclaration()
	}
	if p.match(TOKEN_FN) {
		return p.fnDeclaration()
	}
	if p.match(TOKEN_STRUCT) {
		return p.structDeclaration()
	}
	if p.match(TOKEN_PUB) {
		if p.check(TOKEN_FN) {
			p.advance()
			return p.fnDeclarationPublic()
		}
		if p.check(TOKEN_LET) {
			p.advance()
			return p.varDeclarationPublic()
		}
	}
	return p.statement()
}

func (p *Parser) varDeclaration() Statement {
	name := p.consume(TOKEN_IDENTIFIER, "Expect variable name").Literal
	var initializer Expr
	if p.match(TOKEN_EQUAL) {
		initializer = p.expression()
	}
	p.consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration")

	prev := p.previousToken()
	return &VarStmt{
		Name:        name,
		Initializer: initializer,
		Line:        prev.Line,
		Column:      prev.Column,
		IsPublic:    false,
	}
}

func (p *Parser) varDeclarationPublic() Statement {
	name := p.consume(TOKEN_IDENTIFIER, "Expect variable name").Literal
	var initializer Expr
	if p.match(TOKEN_EQUAL) {
		initializer = p.expression()
	}
	p.consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration")

	prev := p.previousToken()
	return &VarStmt{
		Name:        name,
		Initializer: initializer,
		Line:        prev.Line,
		Column:      prev.Column,
		IsPublic:    true,
	}
}

func (p *Parser) fnDeclaration() Statement {
	name := p.consume(TOKEN_IDENTIFIER, "Expect function name").Literal
	p.consume(TOKEN_LEFT_PAREN, "Expect '(' after function name")

	params := []string{}
	if !p.check(TOKEN_RIGHT_PAREN) {
		for {
			params = append(params, p.consume(TOKEN_IDENTIFIER, "Expect parameter name").Literal)
			if !p.match(TOKEN_COMMA) {
				break
			}
		}
	}
	p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters")
	p.consume(TOKEN_LEFT_BRACE, "Expect '{' before function body")

	body := p.block()

	prev := p.previousToken()
	return &FnStmt{
		Name:     name,
		Params:   params,
		Body:     body,
		Line:     prev.Line,
		Column:   prev.Column,
		IsPublic: false,
	}
}

func (p *Parser) fnDeclarationPublic() Statement {
	name := p.consume(TOKEN_IDENTIFIER, "Expect function name").Literal
	p.consume(TOKEN_LEFT_PAREN, "Expect '(' after function name")

	params := []string{}
	if !p.check(TOKEN_RIGHT_PAREN) {
		for {
			params = append(params, p.consume(TOKEN_IDENTIFIER, "Expect parameter name").Literal)
			if !p.match(TOKEN_COMMA) {
				break
			}
		}
	}
	p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters")
	p.consume(TOKEN_LEFT_BRACE, "Expect '{' before function body")

	body := p.block()

	prev := p.previousToken()
	return &FnStmt{
		Name:     name,
		Params:   params,
		Body:     body,
		Line:     prev.Line,
		Column:   prev.Column,
		IsPublic: true,
	}
}

func (p *Parser) structDeclaration() Statement {
	name := p.consume(TOKEN_IDENTIFIER, "Expect struct name").Literal
	p.consume(TOKEN_LEFT_BRACE, "Expect '{' before struct fields")

	fields := []string{}
	for !p.check(TOKEN_RIGHT_BRACE) && !p.isAtEnd() {
		fields = append(fields, p.consume(TOKEN_IDENTIFIER, "Expect field name").Literal)
		p.match(TOKEN_SEMICOLON)
	}
	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after struct fields")
	p.consume(TOKEN_SEMICOLON, "Expect ';' after struct declaration")

	prev := p.previousToken()
	return &StructStmt{
		Name:   name,
		Fields: fields,
		Line:   prev.Line,
		Column: prev.Column,
	}
}

func (p *Parser) statement() Statement {
	if p.match(TOKEN_LEFT_BRACE) {
		p.current--
		return p.blockStatement()
	}
	if p.match(TOKEN_IF) {
		return p.ifStatement()
	}
	if p.match(TOKEN_FOR) {
		return p.forStatement()
	}
	if p.match(TOKEN_WHILE) {
		return p.whileStatement()
	}
	if p.match(TOKEN_RETURN) {
		return p.returnStatement()
	}
	if p.match(TOKEN_BREAK) {
		p.consume(TOKEN_SEMICOLON, "Expect ';' after break")
		prev := p.previousToken()
		return &BreakStmt{Line: prev.Line, Column: prev.Column}
	}
	if p.match(TOKEN_CONTINUE) {
		p.consume(TOKEN_SEMICOLON, "Expect ';' after continue")
		prev := p.previousToken()
		return &ContinueStmt{Line: prev.Line, Column: prev.Column}
	}
	if p.match(TOKEN_MATCH) {
		return p.matchStatement()
	}

	return p.expressionStatement()
}

func (p *Parser) block() *BlockStmt {
	statements := []Statement{}
	for !p.check(TOKEN_RIGHT_BRACE) && !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after block")
	prev := p.previousToken()
	return &BlockStmt{
		Statements: statements,
		Line:       prev.Line,
		Column:     prev.Column,
	}
}

func (p *Parser) blockStatement() Statement {
	p.advance() // consume '{'
	statements := []Statement{}
	for !p.check(TOKEN_RIGHT_BRACE) && !p.isAtEnd() {
		statements = append(statements, p.declaration())
	}
	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after block")
	prev := p.previousToken()
	return &BlockStmt{
		Statements: statements,
		Line:       prev.Line,
		Column:     prev.Column,
	}
}

func (p *Parser) ifStatement() Statement {
	condition := p.expression()
	thenBranch := p.statement()
	var elseBranch Statement
	if p.match(TOKEN_ELSE) {
		elseBranch = p.statement()
	}
	prev := p.previousToken()
	return &IfStmt{
		Condition:  condition,
		ThenBranch: thenBranch,
		ElseBranch: elseBranch,
		Line:       prev.Line,
		Column:     prev.Column,
	}
}

func (p *Parser) forStatement() Statement {
	var initializer Expr
	var condition Expr
	var increment Expr

	if p.match(TOKEN_SEMICOLON) {
		// No initializer
	} else if p.match(TOKEN_LET) {
		initializer = p.expression()
	} else {
		initializer = p.expression()
	}

	if !p.check(TOKEN_SEMICOLON) {
		condition = p.expression()
	}
	p.consume(TOKEN_SEMICOLON, "Expect ';' after loop condition")

	if !p.check(TOKEN_LEFT_BRACE) {
		increment = p.expression()
	}

	body := p.statement()
	prev := p.previousToken()
	return &ForStmt{
		Initializer: initializer,
		Condition:   condition,
		Increment:   increment,
		Body:        body,
		Line:        prev.Line,
		Column:      prev.Column,
	}
}

func (p *Parser) whileStatement() Statement {
	condition := p.expression()
	body := p.statement()
	prev := p.previousToken()
	return &WhileStmt{
		Condition: condition,
		Body:      body,
		Line:      prev.Line,
		Column:    prev.Column,
	}
}

func (p *Parser) returnStatement() Statement {
	var value Expr
	if !p.check(TOKEN_SEMICOLON) {
		value = p.expression()
	}
	p.consume(TOKEN_SEMICOLON, "Expect ';' after return value")
	prev := p.previousToken()
	return &ReturnStmt{
		Value:  value,
		Line:   prev.Line,
		Column: prev.Column,
	}
}

func (p *Parser) matchStatement() Statement {
	expr := p.expression()
	p.consume(TOKEN_LEFT_BRACE, "Expect '{' after match expression")

	cases := []MatchCase{}
	var defaultCase Statement

	for !p.check(TOKEN_RIGHT_BRACE) && !p.isAtEnd() {
		if p.match(TOKEN_DEFAULT) {
			p.consume(TOKEN_COLON, "Expect ':' after default")
			defaultCase = p.statement()
			continue
		}

		values := []Expr{}
		values = append(values, p.expression())
		for p.match(TOKEN_COMMA) {
			values = append(values, p.expression())
		}
		p.consume(TOKEN_EQUAL_ARROW, "Expect '=>' after match case")
		result := p.statement()

		cases = append(cases, MatchCase{
			Values: values,
			Result: result,
		})
	}

	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after match cases")
	prev := p.previousToken()
	return &MatchStmt{
		Expr:    expr,
		Cases:   cases,
		Default: defaultCase,
		Line:    prev.Line,
		Column:  prev.Column,
	}
}

func (p *Parser) expressionStatement() Statement {
	expr := p.expression()
	p.consume(TOKEN_SEMICOLON, "Expect ';' after expression")
	prev := p.previousToken()
	return &ExprStmt{
		Expression: expr,
		Line:       prev.Line,
		Column:     prev.Column,
	}
}

func (p *Parser) expression() Expr {
	return p.or()
}

func (p *Parser) or() Expr {
	expr := p.and()

	for p.match(TOKEN_OR) {
		right := p.and()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     TOKEN_OR,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) and() Expr {
	expr := p.equality()

	for p.match(TOKEN_AND) {
		right := p.equality()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     TOKEN_AND,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) equality() Expr {
	expr := p.comparison()

	for p.match(TOKEN_BANG_EQUAL, TOKEN_EQUAL_EQUAL) {
		op := p.previousToken().Type
		right := p.comparison()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) comparison() Expr {
	expr := p.shift()

	for p.match(TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL) {
		op := p.previousToken().Type
		right := p.shift()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) shift() Expr {
	expr := p.term()

	for p.match(TOKEN_LEFT_SHIFT, TOKEN_RIGHT_SHIFT) {
		op := p.previousToken().Type
		right := p.term()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) term() Expr {
	expr := p.factor()

	for p.match(TOKEN_PLUS, TOKEN_MINUS) {
		op := p.previousToken().Type
		right := p.factor()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) factor() Expr {
	expr := p.unary()

	for p.match(TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT) {
		op := p.previousToken().Type
		right := p.unary()
		prev := p.previousToken()
		expr = &BinaryExpr{
			Left:   expr,
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return expr
}

func (p *Parser) unary() Expr {
	if p.match(TOKEN_NOT, TOKEN_MINUS) {
		op := p.previousToken().Type
		right := p.unary()
		prev := p.previousToken()
		return &UnaryExpr{
			Op:     op,
			Right:  right,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}

	return p.call()
}

func (p *Parser) call() Expr {
	expr := p.primary()

	for {
		if p.match(TOKEN_LEFT_PAREN) {
			args := []Expr{}
			if !p.check(TOKEN_RIGHT_PAREN) {
				for {
					args = append(args, p.expression())
					if !p.match(TOKEN_COMMA) {
						break
					}
				}
			}
			p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments")
			prev := p.previousToken()
			expr = &CallExpr{
				Callee: expr,
				Args:   args,
				Line:   prev.Line,
				Column: prev.Column,
			}
		} else if p.match(TOKEN_LEFT_SQUARE) {
			index := p.expression()
			p.consume(TOKEN_RIGHT_SQUARE, "Expect ']' after index")
			prev := p.previousToken()
			expr = &IndexExpr{
				Object: expr,
				Index:  index,
				Line:   prev.Line,
				Column: prev.Column,
			}
		} else if p.match(TOKEN_DOT) {
			name := p.consume(TOKEN_IDENTIFIER, "Expect property name after '.'")
			prev := p.previousToken()
			expr = &IndexExpr{
				Object: expr,
				Index:  &StringExpr{Value: name.Literal, Line: prev.Line, Column: prev.Column},
				Line:   prev.Line,
				Column: prev.Column,
			}
		} else if p.match(TOKEN_LEFT_BRACE) {
			if ident, ok := expr.(*IdentifierExpr); ok {
				fields := []FieldPair{}
				if !p.check(TOKEN_RIGHT_BRACE) {
					for {
						fieldName := p.consume(TOKEN_IDENTIFIER, "Expect field name").Literal
						p.consume(TOKEN_COLON, "Expect ':' after field name")
						value := p.expression()
						fields = append(fields, FieldPair{Name: fieldName, Value: value})
						if !p.match(TOKEN_COMMA) {
							break
						}
					}
				}
				p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after struct fields")
				prev := p.previousToken()
				expr = &StructInstantiation{
					Name:   ident.Name,
					Fields: fields,
					Line:   prev.Line,
					Column: prev.Column,
				}
			} else {
				break
			}
		} else {
			break
		}
	}

	return expr
}

func (p *Parser) primary() Expr {
	if p.match(TOKEN_FALSE) {
		prev := p.previousToken()
		return &BooleanExpr{
			Value:  false,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_TRUE) {
		prev := p.previousToken()
		return &BooleanExpr{
			Value:  true,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_NIL) {
		prev := p.previousToken()
		return &NilExpr{
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_INT, TOKEN_FLOAT) {
		prev := p.previousToken()
		return &NumberExpr{
			Value:  prev.Literal,
			IsInt:  prev.Type == TOKEN_INT,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_STRING) {
		prev := p.previousToken()
		return &StringExpr{
			Value:  prev.Literal,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_IDENTIFIER) {
		prev := p.previousToken()
		return &IdentifierExpr{
			Name:   prev.Literal,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_LEFT_PAREN) {
		inner := p.expression()
		p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression")
		prev := p.previousToken()
		return &GroupingExpr{
			Inner:  inner,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_LEFT_SQUARE) {
		elements := []Expr{}
		if !p.check(TOKEN_RIGHT_SQUARE) {
			for {
				elements = append(elements, p.expression())
				if !p.match(TOKEN_COMMA) {
					break
				}
			}
		}
		p.consume(TOKEN_RIGHT_SQUARE, "Expect ']' after array elements")
		prev := p.previousToken()
		return &ArrayExpr{
			Elements: elements,
			Line:     prev.Line,
			Column:   prev.Column,
		}
	}
	if p.match(TOKEN_LEFT_BRACE) {
		pairs := []TablePair{}
		if !p.check(TOKEN_RIGHT_BRACE) {
			for {
				key := p.expression()
				p.consume(TOKEN_COLON, "Expect ':' after table key")
				value := p.expression()
				pairs = append(pairs, TablePair{Key: key, Value: value})
				if !p.match(TOKEN_COMMA) {
					break
				}
			}
		}
		p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after table pairs")
		prev := p.previousToken()
		return &TableExpr{
			Pairs:  pairs,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_TYPEOF) {
		expr := p.unary()
		prev := p.previousToken()
		return &TypeofExpr{
			Expr:   expr,
			Line:   prev.Line,
			Column: prev.Column,
		}
	}
	if p.match(TOKEN_FN) {
		return p.parseFunctionExpression()
	}

	prev := p.previousToken()
	p.addError("Expect expression", prev.Line, prev.Column)
	return &NilExpr{
		Line:   prev.Line,
		Column: prev.Column,
	}
}

func (p *Parser) parseFunctionExpression() Expr {
	params := []string{}
	if !p.check(TOKEN_LEFT_PAREN) {
		p.consume(TOKEN_LEFT_PAREN, "Expect '(' after fn")
	}
	if !p.check(TOKEN_RIGHT_PAREN) {
		for {
			params = append(params, p.consume(TOKEN_IDENTIFIER, "Expect parameter name").Literal)
			if !p.match(TOKEN_COMMA) {
				break
			}
		}
	}
	p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters")
	p.consume(TOKEN_LEFT_BRACE, "Expect '{' before function body")
	body := p.block()
	prev := p.previousToken()
	return &FnExpr{
		Params: params,
		Body:   body,
		Line:   prev.Line,
		Column: prev.Column,
	}
}

func (p *Parser) parseStructInstantiation(name string, line, column int) Expr {
	fields := []FieldPair{}
	if !p.check(TOKEN_RIGHT_BRACE) {
		for {
			fieldName := p.consume(TOKEN_IDENTIFIER, "Expect field name").Literal
			p.consume(TOKEN_COLON, "Expect ':' after field name")
			value := p.expression()
			fields = append(fields, FieldPair{Name: fieldName, Value: value})
			if !p.match(TOKEN_COMMA) {
				break
			}
		}
	}
	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after struct fields")
	prev := p.previousToken()
	return &StructInstantiation{
		Name:   name,
		Fields: fields,
		Line:   prev.Line,
		Column: prev.Column,
	}
}
