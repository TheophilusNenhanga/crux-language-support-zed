package analysis

type TokenType int

const (
	TOKEN_LEFT_PAREN TokenType = iota
	TOKEN_RIGHT_PAREN
	TOKEN_LEFT_BRACE
	TOKEN_RIGHT_BRACE
	TOKEN_LEFT_SQUARE
	TOKEN_RIGHT_SQUARE
	TOKEN_COMMA
	TOKEN_DOT
	TOKEN_MINUS
	TOKEN_PLUS
	TOKEN_SEMICOLON
	TOKEN_SLASH
	TOKEN_BACKSLASH
	TOKEN_STAR
	TOKEN_STAR_STAR
	TOKEN_PERCENT
	TOKEN_COLON
	TOKEN_SINGLE_QUOTE
	TOKEN_DOUBLE_QUOTE
	TOKEN_BANG_EQUAL
	TOKEN_EQUAL
	TOKEN_EQUAL_EQUAL
	TOKEN_GREATER
	TOKEN_GREATER_EQUAL
	TOKEN_LESS
	TOKEN_LESS_EQUAL
	TOKEN_LEFT_SHIFT
	TOKEN_RIGHT_SHIFT
	TOKEN_PLUS_EQUAL
	TOKEN_MINUS_EQUAL
	TOKEN_STAR_EQUAL
	TOKEN_SLASH_EQUAL
	TOKEN_BACK_SLASH_EQUAL
	TOKEN_PERCENT_EQUAL
	TOKEN_IDENTIFIER
	TOKEN_STRING
	TOKEN_INT
	TOKEN_FLOAT
	TOKEN_AND
	TOKEN_NOT
	TOKEN_ELSE
	TOKEN_FALSE
	TOKEN_FOR
	TOKEN_FN
	TOKEN_IF
	TOKEN_NIL
	TOKEN_OR
	TOKEN_RETURN
	TOKEN_TRUE
	TOKEN_LET
	TOKEN_WHILE
	TOKEN_ERROR
	TOKEN_BREAK
	TOKEN_CONTINUE
	TOKEN_USE
	TOKEN_FROM
	TOKEN_PUB
	TOKEN_AS
	TOKEN_EOF
	TOKEN_MATCH
	TOKEN_EQUAL_ARROW
	TOKEN_OK
	TOKEN_ERR
	TOKEN_DEFAULT
	TOKEN_GIVE
	TOKEN_TYPEOF
	TOKEN_NEW
	TOKEN_PANIC
	TOKEN_DOLLAR_LEFT_CURLY
	TOKEN_DOLLAR_LEFT_SQUARE
	TOKEN_STRUCT
	TOKEN_QUESTION_MARK
)

var keywords = map[string]TokenType{
	"and":      TOKEN_AND,
	"not":      TOKEN_NOT,
	"else":     TOKEN_ELSE,
	"false":    TOKEN_FALSE,
	"for":      TOKEN_FOR,
	"fn":       TOKEN_FN,
	"if":       TOKEN_IF,
	"nil":      TOKEN_NIL,
	"or":       TOKEN_OR,
	"return":   TOKEN_RETURN,
	"true":     TOKEN_TRUE,
	"let":      TOKEN_LET,
	"while":    TOKEN_WHILE,
	"break":    TOKEN_BREAK,
	"continue": TOKEN_CONTINUE,
	"use":      TOKEN_USE,
	"from":     TOKEN_FROM,
	"pub":      TOKEN_PUB,
	"as":       TOKEN_AS,
	"match":    TOKEN_MATCH,
	"Ok":       TOKEN_OK,
	"Err":      TOKEN_ERR,
	"default":  TOKEN_DEFAULT,
	"give":     TOKEN_GIVE,
	"typeof":   TOKEN_TYPEOF,
	"new":      TOKEN_NEW,
	"panic":    TOKEN_PANIC,
	"struct":   TOKEN_STRUCT,
}

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

type Scanner struct {
	source  string
	start   int
	current int
	line    int
	tokens  []Token
}

func NewScanner(source string) *Scanner {
	return &Scanner{
		source:  source,
		start:   0,
		current: 0,
		line:    1,
		tokens:  []Token{},
	}
}

func (s *Scanner) isAtEnd() bool {
	return s.current >= len(s.source)
}

func (s *Scanner) advance() byte {
	c := s.source[s.current]
	s.current++
	return c
}

func (s *Scanner) peek() byte {
	if s.isAtEnd() {
		return 0
	}
	return s.source[s.current]
}

func (s *Scanner) peekNext() byte {
	if s.current+1 >= len(s.source) {
		return 0
	}
	return s.source[s.current+1]
}

func (s *Scanner) match(expected byte) bool {
	if s.isAtEnd() {
		return false
	}
	if s.source[s.current] != expected {
		return false
	}
	s.current++
	return true
}

func (s *Scanner) isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func (s *Scanner) isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func (s *Scanner) isIdentifierStart(c byte) bool {
	return s.isAlpha(c) || c == '_'
}

func (s *Scanner) isIdentifier(c byte) bool {
	return s.isIdentifierStart(c) || s.isDigit(c)
}

func (s *Scanner) skipWhitespace() {
	for !s.isAtEnd() {
		c := s.peek()
		switch c {
		case ' ', '\r', '\t':
			s.advance()
		case '\n':
			s.line++
			s.advance()
		case '/':
			if s.peekNext() == '/' {
				for s.peek() != '\n' && !s.isAtEnd() {
					s.advance()
				}
			} else {
				return
			}
		default:
			return
		}
	}
}

func (s *Scanner) scanString(quote byte) Token {
	start := s.line
	for s.peek() != quote && !s.isAtEnd() {
		if s.peek() == '\n' {
			s.line++
		}
		if s.peek() == '\\' && !s.isAtEnd() {
			s.advance()
			if !s.isAtEnd() {
				s.advance()
			}
			continue
		}
		s.advance()
	}

	if s.isAtEnd() {
		return Token{Type: TOKEN_ERROR, Literal: "Unterminated String", Line: start}
	}

	s.advance()
	return Token{Type: TOKEN_STRING, Literal: s.source[s.start+1 : s.current-1], Line: start}
}

func (s *Scanner) scanNumber() Token {
	start := s.line
	hasDecimal := false

	for s.isDigit(s.peek()) {
		s.advance()
	}

	if s.peek() == '.' && s.isDigit(s.peekNext()) {
		hasDecimal = true
		s.advance()
		for s.isDigit(s.peek()) {
			s.advance()
		}
	}

	tokenType := TOKEN_INT
	if hasDecimal {
		tokenType = TOKEN_FLOAT
	}

	return Token{Type: tokenType, Literal: s.source[s.start:s.current], Line: start}
}

func (s *Scanner) scanIdentifier() Token {
	for s.isIdentifier(s.peek()) {
		s.advance()
	}

	text := s.source[s.start:s.current]
	tokenType, ok := keywords[text]
	if !ok {
		tokenType = TOKEN_IDENTIFIER
	}

	return Token{Type: tokenType, Literal: text, Line: s.line}
}

func (s *Scanner) makeToken(tokenType TokenType) Token {
	return Token{
		Type:    tokenType,
		Literal: s.source[s.start:s.current],
		Line:    s.line,
		Column:  s.start,
	}
}

func (s *Scanner) errorToken(message string) Token {
	return Token{
		Type:    TOKEN_ERROR,
		Literal: message,
		Line:    s.line,
		Column:  s.start,
	}
}

func (s *Scanner) ScanToken() Token {
	s.skipWhitespace()
	s.start = s.current

	if s.isAtEnd() {
		return s.makeToken(TOKEN_EOF)
	}

	c := s.advance()

	if s.isDigit(c) {
		return s.scanNumber()
	}

	if s.isIdentifierStart(c) {
		return s.scanIdentifier()
	}

	switch c {
	case '(':
		return s.makeToken(TOKEN_LEFT_PAREN)
	case ')':
		return s.makeToken(TOKEN_RIGHT_PAREN)
	case '{':
		return s.makeToken(TOKEN_LEFT_BRACE)
	case '}':
		return s.makeToken(TOKEN_RIGHT_BRACE)
	case '[':
		return s.makeToken(TOKEN_LEFT_SQUARE)
	case ']':
		return s.makeToken(TOKEN_RIGHT_SQUARE)
	case ';':
		return s.makeToken(TOKEN_SEMICOLON)
	case ',':
		return s.makeToken(TOKEN_COMMA)
	case '.':
		return s.makeToken(TOKEN_DOT)
	case '-':
		if s.match('=') {
			return s.makeToken(TOKEN_MINUS_EQUAL)
		}
		return s.makeToken(TOKEN_MINUS)
	case '+':
		if s.match('=') {
			return s.makeToken(TOKEN_PLUS_EQUAL)
		}
		return s.makeToken(TOKEN_PLUS)
	case '/':
		if s.match('=') {
			return s.makeToken(TOKEN_SLASH_EQUAL)
		}
		return s.makeToken(TOKEN_SLASH)
	case '\\':
		if s.match('=') {
			return s.makeToken(TOKEN_BACK_SLASH_EQUAL)
		}
		return s.makeToken(TOKEN_BACKSLASH)
	case '*':
		if s.match('*') {
			return s.makeToken(TOKEN_STAR_STAR)
		}
		if s.match('=') {
			return s.makeToken(TOKEN_STAR_EQUAL)
		}
		return s.makeToken(TOKEN_STAR)
	case '%':
		if s.match('=') {
			return s.makeToken(TOKEN_PERCENT_EQUAL)
		}
		return s.makeToken(TOKEN_PERCENT)
	case '!':
		if s.match('=') {
			return s.makeToken(TOKEN_BANG_EQUAL)
		}
		return s.errorToken("Unexpected token")
	case '=':
		if s.match('=') {
			return s.makeToken(TOKEN_EQUAL_EQUAL)
		}
		if s.match('>') {
			return s.makeToken(TOKEN_EQUAL_ARROW)
		}
		return s.makeToken(TOKEN_EQUAL)
	case '<':
		if s.match('<') {
			return s.makeToken(TOKEN_LEFT_SHIFT)
		}
		if s.match('=') {
			return s.makeToken(TOKEN_LESS_EQUAL)
		}
		return s.makeToken(TOKEN_LESS)
	case '>':
		if s.match('>') {
			return s.makeToken(TOKEN_RIGHT_SHIFT)
		}
		if s.match('=') {
			return s.makeToken(TOKEN_GREATER_EQUAL)
		}
		return s.makeToken(TOKEN_GREATER)
	case '"':
		return s.scanString('"')
	case '\'':
		return s.scanString('\'')
	case '?':
		return s.makeToken(TOKEN_QUESTION_MARK)
	case ':':
		return s.makeToken(TOKEN_COLON)
	}

	return s.errorToken("Unexpected character.")
}

func (s *Scanner) ScanAll() []Token {
	s.line = 1
	s.start = 0
	s.current = 0

	tokens := []Token{}
	for {
		token := s.ScanToken()
		tokens = append(tokens, token)
		if token.Type == TOKEN_EOF || token.Type == TOKEN_ERROR {
			break
		}
	}
	return tokens
}
