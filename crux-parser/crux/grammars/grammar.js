module.exports = grammar({
  name: "crux",

  conflicts: $ => [
    [$._expression, $._statement],
    [$.block, $.table_expression],
    [$._expression, $.struct_instantiation],
    [$.call_expression, $.typeof_expression],
    [$.binary_expression, $.typeof_expression],
    [$.index_expression, $.typeof_expression],
    [$.property_expression, $.typeof_expression],
  ],

  precedences: $ => [
    [$.match_expression, $.call_expression],
  ],

  extras: $ => [
    $.comment,
    /[\s\ufeff]/,
  ],

  inline: $ => [
    $._literal,
  ],

  rules: {
    program: $ => repeat($._statement),

    _statement: $ => choice(
      $.expression_statement,
      $.variable_declaration,
      $.function_declaration,
      $.struct_declaration,
      $.return_statement,
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.match_expression,
      $.break_statement,
      $.continue_statement,
      $.use_statement,
      $.pub_declaration,
      $.panic_statement,
      $.block,
    ),

    block: $ => seq("{", repeat($._statement), "}"),

    expression_statement: $ => seq($._expression, ";"),

    comment: $ => token(choice(
      seq("//", /[^\n]*/),
    )),

    variable_declaration: $ => seq(
      "let",
      $.identifier,
      optional(seq("=", $._expression)),
      ";"
    ),

    function_declaration: $ => seq(
      "fn",
      $.identifier,
      $.parameters,
      $.block
    ),

    parameters: $ => seq(
      "(",
      optional(seq(
        $.identifier,
        repeat(seq(",", $.identifier))
      )),
      ")"
    ),

    struct_declaration: $ => seq(
      "struct",
      $.identifier,
      "{",
      optional(seq(
        $.identifier,
        repeat(seq(",", $.identifier))
      )),
      "}"
    ),

    return_statement: $ => seq(
      "return",
      optional($._expression),
      ";"
    ),

    if_statement: $ => seq(
      "if",
      $._expression,
      $.block,
      optional(seq("else", choice($.block, $.if_statement)))
    ),

    while_statement: $ => seq(
      "while",
      $._expression,
      $.block
    ),

    for_statement: $ => seq(
      "for",
      optional($.variable_declaration),
      optional(seq($._expression, ";")),
      optional($._expression),
      $.block
    ),

    match_expression: $ => seq(
      "match",
      $._expression,
      "{",
      repeat($.match_arm),
      "}"
    ),

    match_arm: $ => seq(
      choice(
        $.match_pattern,
        "default",
      ),
      "=>",
      choice($.block, seq($._expression, ";")),
    ),

    match_pattern: $ => choice(
      seq("Ok", optional(seq("(", $.identifier, ")"))),
      seq("Err", optional(seq("(", $.identifier, ")"))),
      $._expression,
    ),

    break_statement: $ => seq("break", ";"),

    continue_statement: $ => seq("continue", ";"),

    use_statement: $ => seq(
      "use",
      $.identifier,
      optional(seq("as", $.identifier)),
      "from",
      $.string,
      ";"
    ),

    pub_declaration: $ => seq(
      "pub",
      choice(
        $.function_declaration,
        $.variable_declaration,
        $.struct_declaration
      )
    ),

    panic_statement: $ => seq("panic", $._expression, ";"),

    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.index_expression,
      $.property_expression,
      $._literal,
      $.identifier,
      $.function_expression,
      $.array_expression,
      $.table_expression,
      $.struct_instantiation,
      $.grouped_expression,
      $.match_expression,
      $.typeof_expression,
    ),

    grouped_expression: $ => seq("(", $._expression, ")"),

    binary_expression: $ => prec.left(seq(
      $._expression,
      $.binary_operator,
      $._expression,
    )),

    binary_operator: $ => choice(
      "+", "-", "*", "/", "\\", "%", "**",
      "<<", ">>",
      "==", "!=", "<", ">", "<=", ">=",
      "and", "or",
    ),

    unary_expression: $ => prec.right(seq(
      $.unary_operator,
      $._expression,
    )),

    unary_operator: $ => choice("-", "not"),

    call_expression: $ => seq(
      $._expression,
      $.arguments
    ),

    arguments: $ => seq(
      "(",
      optional(seq(
        $._expression,
        repeat(seq(",", $._expression))
      )),
      ")"
    ),

    index_expression: $ => seq(
      $._expression,
      "[",
      $._expression,
      "]"
    ),

    property_expression: $ => seq(
      $._expression,
      ".",
      $.identifier
    ),

    function_expression: $ => seq(
      "fn",
      $.parameters,
      $.block
    ),

    array_expression: $ => seq(
      "[",
      optional(seq(
        $._expression,
        repeat(seq(",", $._expression))
      )),
      "]"
    ),

    table_expression: $ => seq(
      "{",
      optional(seq(
        $.table_pair,
        repeat(seq(",", $.table_pair))
      )),
      "}"
    ),

    table_pair: $ => seq(
      $._expression,
      ":",
      $._expression
    ),

    struct_instantiation: $ => seq(
      $.identifier,
      "{",
      optional(seq(
        $.field_pair,
        repeat(seq(",", $.field_pair))
      )),
      "}"
    ),

    field_pair: $ => seq(
      $.identifier,
      ":",
      $._expression
    ),

    typeof_expression: $ => seq(
      "typeof",
      $._expression
    ),

    _literal: $ => choice(
      $.number,
      $.string,
      $.boolean,
      $.nil,
    ),

    number: $ => choice(
      token(/[0-9]+/),
      token(/[0-9]*\.[0-9]+/),
    ),

    string: $ => choice(
      token(/"([^"\\]|\\.)*"/),
      token(/'([^'\\]|\\.)*'/),
    ),

    boolean: $ => choice("true", "false"),

    nil: $ => "nil",

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
  }
});
