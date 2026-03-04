package main

import "core:os"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:strconv"

Token_Id  :: distinct string
Token_Str :: distinct string
Token_Num :: int

Token_Op :: enum {
    ADD,
    SUB,
    MUL,
    DIV,
    EQL,
    NEQ,
}

Token_Builtin_Type :: enum {
    INTEGER,
    BOOLEAN,
    STRING,
    VOID,
}

Token_Kind :: enum {
// Fields with payload
    ID,
    STR,
    NUM,
    OP,
    TYPE,
    KEYWORD,

// Fields without payload
    DDOT,
    LPAR,
    RPAR,
    LSQPAR,
    RSQPAR,
    LCPAR,
    RCPAR,
    COL,
    EQ,
    EOL,
    EOF,
    COMMA,
    ARROW,
}

Token_Keyword :: enum {
    IF,
    ELSE,
}

Token :: struct {
    line   : int,
    column : int,
    kind   : Token_Kind,
    handle : int,
}

Tokens :: struct {
    list     : [dynamic]Token,
    ids      : [dynamic]Token_Id,
    nums     : [dynamic]Token_Num,
    ops      : [dynamic]Token_Op,
    strs     : [dynamic]Token_Str,
    types    : [dynamic]Token_Builtin_Type,
    keywords : [dynamic]Token_Keyword,
}

// NOTE: Idk about that
token_with_payload :: proc(tokens: ^Tokens, line, column: int, payload: $T) {
    payload := payload
    token := Token {
        line   = line,
        column = column,
    }

    switch (typeid_of(T)) {
    case Token_Id:
        token.kind = .ID
        token.handle = len(tokens.ids)
        payload := (cast(^Token_Id)cast(^any)&payload)^
        append(&tokens.ids, payload)
    case Token_Str:
        token.kind = .STR
        token.handle = len(tokens.strs)
        payload := (cast(^Token_Str)cast(^any)&payload)^
        append(&tokens.strs, payload)
    case Token_Num:
        token.kind = .NUM
        token.handle = len(tokens.nums)
        payload := (cast(^Token_Num)cast(^any)&payload)^
        append(&tokens.nums, payload)
    case Token_Op:
        token.kind = .OP
        token.handle = len(tokens.ops)
        payload := (cast(^Token_Op)cast(^any)&payload)^
        append(&tokens.ops, payload)
    case Token_Builtin_Type:
        token.kind = .TYPE
        token.handle = len(tokens.types)
        payload := (cast(^Token_Builtin_Type)cast(^any)&payload)^
        append(&tokens.types, payload)
    case Token_Keyword:
        token.kind = .KEYWORD
        token.handle = len(tokens.keywords)
        payload := (cast(^Token_Keyword)cast(^any)&payload)^
        append(&tokens.keywords, payload)
    case:
        fmt.panicf("Unknown payload type: `%v`", typeid_of(T))
    }

    append(&tokens.list, token)
}

token_without_payload :: proc(tokens: ^Tokens, line, column: int, kind: Token_Kind) {
    token := Token {
        line   = line,
        column = column,
        kind   = kind,
        handle = -1,
    }
    append(&tokens.list, token)
}

token_get_id :: proc(tokens: ^Tokens, token: Token) -> Token_Id {
    assert(token.kind == .ID)
    return tokens.ids[token.handle]
}

token_get_type :: proc(tokens: ^Tokens, token: Token) -> Token_Builtin_Type {
    assert(token.kind == .TYPE)
    return tokens.types[token.handle]
}

token_get_num :: proc(tokens: ^Tokens, token: Token) -> Token_Num {
    assert(token.kind == .NUM)
    return tokens.nums[token.handle]
}

token_get_op :: proc(tokens: ^Tokens, token: Token) -> Token_Op {
    assert(token.kind == .OP)
    return tokens.ops[token.handle]
}

token_get_str :: proc(tokens: ^Tokens, token: Token) -> Token_Str {
    assert(token.kind == .STR)
    return tokens.strs[token.handle]
}

AST :: struct {
    bindings         : map[Token_Id]Bind,
    procedures       : [dynamic]Proc,
    main             : [dynamic]Instruction,
    bodies           : [dynamic]Instruction,
    parameters       : [dynamic]Func_Parameter,
    strs             : [dynamic]string,
    builtin_types    : [dynamic]Token_Builtin_Type,
    proc_types       : [dynamic]Proc_Type,
    proc_param_types : [dynamic]Type,
}

// We bind expression of some type to the name
// type   - type of the binded expression
// handle - index in corresponding array (e.g. procedures)
Bind :: struct {
    type: Type,
    handle: int,
    mutable: bool,
}

// Type of the expression
// kind   - kind of the type (e.g. proc, buitlin)
// handle - index in corresponding array (e.g. proc_types, builtin_types)
Type :: struct {
    kind: Type_Kind,
    handle: int,
}

Type_Kind :: enum {
    BUILTIN,
    PROC,
}

Proc_Type :: struct {
    param_types: Slice_Index,
    return_type: Type,
}

// TODO: Add support for several return types
Proc :: struct {
    body       : Slice_Index,
    parameters : Slice_Index,
    retype     : Type,
    line       : int,
    column     : int,
}

// TODO: We need to do something about it.
// Maybe attach pointer to dynamic array to slice,
// that way we would know from what array this slice was
// created. Also create functions for constructing indexed
// slice and retrieving regular slice from indexed.
//
// Or maybe we can create some kind of data-structure in
// which we would pre-allocate blocks of fixed size and
// then populate them as we go. That way we can just use
// regular slices but it will require more complex logic
// for management.
//
// We would need to keep track of all allocated blocks and how
// much memory in them are used. If when we append elements of
// one slice, the amount of free space in one block is not enough,
// we would need to find block with enough free space or allocate
// new one if none of them doesn't contain enough free space.
// After which move all elements of current slice there.
//
// It somewhat complicated but with this approach we don't need
// to worry about slices, and in theory this more optimal because
// we don't constantly reallocate space for growing dynamic array
// and dont' moving all content there.
Slice_Index :: struct {
    begin : int,
    end   : int,
}

Func_Parameter :: struct {
    name: Token_Id,
    type: Type,
}

Push_Num :: int
Push_Op  :: Token_Op
Push_Arg :: Token_Id

Func_Call :: struct{
    name: Token_Id,
}

Push_Str :: struct {
    id: int
}

Con_Jump :: struct {
    label: int
}

Jump :: struct {
    label: int
}

Label :: struct {
    label: int
}

Instruction :: union {
    Push_Num,
    Push_Op,
    Push_Arg,
    Push_Str,
    Func_Call,
    Jump,
    Con_Jump,
    Label,
}

Lexer :: struct {
    content : string,
    current : int,
    line    : int,
    column  : int,
    error   : bool,
}

KEYWORDS :: []string { "if", "else", "return" }

check_keywords :: proc(str: string) -> bool {
    for word in KEYWORDS {
        if strings.compare(str, word) == 0 do return true
    }
    return false
}

lexer_init :: proc(content: string) -> Lexer {
    return Lexer {
        content = content,
        current = -1,
        line    = 1,
        column  = 1,
        error   = false,
    }
}

lexer_next_char :: proc(lexer: ^Lexer) -> (char: rune, ok: bool) {
    if lexer.current + 1 >= len(lexer.content) do return
    lexer.current += 1
    return cast(rune)lexer.content[lexer.current], true
}

lexer_peek_char :: proc(lexer: ^Lexer) -> (char: rune, ok: bool) {
    if lexer.current + 1 >= len(lexer.content) do return
    return cast(rune)lexer.content[lexer.current + 1], true
}

lexer_next :: proc(lexer: ^Lexer, tokens: ^Tokens) -> (ok: bool) {
    for char in lexer_peek_char(lexer) {
        if !unicode.is_white_space(char) do break
        _ = lexer_next_char(lexer) or_else unreachable()
        lexer.column += 1
        if char == '\n' { 
            token_without_payload(tokens, lexer.line, lexer.column, .EOL)
            lexer.line += 1
            lexer.column = 1
            return true
        }
    }

    char := lexer_next_char(lexer) or_return
    // TODO: Add support for underscores and digits in names
    // TODO: Tokenize keywords separately
    if unicode.is_letter(char) {
        id_begin := lexer.current
        for char in lexer_peek_char(lexer) {
            if !unicode.is_letter(char) do break
            _ = lexer_next_char(lexer) or_else unreachable()
        }
        id := lexer.content[id_begin:lexer.current + 1]
        if strings.compare(id, "Integer") == 0 {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Builtin_Type.INTEGER)
            return true
        }
        if strings.compare(id, "String") == 0 {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Builtin_Type.STRING)
            return true
        }
        if strings.compare(id, "Boolean") == 0 {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Builtin_Type.BOOLEAN)
            return true
        }
        if strings.compare(id, "if") == 0 {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Keyword.IF)
            return true
        }
        if strings.compare(id, "else") == 0 {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Keyword.ELSE)
            return true
        }
        token_with_payload(tokens, lexer.line, lexer.column, Token_Id(id))
        return true
    }

    if unicode.is_digit(char) {
        num_begin := lexer.current
        for char in lexer_peek_char(lexer) {
            if !unicode.is_digit(char) do break
            _ = lexer_next_char(lexer) or_else unreachable()
        }
        num_str := lexer.content[num_begin:lexer.current + 1]
        if num, ok := strconv.parse_int(num_str); !ok {
            fmt.printf("(%v:%v): Invalid number", lexer.line, lexer.column)
            lexer.error = true
            return
        } else {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Num(num))
            return true
        }
    }
    if char == '"' {
        lexer_next_char(lexer) or_return
        str_begin := lexer.current
        for char in lexer_next_char(lexer) {
            if char == '"' do break
        }
        if lexer.content[lexer.current] != '"' {
            fmt.printf("(%v:%v): Invalid string literal", lexer.line, lexer.column)
            lexer.error = true
            return
        }
        str := lexer.content[str_begin:lexer.current]
        token_with_payload(tokens, lexer.line, lexer.column, Token_Str(str))
        return true
    }

    switch char {
    case '=':
        char := lexer_peek_char(lexer) or_return
        if char == '=' {
            lexer_next_char(lexer)
            token_with_payload(tokens, lexer.line, lexer.column, Token_Op.EQL)
            return true
        }
        token_without_payload(tokens, lexer.line, lexer.column, .EQ)
        return true
    case '(':
        token_without_payload(tokens, lexer.line, lexer.column, .LPAR)
        return true
    case ')':
        token_without_payload(tokens, lexer.line, lexer.column, .RPAR)
        return true
    case '[':
        token_without_payload(tokens, lexer.line, lexer.column, .LSQPAR)
        return true
    case ']':
        token_without_payload(tokens, lexer.line, lexer.column, .RSQPAR)
        return true
    case '{':
        token_without_payload(tokens, lexer.line, lexer.column, .LCPAR)
        return true
    case '}':
        token_without_payload(tokens, lexer.line, lexer.column, .RCPAR)
        return true
    case '.':
        char := lexer_next_char(lexer) or_return
        if char == '.' {
            token_without_payload(tokens, lexer.line, lexer.column, .DDOT)
            return true
        }
    case ':':
        token_without_payload(tokens, lexer.line, lexer.column, .COL)
        return true
    case '+':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.ADD)
        return true
    case '-':
        char, _ := lexer_peek_char(lexer)
        if char == '>' {
            lexer_next_char(lexer)
            token_without_payload(tokens, lexer.line,lexer.column, .ARROW)
            return true
        }
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.SUB)
        return true
    case '*':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.MUL)
        return true
    case '/':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.DIV)
        return true
    case ',':
        token_without_payload(tokens, lexer.line, lexer.column, .COMMA)
        return true
    case '!':
        char := lexer_next_char(lexer) or_return
        if char == '=' {
            token_with_payload(tokens, lexer.line, lexer.column, Token_Op.NEQ)
            return true
        }
    case '#':
        for char in lexer_peek_char(lexer) {
            _ = lexer_next_char(lexer) or_else unreachable()
            lexer.column += 1
            if char == '\n' { 
                token_without_payload(tokens, lexer.line, lexer.column, .EOL)
                lexer.line += 1
                lexer.column = 1
                return true
            }
        }
        token_without_payload(tokens, lexer.line, lexer.column, .EOF)
        return true
    }

    fmt.printf("(%v:%v): Unknown symbol: %v", lexer.line, lexer.column, char)
    lexer.error = true
    return
}

lexer_collect :: proc(lexer: ^Lexer, tokens: ^Tokens) -> (ok: bool) {
    for lexer_next(lexer, tokens) { }
    token_without_payload(tokens, lexer.line, lexer.column, .EOF)
    return !lexer.error
}

lex :: proc(st: ^State) -> (ok: bool) {
    lexer := lexer_init(cast(string)st.source)
    lexer_collect(&lexer, &st.tokens) or_return
    return true
}

Parser :: struct {
    tokens: ^Tokens,
    current: int
}

parser_init :: proc(tokens: ^Tokens) -> Parser {
    return Parser {
        tokens  = tokens,
        current = 0,
    }
}

parser_next :: proc(parser: ^Parser) -> Token {
    token := parser.tokens.list[parser.current]
    if parser.current < len(parser.tokens.list) {
        parser.current += 1
    }
    return token
}

parser_peek :: proc(parser: ^Parser) -> Token {
    return parser.tokens.list[parser.current]
}

parser_save :: proc(parser: Parser) -> int {
    return parser.current
}

parser_recover :: proc(parser: ^Parser, pos: int) {
    parser.current = pos
}

parse_definition :: proc(parser: ^Parser, ast: ^AST, id: Token_Id) -> (ok: bool) {
    expr : Bind
    procedure : Proc
    

    if parser_next(parser).kind != .LPAR do return
    
    param_count := len(ast.parameters)
    defer if error != .NONE {
        for len(ast.parameters) != param_count {
            pop(&ast.parameters)
            // NOTE: not sure if it would work
            pop(&ast.builtin_types)
        }
    }
    
    loop: for {
        if parser_peek(parser).kind == .ARROW {
            parser_next(parser)
            break
        }
        token_id : Token
        param_id : Token_Id
        {
            _token_id, ok := parser_expect(parser, .ID) 
            if !ok {
                fmt.printfln("P(%v:%v): Expected <id>, but got: %v", token_id.line, token_id.column, _token_id.kind)
                return .ETC
            }
            param_id = token_get_id(parser.tokens, _token_id)
            token_id = _token_id
        }
        if check_keywords(cast(string)param_id) {
            fmt.printfln("P(%v:%v): Expected <id>, but got keyword: %v", token_id.line, token_id.column, param_id)
            return .ETC
        }
        
        for param in ast.parameters[param_count:] {
            if strings.compare(string(param_id), string(param.name)) == 0 {
                fmt.printfln("P(%v:%v): Parameter already exists: %v", token_id.line, token_id.column, param_id)
                return .ETC
            }
        }
        
        if parser_next(parser).kind != .COL {
            fmt.printfln("P(%v:%v): Expected ':', but got: %v", token_id.line, token_id.column, param_id)
            return .ETC
        }
        token_type: Token
        type: Token_Builtin_Type
        {
            _token_type, ok := parser_expect(parser, .TYPE)
            if !ok {
                fmt.printfln("P(%v:%v): Expected <type>, but got: %v", token_type.line, token_type.column, token_type.kind)
                return .ETC
            }
            type = token_get_type(parser.tokens, _token_type)
            token_type = _token_type
        }
        // TODO: Add support for not Builtin types here
        handle := len(ast.builtin_types)
        append(&ast.builtin_types, type)
        append(&ast.parameters, Func_Parameter{param_id, Type{.BUILTIN, handle}})

        next := parser_next(parser) 
        #partial switch next.kind {
        case .ARROW:
            break loop
        case .COMMA:
            continue loop
        case:
            fmt.printfln("P(%v:%v): Expected '->' or ',', but got: %v", next.line, next.column, next.kind)
            return .ETC
        }
    }
    procedure.parameters = Slice_Index{param_count, len(ast.parameters)}
    
    procedure.retype = create_type(ast, Token_Builtin_Type.VOID)
    if parser_peek(parser).kind == .TYPE {
        pop(&ast.builtin_types)
        _token_type := parser_next(parser)
        type := token_get_type(parser.tokens, _token_type)
        handle := len(ast.builtin_types)
        
        append(&ast.builtin_types, type)
        procedure.retype = Type{.BUILTIN, handle}
    }

    if parser_next(parser).kind != .RPAR {
        fmt.printfln("P(%v:%v): Expected ')', but got: %v", next.line, next.column, next.kind)
        return .ETC
    }
    expr.handle = len(ast.procedures)
    append(&ast.procedures, procedure)
    handle := len(ast.proc_types)
    begin := len(ast.proc_param_types)
    for i in procedure.parameters.begin..<procedure.parameters.end {
        append(&ast.proc_param_types, ast.parameters[i].type)
    }  
    end := len(ast.proc_param_types)
    proc_type := Proc_Type{{begin, end}, procedure.retype}
    append(&ast.proc_types, proc_type)
    expr.type = Type{.PROC, handle}

    parse_expr(parser, &ast.bodies, ast, ast.parameters[procedure.parameters.begin:procedure.parameters.end])
    ast.bindings[id] = expr 
    return .NONE
}

parse_declarations :: proc(st: ^State) -> (ok: bool) {
    parser := parser_init(&st.tokens)
    for parser_peek(&parser).kind != .EOF {
        if parser_peek(&parser).kind != .ID {
            parser_next(&parser)
            continue
        }
        // TODO: think hard about this
        switch parse_declaration(&parser, &st.ast) {
        case .NONE:
        case .NOT_DECL: continue
        case .ETC: return 
        }
    }
    return true
}

parse_expr :: proc(parser: ^Parser, ast: ^AST, params: []Func_Parameter) -> (ok: bool) {
    @(static) label_count := 0
    next := parser_next(parser)
    #partial switch next.kind {
    case .ID:
        id := token_get_id(parser.tokens, next)
        if strings.compare(string(id), "if") == 0 {
            parse_expr(parser, expr, ast, params) or_return
            append(expr, Con_Jump{label_count})
            parse_expr(parser, expr, ast, params) or_return
            exprlen := len(expr)
            append(expr, Label{label_count})
            label_count += 1
            if parser_peek(parser).kind == .EOL {
                parser_next(parser)
            }
            token_id := parser_peek(parser)
            if token_id.kind == .ID {
                id := token_get_id(parser.tokens, token_id)
                if strings.compare(string(id), "else") == 0 {
                    parser_next(parser)
                    parse_expr(parser, expr, ast, params) or_return
                    inject_at(expr, exprlen, Jump{label_count})
                    append(expr, Label{label_count})
                }
            }
            label_count += 1
            return true
        }
        for param in params {
            if strings.compare(string(param.name), string(id)) == 0 {
                append(expr, id)
                return true
            }
        }
        if id not_in ast.bindings {
            fmt.printfln("P(%v:%v): Used not bounded id: %#v", next.line, next.column, id)
            return
        }
        assert(ast.bindings[id].type.kind == .PROC)

        func_call: Func_Call
        func_call.name = id
        call_params := ast.procedures[ast.bindings[id].handle].parameters
        for i in 0..<call_params.end - call_params.begin {
            parse_expr(parser, expr, ast, params) or_return
        }
        append(expr, func_call)
    case .NUM:
        num := token_get_num(parser.tokens, next)
        append(expr, num)
    case .OP:
        op := token_get_op(parser.tokens, next)
        switch op {
        case .ADD: fallthrough
        case .SUB: fallthrough
        case .MUL: fallthrough
        case .EQL: fallthrough
        case .NEQ: fallthrough
        case .DIV:
            parse_expr(parser, expr, ast, params) or_return
            parse_expr(parser, expr, ast, params) or_return
            append(expr, op)
        }
    case .STR:
        str := token_get_str(parser.tokens, next)
        append(expr, Push_Str{len(ast.strs)})
        append(&ast.strs, string(str))
    case .LCPAR:
        for {
            if parser_peek(parser).kind == .RCPAR {
                parser_next(parser)
                break
            }
            parse_expr(parser, expr, ast, params)
        }
    case .EOL, .EOF:
        return
    case:
        unimplemented()
    }
    
    return true
}

parser_expect :: proc(parser: ^Parser, expected: Token_Kind) -> (token: Token, ok: bool) {
    token = parser_next(parser)
    return token, token.kind == expected
}

parse :: proc(st: ^State) -> (ok: bool) {
    parser := parser_init(&st.tokens)
    
    loop: for {
        next := parser_next(&parser)
        switch next.kind {
        case .ID:
            next1 := parser_next(&parser)
            #partial switch next1.kind {
            case .COL:
                next := parser_next(parser)
                #partial switch next.kind {
                case .COL:
                    expr.mutable = false
                case .EQ:
                    expr.mutable = true
                case:
                    fmt.printfln("P(%v:%v): Expected ':' or '=', but got: %v", next.line, next.column, next.kind)
                    return 
                }
                parse_definition(&parser, &st.ast) or_return
            case .LPAR:
                
            }
        case .STR:
        
        case .NUM:
        
        case .OP:
        
        case .TYPE:
        
        case .DDOT:
        
        case .LPAR:
        
        case .RPAR:
        
        case .LSQPAR:
        
        case .RSQPAR:
        
        case .LCPAR:
        
        case .RCPAR:
        
        case .COL:
        
        case .EQ:
        
        case .EOL:
        
        case .EOF:
            break loop
        
        case .COMMA:
        
        case .ARROW:
        
        case .KEYWORD:
        
        case:
            unreachable()
        }
        // save := parser_save(parser)
        // next = parser_peek(&parser)
        // if next.kind == .ID {
        //     id := token_get_id(&st.tokens, next)
        //     switch parse_declaration(&parser, &st.ast) {
        //     case .NONE:
        //         body_begin := len(st.ast.bodies)
        //         procedure := &st.ast.procedures[st.ast.bindings[id].handle]
        //         parse_expr(&parser, &st.ast.bodies, &st.ast, get_parameters(&st.ast, id)) or_return
        //         procedure.body = {body_begin, len(st.ast.bodies)} 
        //         continue
        //     case .NOT_DECL:
        //     case .ETC:
        //         unreachable()
        //     }
        // }
        // parser_recover(&parser, save)
        // if parse_expr(&parser, &st.ast.main, &st.ast, {}) do continue

        // return
    }
    
    return true
}

create_type :: proc(ast: ^AST, type: $T) -> Type {
    type := type
    switch typeid_of(T) {
    case Token_Builtin_Type:
        handle := len(ast.builtin_types)
        t := (cast(^Token_Builtin_Type)cast(^any)&type)^
        append(&ast.builtin_types, t)
        return {.BUILTIN, handle}
    case Proc_Type:
        handle := len(ast.proc_types)
        t := (cast(^Proc_Type)cast(^any)&type)^
        append(&ast.proc_types, t)
        return {.PROC, handle}
    case:
        fmt.panicf("Unknown type: %v", typeid_of(T))
    }
}

type_stack_pop :: proc(ast: ^AST, type_stack: ^[dynamic]Type) -> Token_Builtin_Type {
    res := pop(type_stack)
    assert(res.kind == .BUILTIN)
    type := ast.builtin_types[res.handle]
    ordered_remove(&ast.builtin_types, res.handle)
    return type
}

get_parameters :: proc(ast: ^AST, id: Token_Id) -> []Func_Parameter {
    assert(ast.bindings[id].type.kind == .PROC)
    procedure := ast.procedures[ast.bindings[id].handle]
    proc_param := procedure.parameters
    return ast.parameters[proc_param.begin:proc_param.end]
}

compare_types :: proc(ast: ^AST, a, b: Type) -> bool {
    if a.kind != b.kind do return false
    switch a.kind {
        case .BUILTIN:
            return ast.builtin_types[a.handle] == ast.builtin_types[b.handle]
        case .PROC:
            unimplemented()
    }
    unreachable()
}

clone_type :: proc(ast: ^AST, type: Type) -> Type {
    assert(type.kind == .BUILTIN)
    return create_type(ast, ast.builtin_types[type.handle])    
}

type_check :: proc(
    instructions: []Instruction,
    params: []Func_Parameter,
    ast: ^AST,
    type_stack: ^[dynamic]Type
) -> Type {
    loop: for i := 0; i < len(instructions); i += 1 {
        sw: switch v in instructions[i] {
        case Push_Num:
            append(type_stack, create_type(ast, Token_Builtin_Type.INTEGER))
        case Push_Op:
            assert(type_stack_pop(ast, type_stack) == .INTEGER)
            assert(type_stack_pop(ast, type_stack) == .INTEGER)
            switch v {
            case .ADD:
                append(type_stack, create_type(ast, Token_Builtin_Type.INTEGER))
            case .SUB: 
                append(type_stack, create_type(ast, Token_Builtin_Type.INTEGER))
            case .MUL: 
                append(type_stack, create_type(ast, Token_Builtin_Type.INTEGER))
            case .DIV:
                append(type_stack, create_type(ast, Token_Builtin_Type.INTEGER))
            case .EQL:
                append(type_stack, create_type(ast, Token_Builtin_Type.BOOLEAN))
            case .NEQ:
                append(type_stack, create_type(ast, Token_Builtin_Type.BOOLEAN))
            case: 
                unreachable()
            }
        case Push_Arg:
            for param, i in params {
                if strings.compare(string(param.name), string(v)) == 0 {
                    append(type_stack, clone_type(ast, param.type))
                    break sw
                }
            }
            unreachable()
        case Func_Call:
            params := get_parameters(ast, v.name)
            #reverse for param in params {
                assert(param.type.kind == .BUILTIN)
                assert(type_stack_pop(ast, type_stack) == ast.builtin_types[param.type.handle])
            }
            handle := ast.procedures[ast.bindings[v.name].handle].retype.handle
            if ast.builtin_types[handle] != .VOID {
                append(type_stack, clone_type(ast, ast.procedures[ast.bindings[v.name].handle].retype))
            }
        case Jump:
            for v1, j in instructions[i+1:] {
                if label, ok := v1.(Label); ok {
                    if label.label == v.label {
                        i += j + 1
                        break
                    }
                }
            }
        case Con_Jump:
            assert(type_stack_pop(ast, type_stack) == .BOOLEAN)
            else_branch: Type
            type_stack_saved: [dynamic]Type
            defer delete(type_stack_saved)
            for type in type_stack {
                append(&type_stack_saved, clone_type(ast, type))
            }
            
            for v1, j in instructions[i+1:] {
                if label, ok := v1.(Label); ok {
                    if label.label == v.label {
                        else_branch = type_check(instructions[i+j+2:], params, ast, &type_stack_saved)
                        break
                    }
                }
            }
            
            clear(&type_stack_saved)
            for type in type_stack {
                append(&type_stack_saved, clone_type(ast, type))
            }
            if_branch := type_check(instructions[i+1:], params, ast, &type_stack_saved)
            assert(compare_types(ast, if_branch, else_branch))
            clear(type_stack)
            if ast.builtin_types[if_branch.handle] == .VOID do continue
            append(type_stack, if_branch)
            break loop
        case Label:
        case Push_Str:
            append(type_stack, create_type(ast, Token_Builtin_Type.STRING))
        }
    }
    assert(len(type_stack) <= 1)
    if len(type_stack) == 0 do return create_type(ast, Token_Builtin_Type.VOID)
    return type_stack[0]
}

generate_expr_asm :: proc(buffer: ^strings.Builder, expr: []Instruction, params: []Func_Parameter, ast: ^AST) {
    @(static) type_stack: [dynamic]Type
    clear(&type_stack)

    type_check(expr, params, ast, &type_stack)
    for inst, i in expr {
        sw: switch inst in inst {
        case Push_Num:
            fmt.sbprintf(buffer, "        push qword %v\n", inst)
        case Push_Op:
            fmt.sbprintf(buffer, "        pop rbx\n")
            fmt.sbprintf(buffer, "        pop rax\n")
            switch inst {
            case .ADD:
                fmt.sbprintf(buffer, "        add rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
            case .SUB: 
                fmt.sbprintf(buffer, "        sub rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
            case .MUL: 
                fmt.sbprintf(buffer, "        imul rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
            case .DIV:
                fmt.sbprintf(buffer, "        idiv rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
            case .EQL:
                fmt.sbprintf(buffer, "        mov rdx, 0\n")
                fmt.sbprintf(buffer, "        mov rcx, 1\n")
                fmt.sbprintf(buffer, "        cmp rax, rbx\n")
                fmt.sbprintf(buffer, "        cmove rdx, rcx\n")
                fmt.sbprintf(buffer, "        push rdx\n")
            case .NEQ:
                fmt.sbprintf(buffer, "        mov rdx, 1\n")
                fmt.sbprintf(buffer, "        mov rcx, 0\n")
                fmt.sbprintf(buffer, "        cmp rax, rbx\n")
                fmt.sbprintf(buffer, "        cmove rdx, rcx\n")
                fmt.sbprintf(buffer, "        push rdx\n")
            case: 
                unreachable()
            }
        case Push_Arg:
            for param, i in params {
                if strings.compare(string(param.name), string(inst)) == 0 {
                    fmt.sbprintf(buffer, "        push qword [rbp + %v]\n", (len(params) - i + 1)*8)
                    break sw
                }
            }
            unreachable()
        case Func_Call:
            params := get_parameters(ast, inst.name)
            byte_pop := 0
            #reverse for param in params {
                assert(param.type.kind == .BUILTIN)
                type := ast.builtin_types[param.type.handle]
                switch type {
                case .INTEGER:
                    byte_pop += 8
                case .STRING:
                    byte_pop += 16
                case .BOOLEAN:
                    byte_pop += 8
                case .VOID:
                    unreachable()
                }
            }
            if strings.compare(string(inst.name), "print") == 0 {
                fmt.sbprintf(buffer, "        mov rax, [rsp]\n")
                fmt.sbprintf(buffer, "        call print\n")
                fmt.sbprintf(buffer, "        add rsp, %v\n", byte_pop)
                fmt.sbprintf(buffer, "        ; Write syscall\n")
                fmt.sbprintf(buffer, "        mov rax, 1\n")
                fmt.sbprintf(buffer, "        mov rdi, 1\n")
                fmt.sbprintf(buffer, "        mov rsi, newline\n")
                fmt.sbprintf(buffer, "        mov rdx, 1\n")
                fmt.sbprintf(buffer, "        syscall\n")
            }
            else {
                fmt.sbprintf(buffer, "        call %v\n", inst.name)
                fmt.sbprintf(buffer, "        add rsp, %v\n", byte_pop)

                // TODO: make return types, now its only numbers
                bind := &ast.bindings[inst.name]
                assert(bind.type.kind == .PROC)
                return_type := ast.procedures[bind.handle].retype
                assert(return_type.kind == .BUILTIN)
                assert(ast.builtin_types[return_type.handle] != .STRING)

                if ast.builtin_types[return_type.handle] != .VOID {
                    fmt.sbprintf(buffer, "        push rax\n")
                }
            }
        case Jump:
            fmt.sbprintf(buffer, "        jmp .label_%v\n", inst.label)
        case Con_Jump:
            fmt.sbprintf(buffer, "        pop rax\n")
            fmt.sbprintf(buffer, "        cmp rax, 0\n")
            fmt.sbprintf(buffer, "        je .label_%v\n", inst.label)
        case Label:
            fmt.sbprintf(buffer, ".label_%v:\n", inst.label)
        case Push_Str:
            fmt.sbprintf(buffer, "        sub rsp, 16\n")
            fmt.sbprintf(buffer, "        mov qword [rsp+8], str%v\n", inst.id)
            fmt.sbprintf(buffer, "        mov qword [rsp], %v\n", len(ast.strs[inst.id]))
        }
    }
}

generate_asm :: proc(ast: ^AST) {
    out, error := os.open("output.nasm", os.O_WRONLY | os.O_TRUNC | os.O_CREATE, 0o666)
    if error != nil {
        fmt.printf("Error during file creation: %v", error)
        return
    }
    defer os.close(out)
    
    buffer: strings.Builder
    defer strings.builder_destroy(&buffer)
     
    fmt.sbprintf(&buffer, "section .data\n")
    fmt.sbprintf(&buffer, "newline db 10\n")
    for str, i in ast.strs {
        fmt.sbprintf(&buffer, "str%v db '%v'\n", i, str)
    }
    fmt.sbprintf(&buffer, "section .text\n")
    fmt.sbprintf(&buffer, "printstr:\n")
    fmt.sbprintf(&buffer, "        push rbp\n")
    fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
    fmt.sbprintf(&buffer, "        ; Write syscall\n")
    fmt.sbprintf(&buffer, "        mov rax, 1\n")
    fmt.sbprintf(&buffer, "        mov rdi, 1\n")
    fmt.sbprintf(&buffer, "        mov rsi, [rbp + 24]\n")
    fmt.sbprintf(&buffer, "        mov rdx, [rbp + 16]\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    fmt.sbprintf(&buffer, "        ; Write syscall\n")
    fmt.sbprintf(&buffer, "        mov rax, 1\n")
    fmt.sbprintf(&buffer, "        mov rdi, 1\n")
    fmt.sbprintf(&buffer, "        mov rsi, newline\n")
    fmt.sbprintf(&buffer, "        mov rdx, 1\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    fmt.sbprintf(&buffer, "        mov rsp, rbp\n")
    fmt.sbprintf(&buffer, "        pop rbp\n")
    fmt.sbprintf(&buffer, "        ret\n")
    fmt.sbprintf(&buffer, "\n")
    fmt.sbprintf(&buffer, "print:\n")
    fmt.sbprintf(&buffer, "        push rbp\n")
    fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
    fmt.sbprintf(&buffer, "        push qword 10 ; rbp - 8\n")
    fmt.sbprintf(&buffer, "        push qword 0  ; rbp - 16\n")
    fmt.sbprintf(&buffer, "        push rax\n")
    fmt.sbprintf(&buffer, ".lp:\n")
    fmt.sbprintf(&buffer, "        cqo\n")
    fmt.sbprintf(&buffer, "        idiv qword [rbp - 8]\n")
    fmt.sbprintf(&buffer, "        cmp rdx, 0\n")
    fmt.sbprintf(&buffer, "        jge .skip1\n")
    fmt.sbprintf(&buffer, "        neg rdx\n")
    fmt.sbprintf(&buffer, ".skip1:\n")
    fmt.sbprintf(&buffer, "        add rdx, '0'\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, "        mov [rsp], dl\n")
    fmt.sbprintf(&buffer, "        inc qword [rbp - 16]\n")
    fmt.sbprintf(&buffer, "        cmp rax, 0\n")
    fmt.sbprintf(&buffer, "        jne .lp\n")
    fmt.sbprintf(&buffer, "        cmp qword [rbp-24], 0\n")
    fmt.sbprintf(&buffer, "        jge .skip\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, "        mov [rsp], byte '-'\n")
    fmt.sbprintf(&buffer, "        inc qword [rbp - 16]\n")
    fmt.sbprintf(&buffer, ".skip:\n")
    fmt.sbprintf(&buffer, "        ; Write syscall\n")
    fmt.sbprintf(&buffer, "        mov rax, 1\n")
    fmt.sbprintf(&buffer, "        mov rdi, 1\n")
    fmt.sbprintf(&buffer, "        mov rsi, rsp\n")
    fmt.sbprintf(&buffer, "        mov rdx, [rbp - 16]\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    fmt.sbprintf(&buffer, "\n")
    fmt.sbprintf(&buffer, "        mov rsp, rbp\n")
    fmt.sbprintf(&buffer, "        pop rbp\n")
    fmt.sbprintf(&buffer, "        ret\n")

    for name, expr in ast.bindings {
        if expr.type.kind != .PROC do continue
        if strings.compare(string(name), "print") == 0 do continue
        if strings.compare(string(name), "printstr") == 0 do continue
        
        fmt.sbprintf(&buffer, "%v:\n", name)
        fmt.sbprintf(&buffer, "        push rbp\n")
        fmt.sbprintf(&buffer, "        mov rbp, rsp\n")

        procedure := ast.procedures[expr.handle]
        body := ast.bodies[procedure.body.begin:procedure.body.end]
        parameters := get_parameters(ast, name)
        generate_expr_asm(&buffer, body, parameters, ast)
        
        fmt.sbprintf(&buffer, "        pop rax\n")
        fmt.sbprintf(&buffer, "        mov rsp, rbp\n")
        fmt.sbprintf(&buffer, "        pop rbp\n")
        fmt.sbprintf(&buffer, "        ret\n")
    }
    
    fmt.sbprintf(&buffer, "global _start\n")
    fmt.sbprintf(&buffer, "_start:\n")
    
    generate_expr_asm(&buffer, ast.main[:], {}, ast)
    
    fmt.sbprintf(&buffer, "        mov rax, 0x3c\n")
    fmt.sbprintf(&buffer, "        mov rdi, 0\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    os.write(out, buffer.buf[:])
}

state_nuke :: proc(st: ^State) {
    delete(st.source)

    delete(st.tokens.list)
    delete(st.tokens.ids)
    delete(st.tokens.nums)
    delete(st.tokens.ops)
    delete(st.tokens.strs)
    delete(st.tokens.types)

    delete(st.ast.bindings)
    delete(st.ast.procedures)
    delete(st.ast.main)
    delete(st.ast.bodies)
    delete(st.ast.parameters)
    delete(st.ast.strs)
    delete(st.ast.builtin_types)
    delete(st.ast.proc_types)
    delete(st.ast.proc_param_types)
}

State :: struct {
    source: []byte,
    tokens: Tokens,
    ast: AST,
}

read_source :: proc(st: ^State) -> (ok: bool) {
    if len(os.args) < 2 {
        fmt.printf("Usage: ijaq <file>")
        return
    }

    filename := os.args[1]
    st.source, ok = os.read_entire_file_from_filename(filename)
    if !ok {
        fmt.printf("Couldn't open a file: %v", filename)
        return
    }
    return true
}

run :: proc() -> (ok: bool) {
    st: State
    defer state_nuke(&st)

    read_source(&st) or_return
    lex(&st) or_return
    
    // TODO: Do something about builtin functions
    // Probably don't handle them like regular functions.
    {
        procedure: Proc
        append(&st.ast.parameters, Func_Parameter{type = create_type(&st.ast, Token_Builtin_Type.INTEGER)})
        procedure.parameters.end = 1
        procedure.retype = create_type(&st.ast, Token_Builtin_Type.VOID)
    
        bind : Bind
        bind.handle = len(st.ast.procedures)
        append(&st.ast.procedures, procedure)
    
        proc_type : Proc_Type
        handle := len(st.ast.proc_param_types)
        append(&st.ast.proc_param_types, create_type(&st.ast, Token_Builtin_Type.INTEGER))
        proc_type.param_types = {handle, handle + 1}
        proc_type.return_type = create_type(&st.ast, Token_Builtin_Type.VOID)
        bind.type = create_type(&st.ast, proc_type)
        st.ast.bindings["print"] = bind
    }
    {
        procedure: Proc
        append(&st.ast.parameters, Func_Parameter{type = create_type(&st.ast, Token_Builtin_Type.STRING)})
        procedure.parameters.begin = 1
        procedure.parameters.end = 2
        procedure.retype = create_type(&st.ast, Token_Builtin_Type.VOID)
    
        bind : Bind
        bind.handle = len(st.ast.procedures)
        append(&st.ast.procedures, procedure)
    
        proc_type : Proc_Type
        handle := len(st.ast.proc_param_types)
        append(&st.ast.proc_param_types, create_type(&st.ast, Token_Builtin_Type.STRING))
        proc_type.param_types = {handle, handle + 1}
        proc_type.return_type = create_type(&st.ast, Token_Builtin_Type.VOID)
        bind.type = create_type(&st.ast, proc_type)
        st.ast.bindings["printstr"] = bind
    }
    
    parse(&st) or_return
    fmt.printf("%#v",st.ast)
    generate_asm(&st.ast)
    return true
}

main :: proc() {
    if run() { os.exit(0) }
    os.exit(69)
}
