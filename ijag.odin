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
    PRIME,
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
    ID,
    NUM,
    TYPE,
    OP,
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
    STR,
}

Token :: struct {
    line   : int,
    column : int,
    kind   : Token_Kind,
    handle : int,
}

Tokens :: struct {
    list  : [dynamic]Token,
    ids   : [dynamic]Token_Id,
    nums  : [dynamic]Token_Num,
    ops   : [dynamic]Token_Op,
    strs  : [dynamic]Token_Str,
    types : [dynamic]Token_Builtin_Type,
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
    functions  : map[Token_Id]Func_Def,
    main       : [dynamic]Instruction,
    bodies     : [dynamic]Instruction,
    parameters : [dynamic]Func_Parameter,
    strs       : [dynamic]string,
}

Func_Decl :: struct {
    id         : Token_Id,
    parameters : []Func_Parameter,
    line       : int,
    column     : int,
}

Func_Def :: struct {
    body       : Slice_Index,
    parameters : Slice_Index,
}

Slice_Index :: struct {
    begin : int,
    end   : int,
}

Func_Parameter :: struct {
    name: Token_Id,
    type: Token_Builtin_Type,
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
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.SUB)
        return true
    case '*':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.MUL)
        return true
    case '/':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.DIV)
        return true
    case '\'':
        token_with_payload(tokens, lexer.line, lexer.column, Token_Op.PRIME)
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

parse_declaration :: proc(parser: ^Parser) -> (decl: Func_Decl, ok: bool){
    @(static) params: [dynamic]Func_Parameter
    clear(&params)

    token_id := parser_expect(parser, .ID) or_return
    id := token_get_id(parser.tokens, token_id)

    decl.id = id
    decl.line = token_id.line
    decl.column = token_id.column
    
    if strings.compare(string(id), "if") == 0 || strings.compare(string(id), "else") == 0 {
        return
    }
    
    next := parser_next(parser)
    #partial switch next.kind {
    case .LPAR:
        loop: for {
            token_id := parser_expect(parser, .ID) or_return
            id := token_get_id(parser.tokens, token_id)
            
            for param in params {
                if strings.compare(string(id), string(param.name)) == 0 {
                    return
                }
            }

            parser_expect(parser, .COL) or_return
            token_type := parser_expect(parser, .TYPE) or_return
            type := token_get_type(parser.tokens, token_type)
            append(&params, Func_Parameter{id, type})

            next := parser_next(parser) 
            #partial switch next.kind {
            case .RPAR:
                break loop
            case .COMMA:
                continue loop
            case:
                return
            }
        }
        parser_expect(parser, .COL) or_return
        fallthrough
    case .COL:
        token_type := parser_expect(parser, .TYPE) or_return
        type := token_get_type(parser.tokens, token_type)
        assert(type == .INTEGER)
    case:
        return
    }
    decl.parameters = params[:]
    return decl, true
}

parse_declarations :: proc(st: ^State) -> (ok: bool) {
    parser := parser_init(&st.tokens)
    for parser_peek(&parser).kind != .EOF {
        if parser_peek(&parser).kind != .ID {
            parser_next(&parser)
            continue
        }
        decl := parse_declaration(&parser) or_continue
        if decl.id in st.ast.functions {
            fmt.printf("(%v:%v): Function is already declared: %#v", decl.line, decl.column, decl.id)
            return 
        }
        param_begin := len(st.ast.parameters)
        append(&st.ast.parameters, ..decl.parameters)
        st.ast.functions[decl.id] = Func_Def{parameters = {param_begin, len(st.ast.parameters)}}
    }
    return true
}

parse_expr :: proc(parser: ^Parser, expr: ^[dynamic]Instruction, ast: ^AST, params: []Func_Parameter) -> (ok: bool) {
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
        if id not_in ast.functions {
            fmt.printf("(%v:%v): Function does not exists: %#v", next.line, next.column, id)
            return
        }

        func_call: Func_Call
        func_call.name = id
        call_params := ast.functions[id].parameters
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
        case .PRIME:
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
    parse_declarations(st) or_return
    parser := parser_init(&st.tokens)
    
    for {
        next := parser_peek(&parser)
        if next.kind == .EOF {
            break
        }
        if next.kind == .EOL {
            parser_next(&parser)
            continue
        }
        save := parser_save(parser)
        if decl, ok := parse_declaration(&parser); ok {
            body_begin := len(st.ast.bodies)
            def := &st.ast.functions[decl.id]
            params := st.ast.parameters[def.parameters.begin:def.parameters.end]
            parse_expr(&parser, &st.ast.bodies, &st.ast, params) or_return
            def.body = {body_begin, len(st.ast.bodies)} 
            continue
        }
        parser_recover(&parser, save)
        if parse_expr(&parser, &st.ast.main, &st.ast, {}) do continue

        return
    }
    return true
}

type_check :: proc(
    instructions: []Instruction,
    params: []Func_Parameter,
    ast: ^AST,
    type_stack: ^[dynamic]Token_Builtin_Type
) -> Token_Builtin_Type {
    loop: for i := 0; i < len(instructions); i += 1 {
        sw: switch v in instructions[i] {
        case Push_Num:
            append(type_stack, Token_Builtin_Type.INTEGER)
        case Push_Op:
            assert(pop(type_stack) == .INTEGER)
            assert(pop(type_stack) == .INTEGER)
            switch v {
            case .ADD:
                append(type_stack, Token_Builtin_Type.INTEGER)
            case .SUB: 
                append(type_stack, Token_Builtin_Type.INTEGER)
            case .MUL: 
                append(type_stack, Token_Builtin_Type.INTEGER)
            case .DIV:
                append(type_stack, Token_Builtin_Type.INTEGER)
            case .EQL:
                append(type_stack, Token_Builtin_Type.BOOLEAN)
            case .NEQ:
                append(type_stack, Token_Builtin_Type.BOOLEAN)
            case .PRIME: 
                unimplemented()
            case: 
                unreachable()
            }
        case Push_Arg:
            for param, i in params {
                if strings.compare(string(param.name), string(v)) == 0 {
                    append(type_stack, param.type)
                    break sw
                }
            }
            unreachable()
        case Func_Call:
            params := ast.functions[v.name].parameters
            params_count := params.end - params.begin
            #reverse for param in ast.parameters[params.begin:params.end] {
                assert(pop(type_stack) == param.type)
            }
            if strings.compare(string(v.name), "print") != 0 && strings.compare(string(v.name), "printstr") != 0 {
                append(type_stack, Token_Builtin_Type.INTEGER)
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
            assert(pop(type_stack) == .BOOLEAN)
            else_branch: Token_Builtin_Type
            type_stack_saved: [dynamic]Token_Builtin_Type
            defer delete(type_stack_saved)
            append(&type_stack_saved, ..type_stack[:])
            
            for v1, j in instructions[i+1:] {
                if label, ok := v1.(Label); ok {
                    if label.label == v.label {
                        else_branch = type_check(instructions[i+j+2:], params, ast, &type_stack_saved)
                        break
                    }
                }
            }
            
            clear(&type_stack_saved)
            append(&type_stack_saved, ..type_stack[:])
            if_branch := type_check(instructions[i+1:], params, ast, &type_stack_saved)
            assert(if_branch == else_branch)
            clear(type_stack)
            if if_branch == .VOID do continue
            append(type_stack, if_branch)
            break loop
        case Label:
        case Push_Str:
            append(type_stack, Token_Builtin_Type.STRING)
        }
    }
    assert(len(type_stack) <= 1)
    if len(type_stack) == 0 do return .VOID
    return type_stack[0]
}

generate_expr_asm :: proc(buffer: ^strings.Builder, expr: []Instruction, params: []Func_Parameter, ast: ^AST) {
    @(static) type_stack: [dynamic]Token_Builtin_Type
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
            case .PRIME: 
                unimplemented()
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
            params := ast.functions[inst.name].parameters
            params_count := params.end - params.begin 
            byte_pop := 0
            #reverse for param in ast.parameters[params.begin:params.end] {
                switch param.type {
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
                if strings.compare(string(inst.name), "printstr") != 0 {
                    // TODO: make return types, now its only numbers
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

    for name, def in ast.functions {
        if strings.compare(string(name), "print") == 0 do continue
        if strings.compare(string(name), "printstr") == 0 do continue
        fmt.sbprintf(&buffer, "%v:\n", name)
        fmt.sbprintf(&buffer, "        push rbp\n")
        fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
        body := ast.bodies[def.body.begin:def.body.end]
        parameters := ast.parameters[def.parameters.begin:def.parameters.end]
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

    delete(st.ast.main)
    delete(st.ast.functions)
    delete(st.ast.bodies)
    delete(st.ast.parameters)
    delete(st.ast.strs)
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
    def: Func_Def
    append(&st.ast.parameters, Func_Parameter{type = .INTEGER})
    def.parameters.end = 1
    st.ast.functions["print"] = def
    append(&st.ast.parameters, Func_Parameter{type = .STRING})
    def.parameters.begin = 1
    def.parameters.end = 2
    st.ast.functions["printstr"] = def
    
    parse(&st) or_return
    generate_asm(&st.ast)
    return true
}

main :: proc() {
    if run() { os.exit(0) }
    os.exit(69)
}
