package main

import "core:os"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:strconv"
import "core:slice"

Token_Id :: string
Token_Num :: int
Token_Op :: enum {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVISON,
    PRIME,
}

Sum :: struct {
    a, b: Token_Id,
}

List :: struct {
    start, end: int
}

Func_call :: struct{
    name: string,
    expr: [dynamic]Instruction,
}

Token :: struct {
    row: int,
    column: int,
    type: Token_Type,
    as: struct #raw_union {
        id: Token_Id,
        num: Token_Num,
        op: Token_Op,
    }
}

token_id :: proc(row, column: int, id: Token_Id) -> Token {
    return Token {
        row,
        column,
        .ID,
        {
            id = id
        }
    }
}

token_num :: proc(row, column: int, num: Token_Num) -> Token {
    return Token {
        row,
        column,
        .NUM,
        {
            num = num
        }
    }
}

token_op :: proc(row, column: int, op: Token_Op) -> Token {
    return Token {
        row,
        column,
        .OP,
        {
            op = op
        }
    }
}

token :: proc(row, column: int, type: Token_Type) -> Token {
    return Token {
        row,
        column,
        type,
        {}
    }
}

Token_Type :: enum {
    ID,
    NUM,
    OP,
    DDOT,
    LPAR,
    RPAR,
    LSQPAR,
    RSQPAR,
    COL,
    EQ,
    EOL,
    EOF
}

AST :: struct {
    variables: map[string][dynamic]Instruction,
    func_calls: [dynamic]Func_call,
}

PushNum :: int
PushOp :: Token_Op
PushVal :: Token_Id

Instruction :: union {
    PushNum,
    PushOp,
    PushVal,
}

Parser :: struct {
    tokens: []Token,
    current: int
}

parser_init :: proc(tokens: []Token) -> Parser {
    return Parser{tokens, -1}
}

parser_next :: proc(parser: ^Parser) -> Token {
    if parser.current < len(parser.tokens)-1 {
        parser.current += 1
    }
    return parser.tokens[parser.current]
}

parser_peek :: proc(parser: ^Parser) -> Token {
    if parser.current < len(parser.tokens)-1 {
        return parser.tokens[parser.current+1]
    }
    return parser.tokens[parser.current]
}

is_type :: proc(token: Token, type: Token_Type, log := true) -> (result: Token, ok: bool) {
    if token.type == .EOF {
        if log {
            fmt.printf("(%v:%v): Expected <%v> but found nothing", token.row, token.column, type)
        }
        return
    }
    if token.type != type { 
        if log {
            fmt.printf("(%v:%v): Expected <%v> but found: %v", token.row, token.column, type, token.type)
        }
        return
    }
    return token, true
}

is_id :: proc(token: Token) -> (result: Token_Id, ok: bool) {
    token := is_type(token, .ID) or_return
    return token.as.id, true
}

is_op :: proc(token: Token) -> (result: Token_Op, ok: bool) {
    token := is_type(token, .OP) or_return
    return token.as.op, true
}

is_num :: proc(token: Token) -> (result: Token_Num, ok: bool) {
    token := is_type(token, .NUM) or_return
    return token.as.num, true
}

is_exact_id :: proc(token: Token, expected: Token_Id) -> (ok: bool) {
    token := is_type(token, .ID) or_return
    return token.as.id == expected
}

is_exact_op :: proc(token: Token, expected: Token_Op) -> (ok: bool) {
    token := is_type(token, .OP) or_return
    return token.as.op == expected
}

is_exact_num :: proc(token: Token, expected: Token_Num) -> (ok: bool) {
    token := is_type(token, .NUM) or_return
    return token.as.num == expected
}

parser_save :: proc(parser: Parser) -> int {
    return parser.current
}

parser_recover :: proc(parser: ^Parser, pos: int) {
    parser.current = pos
}

parse_expr :: proc(parser: ^Parser, expr: ^[dynamic]Instruction, ast: ^AST) -> (ok: bool) {
    next := parser_next(parser)
    if next.type == .EOL || next.type == .EOF {
        return;
    }
    switch next.type {
    case .ID:
        if next.as.id not_in ast.variables {
            fmt.printf("%v", ast)
            fmt.printf("(%v:%v): Function does not exists: %#v", next.row, next.column, next.as.id)
            return
        }
        append(expr, next.as.id)
    case .NUM:
        append(expr, next.as.num)
    case .OP:
        switch next.as.op {
        case .PLUS: fallthrough
        case .MINUS: fallthrough
        case .MULTIPLY: fallthrough
        case .DIVISON:
            parse_expr(parser, expr, ast) or_return
            parse_expr(parser, expr, ast) or_return
            append(expr, next.as.op)
        case .PRIME:
            parse_expr(parser, expr, ast) or_return
            append(expr, next.as.op)
        }            
    case .EQ:
        unimplemented()
    case .LPAR: 
        unimplemented()
    case .RPAR:
        unimplemented()
    case .DDOT:
        unimplemented()
    case .LSQPAR:
        unimplemented()
    case .RSQPAR:
        unimplemented()
    case .COL:
        unimplemented()
    case .EOL: fallthrough
    case .EOF:
        return
    }
    return true
}

op_prio :: proc(op: Token_Op) -> (prio:int) {
    switch op {
    case .PLUS: fallthrough
    case .MINUS: return 1
    case .MULTIPLY: fallthrough
    case .DIVISON: return 2
    case .PRIME:
        fmt.panicf("Error <%v> in expresion", op)
    case: 
        unreachable()
    }
}

lex :: proc(file: string) -> (tokens: [dynamic]Token, ok: bool) {
    row := 1
    column := 1
    for i := 0; i < len(file); {
        for i < len(file) && unicode.is_white_space(cast(rune)file[i]) {
            if file[i] == '\n' { 
                row += 1
                column = 1
                append(&tokens, token(row, column, .EOL))
            }
            else {
                column += 1
            }
            i += 1
        }
        if i >= len(file) { break }
        if unicode.is_letter(cast(rune)file[i]) {
            istart := i
            for i < len(file) && unicode.is_letter(cast(rune)file[i]) {
                i += 1
            }
            tid := cast(Token_Id)file[istart:i]
            append(&tokens, token_id(row, column, tid))
            column += i - istart
        }
        else if unicode.is_digit(cast(rune)file[i]) {
            istart := i
            for i < len(file) && unicode.is_digit(cast(rune)file[i]) {
                i += 1
            }
            num, ok := strconv.parse_int(cast(string)file[istart:i])
            if !ok {
                fmt.printf("(%v:%v): Invalid number", row, column)
                return
            }
            tnum := cast(Token_Num)num
            append(&tokens, token_num(row, column, tnum))
            column += i - istart
        }
        else if file[i] == '=' {
            append(&tokens, token(row, column, .EQ))
            i += 1
            column += 1
        }
        else if file[i] == '(' {
            append(&tokens, token(row, column, .LPAR))
            i += 1
            column += 1
        }
        else if file[i] == ')' {
            append(&tokens, token(row, column, .RPAR))
            i += 1
            column += 1
        }
        else if file[i] == '[' {
            append(&tokens, token(row, column, .LSQPAR))
            i += 1
            column += 1
        }
        else if file[i] == ']' {
            append(&tokens, token(row, column, .RSQPAR))
            i += 1
            column += 1
        }
        else if file[i] == '*' {
            append(&tokens, token_op(row, column, .MULTIPLY))
            i += 1
            column += 1
        }
        else if file[i] == '+' {
            append(&tokens, token_op(row, column, .PLUS))
            i += 1
            column += 1
        }
        else if file[i] == '-' {
            append(&tokens, token_op(row, column, .MINUS))
            i += 1
            column += 1
        }
        else if file[i] == '\'' {
            append(&tokens, token_op(row, column, .PRIME))
            i += 1
            column += 1
        }
        else if file[i] == '/' {
            append(&tokens, token_op(row, column, .DIVISON))
            i += 1
            column += 1
        }
        else if i + 1 < len(file) && strings.compare(cast(string)file[i:i+2], "..") == 0 {
            append(&tokens, token(row, column, .DDOT))
            i += 2
            column += 2
        }
        else if file[i] == ':' {
            append(&tokens, token(row, column, .COL))
            i += 1
            column += 1
        }
        else {
            fmt.printf("(%v:%v): Unknown symbol: %v", row, column, cast(rune)file[i])
            return
        }
    }
    append(&tokens, token(row, column, .EOF))
    return tokens, true
}

parse :: proc(tokens: []Token) -> (ast: AST, ok: bool) {
    parser := parser_init(tokens)
    
    for {
        saved := parser_save(parser)
        next := parser_next(&parser)
        if next.type == .EOF {
            break
        }
        if next.type == .EOL {
            continue
        }
        id := is_id(next) or_return
        next = parser_next(&parser)

        _, ok := is_type(next, .COL, false)
        if ok {
            next = parser_next(&parser)
            is_type(next, .EQ) or_return

            expr: [dynamic]Instruction
            parse_expr(&parser, &expr, &ast) or_return
            ast.variables[id] = expr
            continue
        }
        parser_recover(&parser, saved)
        next = parser_next(&parser)
        
        if strings.compare(next.as.id, "print") == 0 {
            expr: [dynamic]Instruction
            parse_expr(&parser, &expr, &ast) or_return
            append(&ast.func_calls, Func_call{"print", expr})
        }
        else {
            fmt.printf("(%v:%v): Function does not exists: %#v", next.row, next.column, next.as.id)
            return
        }
    }
    return ast, true
}

generate_expr_asm :: proc(buffer: ^strings.Builder, expr: []Instruction, ast: ^AST) -> int {
    nums_count := 0
    for inst in expr {
        switch inst in inst{
        case PushNum:
            nums_count += 1
            fmt.sbprintf(buffer, "        push qword %v\n", inst)
        case PushOp:
            nums_count -= 2
            assert(nums_count >= 0)
            
            fmt.sbprintf(buffer, "        pop rbx\n")
            fmt.sbprintf(buffer, "        pop rax\n")
            
            switch inst {
            case .PLUS:
                fmt.sbprintf(buffer, "        add rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .MINUS: 
                fmt.sbprintf(buffer, "        sub rax, rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .MULTIPLY: 
                fmt.sbprintf(buffer, "        imul rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1
            case .DIVISON:
                fmt.sbprintf(buffer, "        idiv rbx\n")
                fmt.sbprintf(buffer, "        push rax\n")
                nums_count += 1

            case .PRIME: 
                unimplemented()
            case: 
                unreachable()
            }
        case PushVal:
            num := generate_expr_asm(buffer, ast.variables[inst][:], ast)
            nums_count += 1 + num
        }
    }
    return nums_count
}

generate_asm :: proc(ast: ^AST) {
    out, error := os.open("output.nasm", os.O_WRONLY | os.O_TRUNC | os.O_CREATE, 0o666)
    if error != nil {
        fmt.printf("Error during file creation: %v", error)
        return
    }
    
    buffer: strings.Builder
    fmt.sbprintf(&buffer, "global _start\n")
    fmt.sbprintf(&buffer, "section .data\n")
    fmt.sbprintf(&buffer, "buffer db 32\n")
    fmt.sbprintf(&buffer, "lsqpar db '['\n")
    fmt.sbprintf(&buffer, "rsqpar db ']'\n")
    fmt.sbprintf(&buffer, "commaspace db ', '\n")
    fmt.sbprintf(&buffer, "newline db 10\n")
    fmt.sbprintf(&buffer, "section .text\n")
    fmt.sbprintf(&buffer, "print:\n")
    fmt.sbprintf(&buffer, "        push rbp\n")
    fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
    fmt.sbprintf(&buffer, "        push qword 10 ; rbp - 8\n")
    fmt.sbprintf(&buffer, "        push qword 0  ; rbp - 16\n")
    fmt.sbprintf(&buffer, "        push rax\n")
    fmt.sbprintf(&buffer, "\n")
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
    fmt.sbprintf(&buffer, "_start:\n")
    
    for f, i in ast.func_calls {
        nums_count := generate_expr_asm(&buffer, f.expr[:], ast)
        
        fmt.sbprintf(&buffer, "        mov rax, [rsp]\n")
        fmt.sbprintf(&buffer, "        call print\n")
        fmt.sbprintf(&buffer, "        add rsp, %v\n", 8 * nums_count)
        fmt.sbprintf(&buffer, "        ; Write syscall\n")
        fmt.sbprintf(&buffer, "        mov rax, 1\n")
        fmt.sbprintf(&buffer, "        mov rdi, 1\n")
        fmt.sbprintf(&buffer, "        mov rsi, newline\n")
        fmt.sbprintf(&buffer, "        mov rdx, 1\n")
        fmt.sbprintf(&buffer, "        syscall\n")
        
    }
    
    fmt.sbprintf(&buffer, "        mov rax, 0x3c\n")
    fmt.sbprintf(&buffer, "        mov rdi, 0\n")
    fmt.sbprintf(&buffer, "        syscall\n")
    os.write(out, buffer.buf[:])

    os.close(out)
}

run :: proc() -> (main_ok: bool) {
    if len(os.args) < 2 {
        fmt.printf("Usage: ijaq <file>")
        return
    } 
    filename := os.args[1]
    
    file, ok := os.read_entire_file_from_filename(filename)
    if !ok {
        fmt.printf("Couldn't open a file: %v", filename)
        return
    }

    tokens := lex(cast(string)file) or_return
    ast := parse(tokens[:]) or_return
    fmt.printf("%#v", ast)
    generate_asm(&ast)
    
    return true
}

main :: proc() {
    if run() {
        os.exit(0)
    }
    os.exit(69)
}
