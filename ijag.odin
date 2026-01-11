package main

import "core:os"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:strconv"

Token_Id :: distinct string
Token_Num :: distinct int
Token_Op :: enum {
    EQ,
    PLUS,
    LPAR,
    RPAR,
    LSQPAR,
    RSQPAR,
    DDOT,
}

Sum :: struct {
    a, b: Token_Id,
}

List :: struct {
    start, end: int
}

Func_call :: struct{
    name: string,
    arg: Token_Id,
}

Expr :: union {
    int,
    Sum,
    List,
}

Var :: struct {
    name: Token_Id,
    expr: Expr
}

Token :: struct {
    row: int,
    column: int,
    type: Token_Type,
}

Token_Type :: union{
    Token_Id,
    Token_Num,
    Token_Op,
}

AST :: struct {
    variables: [dynamic]Var,
    func_calls: [dynamic]Func_call,
}

next_token :: proc(tokens: []Token, index: ^int) -> ^Token
{   
    if index^ < len(tokens)-1 {
        index^ += 1
        return &tokens[index^]
    }
    return nil
}

var_exists :: proc(var: []Var, id: Token_Id) -> bool{
    for v in var{
        if strings.compare(cast(string)v.name, cast(string)id) == 0 {
            return true
        }
    } 
    return false
}
var_find :: proc(var: []Var, id: Token_Id) -> int{
    for v,i  in var{
        if strings.compare(cast(string)v.name, cast(string)id) == 0 {
            return i
        }
    } 
    return -1
}

expect_num :: proc(tokens: []Token, index: ^int) -> (result: Token_Num, ok: bool) {
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <num> but found nothing", current.row, current.column)
        return
    }
    if _num, ok := next.type.(Token_Num); !ok { 
        fmt.printf("(%v:%v): Expected <num> but found: %v", next.row, next.column, next.type)
        return
    }
    else {
        result = _num
    }
    return result, true
}

expect_id :: proc(tokens: []Token, index: ^int) -> (result: ^Token, ok: bool){
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <id> but found nothing", current.row, current.column)
        return
    }
    if _, ok := next.type.(Token_Id); !ok { 
        fmt.printf("(%v:%v): Expected <id> but found: %v", next.row, next.column, next.type)
        return
    }
    else {
        result = next
    }
    return result, true
}

expect_existing_id :: proc(tokens: []Token, index: ^int, variables:[]Var) -> (result:Token_Id, ok:bool) {
    id := expect_id(tokens, index) or_return
    if !var_exists(variables[:], id.type.(Token_Id)){
        fmt.printf("(%v:%v): Variable is not defined: %v", id.row, id.column, id.type.(Token_Id))
        return
    }
    return id.type.(Token_Id), true
}

expect_op :: proc(tokens: []Token, index: ^int) -> (result: ^Token, ok: bool){
    current := tokens[index^]
    next := next_token(tokens[:], index)
    if next == nil {
        fmt.printf("(%v:%v): Expected <op> but found nothing", current.row, current.column)
        return
    }
    result = next
    if _op, ok := next.type.(Token_Op); !ok { 
        fmt.printf("(%v:%v): Expected <op> but found: %v", next.row, next.column, next.type)
        return
    }
    return result, true
}
expect_exact_op :: proc(tokens: []Token, index: ^int, expected: Token_Op) -> bool{
    op := expect_op(tokens, index) or_return
    if op.type != expected {
        fmt.printf("(%v:%v): Expected `%v` but found: %v", op.row, op.column, expected, op.type)
        return false
    }
    return true
}

lex :: proc(file: string) -> (tokens: [dynamic]Token, ok: bool) {
    row := 1
    column := 1
    for i := 0; i < len(file); {
        for i < len(file) && unicode.is_white_space(cast(rune)file[i]) {
            if file[i] == '\n' { 
                row += 1
                column = 1
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
            append(&tokens,Token{row, column, tid})
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
            append(&tokens, Token{row, column, tnum})
            column += i - istart
        }
        else if file[i] == '=' {
            append(&tokens, Token{row, column, Token_Op.EQ})
            i += 1
            column += 1
        }
        else if file[i] == '+' {
            append(&tokens, Token{row, column, Token_Op.PLUS})
            i += 1
            column += 1
        }
        else if file[i] == '(' {
            append(&tokens, Token{row, column, Token_Op.LPAR})
            i += 1
            column += 1
        }
        else if file[i] == ')' {
            append(&tokens, Token{row, column, Token_Op.RPAR})
            i += 1
            column += 1
        }
        else if file[i] == '[' {
            append(&tokens, Token{row, column, Token_Op.LSQPAR})
            i += 1
            column += 1
        }
        else if file[i] == ']' {
            append(&tokens, Token{row, column, Token_Op.RSQPAR})
            i += 1
            column += 1
        }
        else if i + 1 < len(file) && strings.compare(cast(string)file[i:i+2], "..") == 0 {
            append(&tokens, Token{row, column, Token_Op.DDOT})
            i += 2
            column += 2
        }
        else {
            fmt.printf("(%v:%v): Unknown symbol: %v", row, column, cast(rune)file[i])
            return
        }
    }
    return tokens, true
}

parse :: proc(tokens: []Token) -> (ast: AST, ok: bool) {
    for i := 0; i < len(tokens); {
        curr_token := &tokens[i]
        switch t in tokens[i].type {
        case Token_Id:
            op, ok := expect_op(tokens, &i)
            if !ok { return }
            switch op.type.(Token_Op) {
            case .EQ:
                if var_exists(ast.variables[:], t) {
                    fmt.printf("(%v:%v): Variable redeclaration is prohibited! F@ck U!", curr_token.row, curr_token.column)
                    return
                }
                lhs := next_token(tokens, &i)
                if lhs == nil {
                    fmt.printf("(%v:%v): Expected token not found", curr_token.row, curr_token.column)
                    return
                }
                switch lhst in lhs.type {
                case Token_Id:
                    if !var_exists(ast.variables[:], lhst) {
                        fmt.printf("(%v:%v): Variable is not declared: %v", lhs.row, lhs.column, lhst)
                        return
                    }
                    if !expect_exact_op(tokens, &i, .PLUS) { return }
                    rhs := expect_existing_id(tokens, &i, ast.variables[:]) or_return
                    
                    append(&ast.variables, Var{t, Sum{lhst, rhs}})
                case Token_Num:
                    append(&ast.variables, Var{t, cast(int)lhst})
                case Token_Op:
                    if lhst != .LSQPAR {
                        fmt.printf("(%v:%v): Expected `[` got: %v", lhs.row, lhs.column, lhst)
                        return
                    }
                    start := expect_num(tokens, &i) or_return
                    expect_exact_op(tokens, &i, .DDOT) or_return
                    // end := expect_num(tokens, &i) or_return
                    // hardcoded pseudo infinite list
                    next := next_token(tokens[:], &i)
                    if next == nil {
                        fmt.printf("(%v:%v): Expected <num> but found nothing", curr_token.row, curr_token.column)
                        return
                    }
                    end: int
                    if _num, ok := next.type.(Token_Num); !ok { 
                        end = max(int)
                        i -= 1
                    }
                    else {
                        end = cast(int)_num
                    }
                    
                    expect_exact_op(tokens, &i, .RSQPAR) or_return
                    append(&ast.variables, Var{t, List{cast(int)start, cast(int)end}})
                }
            case .LPAR:
                if strings.compare(cast(string)t, "print") != 0 {
                    fmt.printf("(%v:%v): Function does not exists: %#v", curr_token.row, curr_token.column, t)
                    return
                }
                arg := expect_existing_id(tokens, &i, ast.variables[:]) or_return

                expect_exact_op(tokens, &i, .RPAR) or_return       
                append(&ast.func_calls, Func_call{"print", arg} )
            case .RPAR:
                unreachable()
            case .PLUS:
                unreachable()
            case .LSQPAR:
                unreachable()
            case .RSQPAR:
                unreachable()
            case .DDOT:
                unreachable()
            case:
                unimplemented()
            }
            i += 1
        case Token_Num:
            unimplemented()
        case Token_Op:
            unimplemented()
        case:
            unreachable()
        }
        
    }
    return ast, true
}

generate_asm :: proc(ast: AST) {
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
    fmt.sbprintf(&buffer, "\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, ".lp:\n")
    fmt.sbprintf(&buffer, "        xor rdx, rdx\n")
    fmt.sbprintf(&buffer, "        div qword [rbp - 8]\n")
    fmt.sbprintf(&buffer, "        add rdx, '0'\n")
    fmt.sbprintf(&buffer, "        sub rsp, 1\n")
    fmt.sbprintf(&buffer, "        mov [rsp], dl\n")
    fmt.sbprintf(&buffer, "        inc qword [rbp - 16]\n")
    fmt.sbprintf(&buffer, "        cmp rax, 0\n")
    fmt.sbprintf(&buffer, "        jne .lp\n")
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
    fmt.sbprintf(&buffer, "        mov rbp, rsp\n")
    fmt.sbprintf(&buffer, "        sub rsp, %v\n", len(ast.variables)*8)

    for v, i in ast.variables {
        switch exp in v.expr {
        case int:
            fmt.sbprintf(&buffer, "        mov qword [rbp-%v], %v\n",(i+1)*8, exp)
        
        case Sum:
            a := var_find(ast.variables[:], exp.a)
            b := var_find(ast.variables[:], exp.b)
            fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (a+1)*8)
            fmt.sbprintf(&buffer, "        add rax, [rbp-%v]\n", (b+1)*8)
            fmt.sbprintf(&buffer, "        mov [rbp-%v], rax\n", (i+1)*8)
        
        case List:

        case:
            unreachable()
        }
    }
    
    for f, i in ast.func_calls {
        arg := var_find(ast.variables[:], f.arg)
        switch t in ast.variables[arg].expr {
        case Sum:
            fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (arg+1)*8)
            fmt.sbprintf(&buffer, "        call print\n")
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, newline\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
        case int:
            fmt.sbprintf(&buffer, "        mov rax, [rbp-%v]\n", (arg+1)*8)
            fmt.sbprintf(&buffer, "        call print\n")
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, newline\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
        case List:
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, lsqpar\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
            
            fmt.sbprintf(&buffer, "        push qword %v\n", t.start)
            fmt.sbprintf(&buffer, ".printlist%v:\n", i)
            fmt.sbprintf(&buffer, "        mov rax, [rsp]\n")
            fmt.sbprintf(&buffer, "        call print\n")
            
            fmt.sbprintf(&buffer, "        mov rax, %v\n", t.end)
            fmt.sbprintf(&buffer, "        cmp [rsp], rax\n")
            fmt.sbprintf(&buffer, "        je .skipcommaspace%v\n", i)
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, commaspace\n")
            fmt.sbprintf(&buffer, "        mov rdx, 2\n")
            fmt.sbprintf(&buffer, "        syscall\n")
            
            fmt.sbprintf(&buffer, ".skipcommaspace%v:\n", i)
            fmt.sbprintf(&buffer, "        inc qword [rsp]\n")
            fmt.sbprintf(&buffer, "        mov rax, %v\n", t.end)
            fmt.sbprintf(&buffer, "        cmp [rsp], rax\n")
            fmt.sbprintf(&buffer, "        jbe .printlist%v\n", i)
            
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, rsqpar\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
            
            fmt.sbprintf(&buffer, "        ; Write syscall\n")
            fmt.sbprintf(&buffer, "        mov rax, 1\n")
            fmt.sbprintf(&buffer, "        mov rdi, 1\n")
            fmt.sbprintf(&buffer, "        mov rsi, newline\n")
            fmt.sbprintf(&buffer, "        mov rdx, 1\n")
            fmt.sbprintf(&buffer, "        syscall\n")
            fmt.sbprintf(&buffer, "        add rsp, 8\n")
        } 
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
    generate_asm(ast)
    
    return true
}

main :: proc() {
    if run() {
        os.exit(0)
    }
    os.exit(69)
}
