include("matchtools.jl")
test2{T, T2}(a::T, b::T2) = a + b
#

#function ast_macro(f, types)
function test{T}(a::T, b)
    c = a+b::T
    if c == 10
        for i=1:7
            c += rand(Bool) ? 1 : 2
        end
    end
    f = (x,y) -> x^y
    c, f
end

function get_ast(li::LambdaInfo)
    ast = li.code
    if isa(ast, Vector{UInt8})
        return Base.uncompressed_ast(li)
    end
    ast
end


"""
Replaces `goto` statements in a loop body with continue and break.
"""
function replace_continue_break(astlist, continue_label, break_label)
    map(astlist) do elem
        if isa(elem, GotoNode) && elem.label == continue_label.label
            Expr(:continue)
        elseif isa(elem, GotoNode) && elem.label == break_label.label
            Expr(:break)
        else
            elem
        end
    end
end
function remove_goto(ast)
    # if else
    ifelse_pattern = (
        isunless, # if branch
        anything, # if body
        isgoto, # goto to jump over else
        (l,h)->is_unless_label(l, h, 1),
        anything,   # else body
        (l,h)->is_goto_label(l, h, 3) # label matching above goto
    )
    ast = _replace(ast, ifelse_pattern) do unless, ifbody, _1, _2, elsebody, _3
        condition = unless.args[1]
        ifbody = Expr(:block, remove_goto(ifbody)...)
        elsebody = Expr(:block, remove_goto(elsebody)...)
        Expr(:if, condition, ifbody, elsebody)
    end

    while_pattern = (
        islabelnode, # loop goto label
        isunless, # while condition branch
        anything, # body
        Greed(islabelnode, 0:1), # optional continue label
        (l,h)->is_goto(l, h, 1), # goto label, matching first label
        (l,h)->is_unless_label(l, h, 2) # goto and break
    )
    ast = _replace(ast, while_pattern) do loop_label, unless, whilebody, continue_label, goto, break_label
        condition = unless.args[1]
        whilebody = replace_continue_break(whilebody, continue_label, break_label)
        whilebody = remove_goto(whilebody)
        block = Expr(:block, whilebody...)
        Expr(:while, condition, block)
    end
    # if
    if_pattern = (isunless, anything, is_unless_label)
    ast = _replace(ast, if_pattern) do unless, body, label
        condition = unless.args[1]
        ifbody = Expr(:block, remove_goto(body)...)
        Expr(:if, condition, ifbody)
    end
end

function test(a, b)
    if a == 10
        x = if b == 22
            7
        else
            8
        end
        for i=1:100
            x += i
            x -= 77
            if i == 77
                continue
            elseif i == 99
                break
            end
        end
        return x
    else
        return 77
    end
end
ast = Base.uncompressed_ast(code_lowered(test, (Int, Int))[])
remove_goto(ast)
pattern = (anything, ('c')*(*), Greed(x-> x in (1, 7, 'x'), 5))

testlist = [1,2,3,4, 'c', 'c', 1, 1, 'x', 7, "9"]
@show _ismatch(testlist, pattern...)

for (i, elem) in enumerate(ast)

    println(i, "  ", elem)
end




# typed_ast = get_ast(typed_lam)
# f = test
# types = (Int, Int)
# lambda = get_lambda(code_lowered, f, types)

# ast = get_ast(lambda)


# function map_if_assignement(x::Expr)
#     if x.head == :(=)
#     end
# end
immutable BreadthFirstIter
    x::Any
end
children(s::Screen) = s.children
function Base.start(iter::BreadthFirstIter)
    (iter.x, 1)
end
function Base.next(iter::BreadthFirstIter, state)
    parent, next_parent, (childs, cstate, _done) = state
    if !done(childs, cstate)
        elem, cstate = next(childs, cstate)
        if isnull(next_parent) && !isempty(children(elem))
            next_parent = Nullable(elem)
        end
        return parent, next_parent, (elem, cstate, false)
    elseif !isnull(next_parent)
        np = get(next_parent)
        childs = children(np)
        return np, Nullable{typeof(np)}(), (childs, start(childs), false)
    else
        np, Nullable{typeof(np)}(), (childs, start(childs), true)
    end
end
