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

f = test
types = (Int, Int)
typed_lam = get_lambda(code_typed, f, types)
ast = get_ast(typed_lam)

ast, str = macro_form(f, types)
params = get_static_parameters(f, types)



# typed_ast = get_ast(typed_lam)
# f = test
# types = (Int, Int)
# lambda = get_lambda(code_lowered, f, types)

# ast = get_ast(lambda)


# function map_if_assignement(x::Expr)
#     if x.head == :(=)
#     end
# end
