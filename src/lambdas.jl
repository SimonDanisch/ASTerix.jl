immutable FuncExpr
    name::Symbol
    slots
    args
    body::Vector
    returntype::Type
end

function remove_static_param!(expr, static_parameters)
    expr # leave any other untouched
end
function remove_static_param!(expr::Vector, static_parameters)
    map!(e->remove_static_param!(e, static_parameters), expr)
end
function remove_static_param!(expr::Expr, static_parameters)
    if expr.head == :static_parameter
        idx = expr.args[1]
        return static_parameters[idx]
    else
        remove_static_param!(expr.args, static_parameters)
        expr
    end
end

function FuncExpr(f, types)
    if isa(f, Core.IntrinsicFunction)
        error("$f is an intrinsic function which can't be transpiled")
    end
    lam_typed, ret_type = get_lambda(code_typed, f, types)
    slots = slot_mapping(lam_typed)
    ast = lam_typed.code
    name = Symbol(f)
    FuncExpr(name, slots, view(slots, 2:(length(slots))), ast, ret_type)
end
function slot_mapping(lam_typed)
    if isa(lam_typed.slotnames, Void) || isa(lam_typed.slottypes, Void)
        return [(SlotNumber(-1), ("", Void))]
    end
    slotnames = copy(lam_typed.slotnames)
    slottypes = copy(lam_typed.slottypes)

    slots = [(SlotNumber(i), (string(name), slottypes[i])) for (i, name) in enumerate(slotnames)]
    ssaslot = [(SSAValue(i-1), ("ssa_$(i-1)", t)) for (i,t) in enumerate(lam_typed.ssavaluetypes)]
    vcat(slots, ssaslot)
end


function get_lambda(pass, f, types)
    lambda = pass(f, types)
    if isa(lambda, Vector)
        if isempty(lambda)
            args = join(map(t->"::$t", types), ", ")
            error("$f($args) couldn't be found")
        end
        return first(lambda)
    else
        isa(lambda, LambdaInfo) && return lambda
        error("Not sure what's up with returntype of $pass. Returned: $lambda")
    end
end

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
test2{T, T2}(a::T, b::T2) = a + b


function get_ast(li::CodeInfo)
    ast = li.code
    if isa(ast, Vector{UInt8})
        return Base.uncompressed_ast(li)
    end
    ast
end
function get_method(f, types)
    first(methods(f, types))
end


function macro_form(f, types)
    m = get_method(f, types)
    file = string(m.file)
    linestart = m.line
    code, str = open(file) do io
        line = ""
        for i=1:linestart-1
            line = readline(io)
        end
        # if !contains(line, string(m.name))
        #     line = readline(io)
        # end
        try # lines can be one off, which will result in a parse error
            parse(line)
        catch e
            line = readline(io)
        end
        while !eof(io)
            line = line*readline(io)
            e = Base.parse_input_line(line; filename=file)
            if !(isa(e,Expr) && e.head === :incomplete)
                return e, line
            end
        end
    end
    code, str
end
body = code.args[2].args

showfn(x) = println(fieldnames(x))
type RichMethod
    static_parameters
    variables
    arguments
    macro_form
    lowered_form
    typed_form
end
function lolz{T1, T2}(a::T1, b::T2)
    a+b
end

# deal with all variances in base that should really be tuples but are something else
_tuple(x) = (x,)
_tuple(x::Core.SimpleVector) = tuple(x...)
_tuple(x::Tuple) = x
_tuple{T<:Tuple}(x::Type{T}) = tuple(x.parameters...)

function get_static_parameters(f, types)
    precompile(f, types) # make sure there is a specialization
    m = get_method(f, types)
    mi = m.specializations.func
    spnames = map(x->x.name, _tuple(m.tvars))
    sptypes = _tuple(mi.sparam_vals)
    map(tuple, zip(spnames, sptypes))
end
get_static_parameters(test2, (Int, Int))
for k in fieldnames(m)
    if isdefined(m, k)
        println(k, " ", getfield(m, k))
    end
end
typed_lam = get_lambda(code_lowered, f, types)
typed_lam, ret_typ = get_lambda(code_typed, f, types)
typed_ast = get_ast(typed_lam)
f()
f = test
types = (Int, Int)
lambda = get_lambda(code_lowered, f, types)

ast = get_ast(lambda)


function map_if_assignement(x::Expr)
    if x.head == :(=)

end
