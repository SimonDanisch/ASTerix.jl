# type RichMethod
#     static_parameters
#     variables
#     arguments
#     macro_form
#     lowered_form
#     typed_form
# end

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

function get_method(f, types)
    if !all(isleaftype, types)
        error("Not all types are concrete: $types")
    end
    if !applicable(f, types)
        error("Method $f with signature $types not defined")
    end
    precompile(f, types) # make sure there is a specialization
    x = methods(f, types)
    if length(x) != 1
        error("
            More than one method found for signature $f $types.
            Please use more specific types!
        ")
    end
    first(x)
end

function macro_form(f, types)
    m = get_method(f, types)
    file = string(m.file)
    linestart = m.line
    @show file linestart
    code, str = open(file) do io
        line = ""
        for i=1:linestart-1
            line = readline(io)
        end
        @show line
        try # lines can be one off, which will result in a parse error
            parse(line)
        catch e
            line = readline(io)
        end
        @show line
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


# deal with all variances in base that should really be tuples but are something else
_tuple(x) = (x,)
_tuple(x::Core.SimpleVector) = tuple(x...)
_tuple(x::Tuple) = x
_tuple{T<:Tuple}(x::Type{T}) = tuple(x.parameters...)

function get_static_parameters(f, types)
    m = get_method(f, types)
    mi = m.specializations.func
    spnames = map(x->x.name, _tuple(m.tvars))
    sptypes = _tuple(mi.sparam_vals)
    spnames, sptypes
end

function get_ast(li::LambdaInfo)
    ast = li.code
    if isa(ast, Vector{UInt8})
        return Base.uncompressed_ast(li)
    end
    ast
end

function filter_expr(keep, ast)
    replace_or_drop(x->(false, x), x->!keep(x), identity, Any[ast])[1]
end
function replace_expr(f, ast)
    replace_or_drop(f, x->false, Any[ast])[1]#ever drop
end
function replace_or_drop(f, drop, ast::Vector, result=[])
    for elem in ast
        replace_or_drop(f, drop, elem, result)
    end
    result
end

function replace_or_drop(f, drop, ast, result=[])
    drop(ast) && return result
    replace, replacement = f(ast)
    if replace
        push!(result, replacement)
    else
        expr = if isa(ast, Expr)
            nexpr = Expr(ast.head)
            @show nexpr
            replace_or_drop(f, drop, ast.args, nexpr.args)
            nexpr
        else
            ast
        end
        push!(result, expr)
    end
    result
end

function remove_static_params(ast, static_params)
    sp_names, sp_types = static_params
    replace_expr(ast) do expr
        if isa(expr, Symbol)
            idx = findfirst(sp_names, expr)
            if idx != 0
                return true, sp_types[idx]
            end
        end
        false, expr
    end
end

_typeof{T}(x::Type{T}) = Type{T}
_typeof{T}(x::T) = T
function extract_type(x::Symbol, slots)
    m = current_module()
    if isdefined(m, x)
        _typeof(getfield(m, x))
    else
        Symbol
    end
end
extract_type{T}(x::T, slots) = T
function extract_type(x::Expr, slots)
    if x.head == :(::)
        x.args[2]
    else
        extract_type(add_typing(x, slots), slots)
    end
end

function get_func(x::Expr)
    x.head == :curly && return get_func(x.args[1])
end
function get_func(x::Symbol)
    getfield(current_module(), x)
end
function get_func(x::GlobalRef)
    getfield(x.mod, x.name)
end
function extract_func(x::Expr, slots)
    @assert x.head == :call
    f = get_func(x.args[1])
    args = x.args[2:end]
    typed = add_typing(args, slots)
    _typed = !isa(typed, Vector) ? Any[typed] :typed
    f, _typed
end

function return_type(f, types)
    x = Base.return_types(f, types)
    if length(x) == 1
        x[1]
    else
        Union{x...}
    end
end

function add_typing(ast, slot_dict)
    replace_expr(ast) do x
        if isa(x, Expr) && x.head == :-> # cant type dem lambdas
            return true, x
        end
        if isa(x, Expr) && x.head == :(::) # already typed
            return true, x
        end
        if haskey(slot_dict, x)
            return true, Expr(:(::), x, slot_dict[x])
        end
        if is_call(x)
            f, typed_args = extract_func(x, slot_dict)
            types = tuple(map(x->extract_type(x, slot_dict), typed_args)...)
            T = return_type(f, types)
            x.args[2:end] = typed_args # type args
            return true, Expr(:(::), x, T)
        end
        return false, x
    end
end

is_linenumber(x) = false
is_linenumber(x::LineNumberNode) = true
function is_linenumber(x::Expr)
    x.head == :line
end

is_call(x) = false
function is_call(x::Expr)
    x.head == :call
end

using Plots; glvisualize()
using GLVisualize
plot(rand(100))
w = GLVisualize.current_screen()


function clean_form(f, types)
    ast, str = macro_form(f, types)
    static_params = get_static_parameters(f, types)
    ast = filter_expr(x->!is_linenumber(x), ast)
    remove_static_params(ast, static_params)
end
function clean_typed(f, types)
    ast = clean_form(f, types)
    slots = slot_mapping(get_lambda(code_typed, f, types))
    slot_dict = Dict()
    for (k, (name, T)) in slots
        if !isa(k, SSAValue)
            slot_dict[k] = T
        end
        slot_dict[Symbol(name)] = T
    end
    add_typing(ast, slot_dict)
end
