immutable FuncExpr
    name::Symbol
    slots
    args
    body::Vector
    returntype::Type
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

function get_ast(li::CodeInfo)
    ast = li.code
    if isa(ast, Vector{UInt8})
        return Base.uncompressed_ast(li)
    end
    ast
end