abstract MatchSteering
immutable NotGreedy <: MatchSteering
	x
end
Base.:(*)(x, ::typeof(*)) = NotGreedy(x)
isgreedy(x::NotGreedy) = false
isgreedy(x) = true

trymatch(ms::MatchSteering, val) = trymatch(ms.x, val)
trymatch(f::Function, val) = f(val)
trymatch(val1, val2) = val1 == val2

function ismatch{N}(x, atoms::Vararg{Any, N}; matchall=false)
	matches = NTuple{N, Vector{Any}}[]
	current_atom = 1
	states, tmp_matches = [], ntuple(x->Any[], Val{N})
	elem, state = next(x, start(x))
	matched_state = state
	last_state = state
	while true
		atom = atoms[current_atom]
		if !trymatch(atom, elem) # reset if not matching
			current_atom = 1
			atom = atoms[current_atom]
			tmp_matches = ntuple(x->Any[], Val{N})
		end
		while !trymatch(atom, elem) # continue until done or found match
			done(x, state) && return matches, states
			elem, state = next(x, state)
		end
		matched_state = ifelse(current_atom==1, state, matched_state)
		ismatched = false
		while trymatch(atom, elem)
			ismatched = true
			push!(tmp_matches[current_atom], elem)
			done(x, state) && break
			elem, state = next(x, state)
			next_atom = atoms[min(current_atom+1, length(atoms))]
			isgreedy(atom) || trymatch(next_atom, elem) && break
		end
		current_atom = ifelse(ismatched, current_atom+1, 1)
		if current_atom > length(atoms)
			push!(states, matched_state)
			push!(matches, tmp_matches)
			matchall || break
			tmp_matches = ntuple(x->Any[], Val{N})
			current_atom = 1
		end
	end
	matches, states
end

function forward(x, state, elem, n)
	for i=1:n
		done(x, state) && break
		elem, state = next(x, state)
	end
	elem, state
end

function _replace(f, x, atoms...; matchall=false)
	matches, states = ismatch(x, atoms..., matchall=matchall)
	if isempty(matches)
		return copy(x)
	end
	result = similar(x, 0)
	(elem, state), i = next(x, start(x)), 1
	while !done(x, state)
		isreplace = i <= length(states) && state == states[i]
		replacements, n = if isreplace
			i += 1
			n = sum(map(length, matches[i-1]))
			tmp = f(matches[i-1]...)
			r = isa(tmp, Tuple) ? tmp : (tmp,)
			r, n
		else
			(elem,), 1
		end
		elem, state = forward(x, state, elem, n)
		for r in replacements
			push!(result, r)
		end
	end
	result

end
[1,2]
[(1,2,3),4,5]


isunless(x::Expr) = x.head == :gotoifnot
isunless(x) = false
islabelnode(x::LabelNode) = true
islabelnode(x) = false
function remove_goto(ast)
	_replace(ast, isunless, (x->true)*(*), islabelnode) do unless, body, label
		condition = unless[].args[1]
		ifbody = Expr(:block, remove_goto(body)...)
		Expr(:if, condition, ifbody)
	end
end