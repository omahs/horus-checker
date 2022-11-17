%lang starknet
%builtins pedersen

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2

# A map from token type to the corresponding balance of the pool.
@storage_var
func pool_balance(token_type : felt) -> (balance : felt):
end

# @pre token_type == 1 || token_type == 2
# @post $Return.balance == pool_balance(token_type)
func get_pool_token_balance{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    token_type : felt
) -> (balance : felt):
    return pool_balance.read(token_type)
end

# This should successfully output unsat
# @pre (token_to == 1 || token_to == 2)
# @pre (token_from == 1 || token_from == 2)
# @pre token_from != token_to
# @pre 0 < amount_from
# @pre pool_balance(token_from) + amount_from <= 10633823966279326983230456482242756608
func do_swap{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    account_id : felt, token_from : felt, token_to : felt, amount_from : felt
) -> (amount_to : felt):
    alloc_locals

    # Get pool balance.
    let (local amm_from_balance) = get_pool_token_balance(token_type=token_from)

    # Calculate swap amount.
    let (local amount_to, _) = unsigned_div_rem(
        amount_from, amm_from_balance + amount_from
    )
    return (amount_to=amount_to)
end

# @pre div <= 10633823966279326983230456482242756608
func unsigned_div_rem{range_check_ptr}(value, div) -> (q, r):
    return (1, 2)
end