%lang starknet
%builtins pedersen

# This version works at ~17000ms. Note that we have omitted a factor from the
# dividend, namely the `amm_to_balance`. It is also notable that adding a
# `@post False` to `do_swap()` gives Unknown. Shouldn't this cause Sat pretty
# quickly?

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

# @declare $div: felt
# @pre $div == (pool_balance(token_from) + amount_from)
#
# @pre (token_to == 1 || token_to == 2)
# @pre (token_from == 1 || token_from == 2)
# @pre token_from != token_to
#
# @pre 0 < amount_from
# @pre $div <= 10633823966279326983230456482242756608
# @pre amount_from < 2 * $div
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
# @pre value < 2 * div
func unsigned_div_rem{range_check_ptr}(value, div) -> (q, r):
    return (1, 2)
end
