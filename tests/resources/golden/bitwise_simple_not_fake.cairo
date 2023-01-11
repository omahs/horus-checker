%builtins bitwise

from starkware.cairo.common.cairo_builtins import BitwiseBuiltin

// @pre a == 24601
func apply_bitwise{bitwise_ptr: BitwiseBuiltin*}(a, b) -> (c: felt, d: felt, e: felt) {
    bitwise_ptr.x = a;
    bitwise_ptr.y = b;
    let bitwise_ptr = bitwise_ptr + BitwiseBuiltin.SIZE;
    return (bitwise_ptr.x_and_y, bitwise_ptr.x_xor_y, bitwise_ptr.x_or_y);
}

func main{bitwise_ptr: BitwiseBuiltin*}(b) -> (c: felt, d: felt, e: felt) {
    let fake_ptr = cast(42, BitwiseBuiltin*);
    let (c, d, e) = apply_bitwise{bitwise_ptr=fake_ptr}(24601, b);
    return (c, d, e);
}
