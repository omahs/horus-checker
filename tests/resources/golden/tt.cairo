// @post [ap - 1] == 42
func main() {
    call id;
    [ap] = 5, ap++;
    [ap] = 6, ap++;
    ret;
}

// @pre x > 13
// @post $Return.res == x
func id(x) -> (res: felt) {
    [ap] = [fp - 3], ap++;
    [ap] = [ap - 1], ap++;
    ret;
}
