function main() : int
{
    let v : float = 2 + 2.5 * 6;
    v = v - 1;
    let result : float = other(10, v);
    print(result)
}

function other(a:int, b:float) : float
{
    let n1 : float = if (a > b) a;
    let n2 : int = while (b >= a) { a = a + 1 };
    let n3 : void = if (a != b) { };
    let n4 : bool = true;
    let n5 : bool = false;
    let xD : float = 1.2345
}
