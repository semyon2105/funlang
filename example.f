function main() : int
{
    let v : float = 2 + 2.5 * 6;
    v = v - 1;
    let result : float = -(other(10, v));
    print(result)
}

function other(a:int, b:float) : float
{
    let n1 : float = if (a > b) a;
    let n2 : int = while (b >= a) { a = a + 1 };
    let n3 : void = null;
    let n4 : bool = !(true);

    let abracadabra : float =
        if (a > b < a * b)
            while (a + b > 10)
                let abc : float = a + b *
                    (while (true) 1234.5) /
                    a - b / a
}
