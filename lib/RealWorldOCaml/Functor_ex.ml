module type Comparable = sig
    type t
    val compare : t -> t -> int
end;;

module Make_interval(Endpoint: Comparable) = struct
    type t = | Interval of (Endpoint.t * Endpoint.t)
             | Empty

    let create low high =
        if compare low high > 0 then Empty else Interval (low, high)

    let is_empty = function
        | Empty      -> true
        | Interval _ -> false

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) ->
            Endpoint.compare x low >= 0 && Endpoint.compare x high <= 0

    let intersect t1 t2 =
        let min x y = if x <= y then x else y in
        let max x y = if x <= y then y else x in
        match t1, t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (low1, high1), Interval (low2, high2) ->
            create (max low1 low2) (min high1 high2)
end;;

module type Interval_intf = sig
    type t
    type endpoint
    val create : endpoint -> endpoint -> t
    val is_empty : t -> bool
    val contains : t -> endpoint -> bool
    val intersect : t -> t -> t
end;;

module Make_interval_intf(Endpoint: Comparable): (Interval_intf with type endpoint = Endpoint.t) = struct
    type endpoint = Endpoint.t
    type t = | Interval of (Endpoint.t * Endpoint.t)
             | Empty

    let create low high =
        if compare low high > 0 then Empty else Interval (low, high)

    let is_empty = function
        | Empty      -> true
        | Interval _ -> false

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) ->
            Endpoint.compare x low >= 0 && Endpoint.compare x high <= 0

    let intersect t1 t2 =
        let min x y = if x <= y then x else y in
        let max x y = if x <= y then y else x in
        match t1, t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (low1, high1), Interval (low2, high2) ->
            create (max low1 low2) (min high1 high2)
end;;
