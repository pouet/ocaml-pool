module Card =
struct

        module Color =
        struct

            type t = Spade | Heart | Diamond | Club

            let all = [ Spade; Heart; Diamond; Club; ]

            let toString = function
                | Spade -> "S"
                | Heart -> "H"
                | Diamond -> "D"
                | Club -> "C"

            let toStringVerbose = function
                | Spade -> "Spade"
                | Heart -> "Heart"
                | Diamond -> "Diamond"
                | Club -> "Club"

        end

        module Value =
        struct

            type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

            let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As; ]

            let toInt = function
                | T2 -> 1
                | T3 -> 2
                | T4 -> 3
                | T5 -> 4
                | T6 -> 5
                | T7 -> 6
                | T8 -> 7
                | T9 -> 8
                | T10 -> 9
                | Jack -> 10
                | Queen -> 11
                | King -> 12
                | As -> 13

            let toString = function
                | T2 -> "2"
                | T3 -> "3"
                | T4 -> "4"
                | T5 -> "5"
                | T6 -> "6"
                | T7 -> "7"
                | T8 -> "8"
                | T9 -> "9"
                | T10 -> "10"
                | Jack -> "J"
                | Queen -> "Q"
                | King -> "K"
                | As -> "A"

            let toStringVerbose = function
                | T2 -> "2"
                | T3 -> "3"
                | T4 -> "4"
                | T5 -> "5"
                | T6 -> "6"
                | T7 -> "7"
                | T8 -> "8"
                | T9 -> "9"
                | T10 -> "10"
                | Jack -> "Jack"
                | Queen -> "Queen"
                | King -> "King"
                | As -> "As"

            let next = function
                | T2 -> T3
                | T3 -> T4
                | T4 -> T5
                | T5 -> T6
                | T6 -> T7
                | T7 -> T8
                | T8 -> T9
                | T9 -> T10
                | T10 -> Jack
                | Jack -> Queen
                | Queen -> King
                | King -> As
                | As -> invalid_arg "As"

            let previous = function
                | T2 -> invalid_arg "T2"
                | T3 -> T2
                | T4 -> T3
                | T5 -> T4
                | T6 -> T5
                | T7 -> T6
                | T8 -> T7
                | T9 -> T8
                | T10 -> T9
                | Jack -> T10
                | Queen -> Jack
                | King -> Queen
                | As -> King

        end

        type t = Value.t * Color.t

        let newCard (value : Value.t) (color : Color.t) =
            (value, color : t)

        let allSpades = List.map (fun c -> (c, Color.Spade : t)) Value.all
        let allHearts = List.map (fun c -> (c, Color.Heart : t)) Value.all
        let allDiamonds = List.map (fun c -> (c, Color.Diamond : t)) Value.all
        let allClubs = List.map (fun c -> (c, Color.Club : t)) Value.all
        let all = allSpades @ allHearts @ allDiamonds @ allClubs

        let getValue (a, _ : t) = a
        let getColor (_, b : t) = b

        let toString (a, b : t) = Value.toString a ^ Color.toString b
        let toStringVerbose (a, b : t) =
            Printf.sprintf "Card(%s,%s)"
                (Value.toStringVerbose a) (Color.toStringVerbose b)

        let compare (a, _ : t) (b, _ : t) =
            let tmpa = Value.toInt a
            and tmpb = Value.toInt b in
            if tmpa < tmpb then -1
            else if tmpa > tmpb then 1
            else 0

        let max (a : t) (b : t) =
            if compare a b < 0 then b
            else a

        let min (a : t) (b : t) =
            if compare a b > 0 then b
            else a

        let best = function
            | [] -> invalid_arg "Empty list"
            | hd :: tl -> List.fold_left max hd (hd :: tl)

        let isOf (_, c : t) (col : Color.t) = c = col
        let isSpade (c : t) = isOf c Color.Spade
        let isHeart (c : t) = isOf c Color.Heart
        let isDiamond (c : t) = isOf c Color.Diamond
        let isClub (c : t) = isOf c Color.Club
end

type t = Card.t list

let newDeck () = List.sort (fun _ _ -> (Random.int 3) - 1) Card.all

let rec toStringList = function
        | []              -> []
        | hd :: tl        -> Card.toString hd :: toStringList tl

let rec toStringListVerbose = function
        | []              -> []
        | hd :: tl        -> Card.toStringVerbose hd :: toStringListVerbose tl

let drawCard = function
        | []            -> raise (Failure "Empty Deck")
        | hd :: tl      -> (hd, tl)
