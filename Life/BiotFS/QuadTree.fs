namespace BamaLlama.DataStructures
 
module QuadTree =
    
    type ICoordinate = 
       abstract member x : float
       abstract member y : float


    type coord = XCoord | YCoord
    with 
       member x.Next = match x with XCoord -> YCoord | YCoord -> XCoord
       override x.ToString() = match x with XCoord -> "XCoord" | YCoord -> "YCoord"

    

    let splitListAtMedian lst = 
       let n = List.length lst
       let mid = n / 2
       let first, last = List.splitAt mid lst
       match last with 
       | [] -> first, None, last
       | x:: xs -> first, Some x, xs

    
    


    type range = Range of (float * float * coord)
    with 
       member x.Coord = match x with Range(_,_,cd) -> cd
       member x.MidPoint = match x with Range(mn,mx,_) -> (mn + mx) / 2.0
       member x.Update(f:float) = 
          match x with 
          | Range(mn,mx,cd) -> Range(min mn f, max mx f, cd)

       override x.ToString() = match x with Range(mn,mx,cd) -> sprintf "(%f - %f) on %s" mn mx (cd.ToString())

    type quadTree<'t when 't :> ICoordinate and 't : equality and 't : comparison> = 
        Empty of coord
      | Node of 't * coord 
      | QuadTree of range * 't * quadTree<'t> * quadTree<'t>    
    with 
       
       member x.Height() = 
          match x with 
          | Empty _ -> 0
          | Node (_,_) -> 1
          | QuadTree(_,_,left,right) -> max (left.Height()) (right.Height()) + 1

       //split list sorted by specified coord at median
       static member medianSplit (cd:coord) (items:'t list) = 
          let sorted = 
             match cd with 
             | XCoord -> List.sortBy (fun (it:'t) -> it.x) items
             | YCoord -> List.sortBy (fun (it:'t) -> it.y) items

          splitListAtMedian sorted

       member x.SplitNode(pt:'t, t:'t, cd:coord) =
          if t = pt 
          then failwith "duplicate coordinates" 
          else 
            match cd with 
            | coord.XCoord ->  // split on x coordinate
                if (t.x < pt.x)
                then QuadTree(Range(t.x, pt.x, cd), pt, Node(t, cd.Next), Node(pt, cd.Next))
                else QuadTree(Range(pt.x, t.x, cd), pt, Node(pt, cd.Next), Node(t, cd.Next))
            | coord.YCoord -> // split on y coordinate
                if (t.y < pt.y)
                then QuadTree(Range(t.y, pt.y, cd), pt,  Node(t, coord.XCoord), Node(pt, coord.XCoord))
                else QuadTree(Range(pt.y, t.y, cd), pt, Node(pt, coord.XCoord), Node(t, coord.XCoord))     
       
       member x.Left = match x with QuadTree(_, _, left, _) -> left | _ -> failwith "Only Quadtree node type has a left element"
       member x.Right = match x with QuadTree(_,_,_, right) -> right | _ -> failwith "Only Quadtree node type has a rightt element"   
       member x.Insert (pt:'t, cd:coord) =
          match x with 
          | Empty cd ->  Node(pt, cd)
          | Node (t, coord) ->  x.SplitNode(pt, t, coord)
          | QuadTree(range, t, left, right) -> 
             match range.Coord with 
             | coord.XCoord -> 
                if t.x < pt.x
                then QuadTree(range.Update(pt.x), t, left.Insert(pt, coord.YCoord), right)
                else QuadTree(range.Update(pt.x), t, left, right.Insert(pt, coord.YCoord))
             | coord.YCoord ->
                if t.y < pt.y
                then QuadTree(range.Update(pt.y), t, left.Insert(pt, coord.YCoord), right)
                else QuadTree(range.Update(pt.y), t, left, right.Insert(pt, coord.YCoord))

       member x.FindMin(axis:coord, cd:coord) : 't option =
          match x with 
          | Empty _ -> None
          | Node (t, _) -> Some t
          | QuadTree(Range(_,_,cdthis),t,left,right) ->
             if cdthis = cd
             then 
                match left with 
                | Empty(_) -> Some t
                | Node(t,_) -> Some t
                | _ -> left.FindMin(axis, cd.Next)
             else
                let leftMin = left.FindMin(axis, cd.Next)
                let rightMin = right.FindMin(axis, cd.Next)
                match (leftMin, rightMin) with
                | None, None -> None
                | Some t, None -> Some t
                | None, Some t -> Some t
                | Some t1, Some t2 -> Some (min t1 t2)
                

//       member x.Remove(pt:'t, cd:coord) = 
//          match x with 
//          | Empty(_) -> x
//          | Node(thispt, cd) -> if pt == thispt then Empty(cd) else failwith (sprintf "pt %s not found in quad tree" (pt.ToString()))
//          | QuadTree(Range(_,_, thiscd) as r, thispt, left , right) -> 
//             if (getCoord pt thiscd) < (getCoord thispt thiscd) then left.Remove(pt, cd.Next)
//             else if (getCoord pt thiscd) > (getCoord thispt thiscd) then right.Remove(pt.cd.Next)
//             else
//                match (left,right) with 
//                | Empty cd1, Empty _ -> Empty(cd1)
//                | Empty _, _ -> QuadTree)
         
       static member FromRange(lst : 't list, cd:coord) = 
          // get coordinate referenced by coord variable
          let getCoord (cd:coord) (pt:'t) = match cd with  | XCoord -> pt.x | YCoord -> pt.y

          match lst with
          | [] -> Empty(coord.XCoord)
          | [t] -> Node(t, coord.XCoord)
          | xs -> 
             let left, mid, right = quadTree<'t>.medianSplit cd xs
             match mid with 
             | None ->                 
                   quadTree<'t>.FromRange(left, cd.Next)
             | Some t -> 
                QuadTree(Range(getCoord cd (List.minBy (getCoord cd) xs), getCoord cd (List.maxBy (getCoord cd) xs), cd), 
                   t, 
                   quadTree<'t>.FromRange(left, cd.Next), 
                   quadTree<'t>.FromRange(right, cd.Next))
          
       member x.Print() = 
          let rec _print(depth, node, prefix) = 
             let space = new System.String(' ', depth * 3) + prefix
             match node with 
             | Empty cd -> sprintf "%sEmpty Tree(%s)" space (cd.ToString())
             | Node (t,coord) -> sprintf "%sNode(%s, %s)" space (t.ToString()) (coord.ToString())
             | QuadTree(range,t,left,right) -> 
                  sprintf "%sQuad(%s,%s,\n%s\n%s)" space (range.ToString()) (t.ToString()) (_print(depth + 1, left, "L")) (_print(depth + 1, right, "R"))
          _print(0,x, "")      



   