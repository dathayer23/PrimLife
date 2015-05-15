namespace BamaLlama.DataStructures
 
module QuadTree =
    //[<AbstractClass>]
    type ICoordinate = 
       abstract member x : float
       abstract member y : float

    type coord = XCoord | YCoord
    with 
       member x.Next = match x with XCoord -> YCoord | YCoord -> XCoord
       override x.ToString() = match x with XCoord -> "XCoord" | YCoord -> "YCoord"

    type range = Range of (float * float * coord)
    with 
       member x.Coord = match x with Range(_,_,cd) -> cd
       member x.MidPoint = match x with Range(mn,mx,_) -> (mn + mx) / 2.0
       member x.Update(f:float) = 
          match x with 
          | Range(mn,mx,cd) -> Range(min mn f, max mx f, cd)

       override x.ToString() = match x with Range(mn,mx,cd) -> sprintf "(%f - %f) on %s" mn mx (cd.ToString())
    type quadTree<'t when 't :> ICoordinate and 't : equality> = 
        Empty of coord
      | Node of 't * coord 
      | QuadTree of range * 't * quadTree<'t> * quadTree<'t>    
    with 
//       member this.x = 
//          match this with 
//          | Empty -> -1.0
//          | Node(pt,coord) -> pt.x
//          | QuadTree(range, t, left, right) -> 
//             match range.Coord with 
//             | coord.XCoord -> t
//             | coord.YCoord -> range.MidPoint
//
//       member this.y = 
//          match this with 
//          | Empty -> -1.0
//          | Node(pt,coord) -> pt.y
//          | QuadTree(range, t, left, right) -> 
//             match range.Coord with 
//             | coord.XCoord -> range.MidPoint
//             | coord.YCoord -> t

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

         
       static member FromRange(lst : 't list, cd:coord) = 
          match lst with
          | [] -> Empty(coord.XCoord)
          | [t] -> Node(t, coord.XCoord)
          | xs -> 
             let left, mid, right = List.medianSplit xs
             QuadTree(Range(List.min xs, List.max xs, cd), mid, FromRange(left, cd.Next), FromRange(right. cd.Next))
          
       member x.Print() = 
          let rec _print(depth, node, prefix) = 
             let space = new System.String(' ', depth * 3) + prefix
             match node with 
             | Empty -> sprintf "%sEmpty Tree" space
             | Node (t,coord) -> sprintf "%sNode(%s, %s)" space (t.ToString()) (coord.ToString())
             | QuadTree(range,t,left,right) -> 
                  sprintf "%sQuad(%s,%s,\n%s\n%s)" space (range.ToString()) (t.ToString()) (_print(depth + 1, left, "L")) (_print(depth + 1, right, "R"))
          _print(0,x, "")      



   