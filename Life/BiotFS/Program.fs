﻿namespace BamaLlama.DataStructures

open System
open QuadTree

module Program = 
   
   type coordinate = Coord of float * float
   with 
      member this.x = (this :> ICoordinate).x
      member this.y = (this :> ICoordinate).y
      interface  ICoordinate with 
         member this.x = match this with Coord(x, _) -> x
         member this.y = match this with Coord(_, y) -> y
      override x.ToString() = sprintf "(%f, %f)" x.x x.y

   [<EntryPoint>]
   let main argv = 
       let testList1 = [Coord(1.0,1.0); Coord(2.0,2.0)]
       let testList2 = [Coord(1.0,1.0)]
       let res1 = splitListAtMedian testList1
       let res2 = splitListAtMedian testList2
       let res3 = splitListAtMedian []
       let mutable testTree:quadTree<coordinate> = Empty(coord.XCoord)
       do Console.WriteLine(testTree.Print(): string)
       do testTree <- testTree.Insert(Coord(35.0,90.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(70.0,80.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(10.0,75.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(80.0,40.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(50.0,90.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(70.0,30.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(90.0,60.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(50.0,25.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(25.0,10.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(20.0,50.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do testTree <- testTree.Insert(Coord(60.0,10.0), coord.XCoord)
       do Console.WriteLine(testTree.Print())
       do Console.WriteLine(sprintf "Tree generated by individual insertion has height %d\n" (testTree.Height()))
       let testList4 = 
          [
            Coord(35.0,90.0);
            Coord(70.0,80.0);
            Coord(10.0,75.0);
            Coord(80.0,40.0);
            Coord(50.0,90.0);
            Coord(70.0,30.0);
            Coord(90.0,60.0);
            Coord(50.0,25.0);
            Coord(25.0,10.0);
            Coord(20.0,50.0);
            Coord(60.0,10.0);
          ]
          
       do testTree <- quadTree.Empty(coord.XCoord)
       do testTree <- quadTree<coordinate>.FromRange(testList4, coord.XCoord)
       do Console.WriteLine(testTree.Print())
       let h = testTree.Height()
       do Console.WriteLine(sprintf "Tree generated by bulk insertion has height %d" h)
       
       0 // return an integer exit code
