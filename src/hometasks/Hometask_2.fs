namespace hometasks

module Hometask2 =
   let subtask1 x =
       x * x * x * x + x * x * x + x * x + x + 1.0

   let subtask2 x =

       let xSquared = x * x
       let xPlusXSquared = xSquared + x
       (xSquared + 1.0) * xPlusXSquared + 1.0

   let createArray amountOfElements =
   
       let genRandomNumbers count =

           let rnd = System.Random()
           Array.init count (fun _ -> rnd.Next ())

       let randomArray = genRandomNumbers amountOfElements
       printfn "%A" randomArray
       randomArray

   let subtask3 (xs: array<int>) maximum =

       let mutable j = 0    
       for i = 0 to xs.Length - 1 do
           if xs.[i] <= maximum
           then j <- j + 1

       let indices = Array.zeroCreate j
       j <- 0

       for i = 0 to xs.Length - 1 do
           if xs.[i] <= maximum
           then
               indices.[j] <- i
               j <- j + 1

       indices    
       
   let subtask4 (xs: array<int>) leftLimit rightLimit =

       let mutable j = 0
       for i = 0 to xs.Length - 1 do
          if xs.[i] < leftLimit || xs.[i] > rightLimit
          then j <- j + 1

       let indices = Array.zeroCreate j
       j <- 0
       
       for i = 0 to xs.Length - 1 do
           if xs.[i] < leftLimit || xs.[i] > rightLimit
           then
               indices.[j] <- i
               j <- j + 1
       indices

   let subtask5 (xs: array<int>) =

       if xs.Length <> 2
       then failwith "expected array of size 2"
       else
           xs.[0] <- xs.[0] ^^^ xs.[1]
           xs.[1] <- xs.[0] ^^^ xs.[1]
           xs.[0] <- xs.[0] ^^^ xs.[1]
       xs

   let subtask6 (xs: array<int>) i j =

        if i < 0 || j < 0 || i >= xs.Length || j >= xs.Length
        then failwith "indices are out of range"    
        elif i = j
        then xs
        else
            xs.[i] <- xs.[i] ^^^ xs.[j]
            xs.[j] <- xs.[i] ^^^ xs.[j]
            xs.[i] <- xs.[i] ^^^ xs.[j]
            xs
