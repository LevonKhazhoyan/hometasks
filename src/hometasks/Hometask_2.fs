namespace hometasks

module Hometask_2 =
   let Subtask_1 x =
       x * x * x * x + x * x * x + x * x + x + 1.0

   let Subtask_2 x =

       let x_squared = x * x
       let x_plus_x_squared = x_squared + x
       (x_squared + 1.0) * x_plus_x_squared + 1.0

   let create_array amount_of_elements =
   
       let genRandomNumbers count =

           let rnd = System.Random()
           Array.init count (fun _ -> ((rnd.Next ())))

       let random_array = genRandomNumbers amount_of_elements

       random_array

   let Subtask_3 (xs: int array) maximum =
       let mutable j = 0    
       for i = 0 to xs.Length - 1 do
           if xs.[i] <= maximum
           then
               j <- j + 1

       let indices = Array.zeroCreate j
       j <- 0

       for i = 0 to xs.Length - 1 do
           if xs.[i] <= maximum
           then
               indices.[j] <- i
               j <- j + 1

       indices    
       
   let Subtask_4 (xs: int array) left_limit right_limit =
       let mutable j = 0

       for i = 0 to xs.Length - 1 do
          if xs.[i] < left_limit || xs.[i] > right_limit
          then
               j <- j + 1

       let indices = Array.zeroCreate j
       j <- 0
       
       for i = 0 to xs.Length - 1 do

           if xs.[i] < left_limit || xs.[i] > right_limit
           then
               indices.[j] <- i
               j <- j + 1

       indices

   let Subtask_5 (xs: int array) =
       if xs.Length <> 2
       then [||]
       else 

           xs.[0] <- xs.[0] ^^^ xs.[1]
           xs.[1] <- xs.[0] ^^^ xs.[1]
           xs.[0] <- xs.[0] ^^^ xs.[1]
           xs

   let Subtask_6 (xs: int array) i j =
        if i < 0 || j < 0 || i >= xs.Length || j >= xs.Length
        then
            [||]
        else if i = j
        then
            xs
        else
            xs.[i] <- xs.[i] ^^^ xs.[j]
            xs.[j] <- xs.[i] ^^^ xs.[j]
            xs.[i] <- xs.[i] ^^^ xs.[j]
            xs
