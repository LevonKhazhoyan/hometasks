namespace hometasks

module Hometask_2 =
   let Subtask_1 x =
       x*x*x*x + x*x*x + x*x + x + 1.0

   let Subtask_2 x =

       let x_squared = x * x
       let x_plus_x_squared = x_squared + x
       (x_squared + 1.0) * x_plus_x_squared

   let create_array amount_of_elements =
   
       let genRandomNumbers count =

           let rnd = System.Random()
           Array.init count (fun _ -> ((rnd.Next ())%10000))

       let random_array = genRandomNumbers amount_of_elements

       printf "Сгенерированные элементы массива: "
       printfn "%A" random_array
       random_array


   let Subtask_3 (random_array: int array) amount_of_elements maximum =

       let mutable j = 0

       for i = 0 to amount_of_elements-1 do
           if random_array.[i] <= maximum then
               j <- j+1

       let Subtask_3_array = Array.zeroCreate j
       j <- 0

       for i = 0 to amount_of_elements-1 do
           if random_array.[i] <= maximum then
               Subtask_3_array.[j] <- i
               j <- j+1

       printf "Индексы элементов массива, не больших, чем заданное число: "
       Subtask_3_array    
       
   let Subtask_4 (random_array: int array) left_limit right_limit amount_of_elements =
       let mutable j = 0
       for i = 0 to amount_of_elements-1 do
          if (random_array.[i] < left_limit) || (random_array.[i] > right_limit) then
               j <- j+1
       let Subtask_4_array = Array.zeroCreate j
       j <- 0 
       for i = 0 to amount_of_elements-1 do
           if (random_array.[i] < left_limit) || (random_array.[i] > right_limit) then
               Subtask_4_array.[j] <- i
               j <- j+1
       Subtask_4_array

   let Subtask_5 (random_array: int array) =
       random_array.[0] <- random_array.[0] + random_array.[1]
       random_array.[1] <- random_array.[0] - random_array.[1]
       random_array.[0] <- random_array.[0] - random_array.[1]
       random_array

   let Subtask_6 (random_array: int array) i j =
       random_array.[i] <- random_array.[i] + random_array.[j]
       random_array.[j] <- random_array.[i] - random_array.[j]
       random_array.[i] <- random_array.[i] - random_array.[j]
       random_array
