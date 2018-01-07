//// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
//// for more guidance on F# programming.

#r "../packages/Logary.4.2.1/lib/net452/Logary.dll"
#r "../packages/FSharp.Collections.ParallelSeq.1.0.2/lib/net40/FSharp.Collections.ParallelSeq.dll"

#load "SpiderSolitare.fs"
#load "Operations.fs"
#load "SpiderSolver.fs"
open SpiderSolitare.Solver.Bellman
open SpiderSolitare.Operations.App





//printfn "Hello, World"

//(*
//RecursiveTypesAndFold-2.fsx

//Related blog post: http://fsharpforfunandprofit.com/posts/recursive-types-and-folds-2/
//*)

//// ==============================================
//// PART 2 - Introducing Folds
//// ==============================================



//// =======================
//// Review of Gift domain
//// =======================

//type Book = {title: string; price: decimal}

//type ChocolateType = Dark | Milk | SeventyPercent
//type Chocolate = {chocType: ChocolateType ; price: decimal}

//type WrappingPaperStyle = 
//    | HappyBirthday
//    | HappyHolidays
//    | SolidColor

//type Gift =
//    | Book of Book
//    | Chocolate of Chocolate 
//    | Wrapped of Gift * WrappingPaperStyle
//    | Boxed of Gift 
//    | WithACard of Gift * message:string


//let rec cataGift fBook fChocolate fWrapped fBox fCard gift :'r =
//    let recurse = cataGift fBook fChocolate fWrapped fBox fCard
//    match gift with 
//    | Book book -> 
//        fBook book
//    | Chocolate choc -> 
//        fChocolate choc
//    | Wrapped (gift,style) -> 
//        fWrapped (recurse gift,style)
//    | Boxed gift -> 
//        fBox (recurse gift)
//    | WithACard (gift,message) -> 
//        fCard (recurse gift,message) 


//// ---------------------------------
//// Sample data
//// ---------------------------------


//// A Book
//let wolfHall = {title="Wolf Hall"; price=20m}
//// A Chocolate
//let yummyChoc = {chocType=SeventyPercent; price=5m}
//// A Gift
//let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")
//// A Gift
//let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)


//// =======================
//// Stack overflow!
//// =======================

//// helper to create deeply nested boxes
//let deeplyNestedBox depth =
//    let rec loop depth boxSoFar =
//        match depth with
//        | 0 -> boxSoFar 
//        | n -> loop (n-1) (Boxed boxSoFar)
//    loop depth (Book wolfHall)

//deeplyNestedBox 5
//// Boxed (Boxed (Boxed (Boxed (Boxed (Book {title = "Wolf Hall"; price = 20M})))))

//deeplyNestedBox 10
////  Boxed(Boxed(Boxed(Boxed(Boxed
////   (Boxed(Boxed(Boxed(Boxed(Boxed(Book {title = "Wolf Hall";price = 20M}))))))))))

//// ---------------------------------
//// Stress test "totalCostUsingCata"
//// ---------------------------------

//let totalCostUsingCata gift =
//    let fBook (book:Book) = 
//        book.price
//    let fChocolate (choc:Chocolate) = 
//        choc.price
//    let fWrapped  (innerCost,style) = 
//        innerCost + 0.5m
//    let fBox innerCost = 
//        innerCost + 1.0m
//    let fCard (innerCost,message) = 
//        innerCost + 2.0m
//    // call the catamorphism
//    cataGift fBook fChocolate fWrapped fBox fCard gift

//deeplyNestedBox 10 |> totalCostUsingCata       // OK     30.0M
//deeplyNestedBox 100 |> totalCostUsingCata      // OK    120.0M
//deeplyNestedBox 1000 |> totalCostUsingCata     // OK   1020.0M
////deeplyNestedBox 10000 |> totalCostUsingCata  // OK  10020.0M
////deeplyNestedBox 100000 |> totalCostUsingCata // Stack overflow
//// somewhere between 10K and 100K things go wrong. Why?


//(*
//fBox is `innerCost + 1.0m`
//so what we have is

//innerCost + 1.0m where innerCost = 
//  innerCost2 + 1.0m where innerCost2 = 
//    innerCost3 + 1.0m where innerCost3 = 
//      innerCost4 + 1.0m where innerCost4 = 
//        ...
//        innerCost999 + 1.0m where innerCost999 = 
//          innerCost1000 + 1.0m where innerCost1000 = 
//            book.price

//innerCost1000 has to be calculated before innerCost999 can be calculated.
//and 999 other inner costs have to be calculated before the top level `innerCost` can be calculated
//*)

//// =======================
//// Introducing an accumulator
//// =======================

//// but how can I get the total without knowing the subtotal
//// Answer use an accumulator -- push the number down into the next step

//(*

//costSoFar = 1.0m; Call calcInnerCost with costSoFar: 
//  costSoFar = costSoFar + 1.0m; Call calcInnerCost with costSoFar: 
//    costSoFar = costSoFar + 1.0m; Call calcInnerCost with costSoFar: 
//      costSoFar = costSoFar + 1.0m; Call calcInnerCost with costSoFar: 
//        ...
//        costSoFar = costSoFar + 1.0m; Call calcInnerCost with costSoFar: 
//          costSoFar = costSoFar + 1.0m; Call calcInnerCost with costSoFar: 
//            finalCost = costSoFar + book.price  // final result

//*)


//// ---------------------------------
//// Stress test "totalCostUsingAcc"
//// ---------------------------------

//// from-scratch implementation of totalCost using an accumulator

//let rec totalCostUsingAcc costSoFar gift =
//    match gift with 
//    | Book book -> 
//        costSoFar + book.price  // final result
//    | Chocolate choc -> 
//        costSoFar + choc.price  // final result
//    | Wrapped (innerGift,style) -> 
//        let newCostSoFar = costSoFar + 0.5m
//        totalCostUsingAcc newCostSoFar innerGift 
//    | Boxed innerGift -> 
//        let newCostSoFar = costSoFar + 1.0m
//        totalCostUsingAcc newCostSoFar innerGift 
//    | WithACard (innerGift,message) -> 
//        let newCostSoFar = costSoFar + 2.0m
//        totalCostUsingAcc newCostSoFar innerGift 

//// no problems with stack overflow now!
//deeplyNestedBox 1000 |> totalCostUsingAcc 0.0m     // OK    1020.0M
//deeplyNestedBox 10000 |> totalCostUsingAcc 0.0m    // OK   10020.0M
//deeplyNestedBox 100000 |> totalCostUsingAcc 0.0m   // OK  100020.0M
//deeplyNestedBox 1000000 |> totalCostUsingAcc 0.0m  // OK 1000020.0M


//// =======================
//// Defining foldGift
//// =======================

//let rec foldGift fBook fChocolate fWrapped fBox fCard acc gift :'r =
//    let recurse = foldGift fBook fChocolate fWrapped fBox fCard 
//    match gift with 
//    | Book book -> 
//        let finalAcc = fBook acc book
//        finalAcc      // final result
//    | Chocolate choc -> 
//        let finalAcc = fChocolate acc choc
//        finalAcc     // final result
//    | Wrapped (innerGift,style) -> 
//        let newAcc = fWrapped acc style
//        recurse newAcc innerGift 
//    | Boxed innerGift -> 
//        let newAcc = fBox acc 
//        recurse newAcc innerGift 
//    | WithACard (innerGift,message) -> 
//        let newAcc = fCard acc message 
//        recurse newAcc innerGift

//(*
//val foldGift :
//  fBook:('a -> Book -> 'r) ->
//  fChocolate:('a -> Chocolate -> 'r) ->
//  fWrapped:('a -> WrappingPaperStyle -> 'a) ->
//  fBox:('a -> 'a) ->
//  fCard:('a -> string -> 'a) -> 
//  // accumulator
//  acc:'a -> 
//  // input value
//  gift:Gift -> 
//  // return value
//  'r
//*)


//(*

//// ---------------------
//// non-recursive cases
//// ---------------------

//// original catamorphism
//fBook:(Book -> 'r)
//fChocolate:(Chocolate -> 'r)

//// fold
//fBook:('a -> Book -> 'r)
//fChocolate:('a -> Chocolate -> 'r)

//// ---------------------
//// recursive cases
//// ---------------------

//// original catamorphism
//fWrapped:('r -> WrappingPaperStyle -> 'r) 
//fBox:('r -> 'r) 

//// fold
//fWrapped:('a -> WrappingPaperStyle -> 'a)
//fBox:('a -> 'a)

//*)

//// ==============================================
////    Rules for creating a fold
////
////    * Create a function parameter to handle each case in the structure.
////    * Add an additional parameter as an accumulator.
////    * For non-recursive cases, pass the function parameter the accumulator plus all the data associated with that case.
////    * For recursive cases, perform two steps:
////      * First, pass the handler the accumulator plus all the data associated with that case (except the inner recursive data). The result is a new accumulator value.
////      * Then, call the fold recursively on the nested value using the new accumulator value.
////
////    Note that each handler only "sees" the data for that case, and the accumulator passed to it from the outer level.
////    It does not have access to the results from the inner levels.
//// ==============================================


//// ---------------------------------
//// Define and test "totalCostUsingFold"
//// ---------------------------------


//let totalCostUsingFold gift =   // no longer recursive! "rec" no longer needed.

//    let fBook costSoFar (book:Book) = 
//        costSoFar + book.price
//    let fChocolate costSoFar (choc:Chocolate) = 
//        costSoFar + choc.price
//    let fWrapped costSoFar style = 
//        costSoFar + 0.5m
//    let fBox costSoFar = 
//        costSoFar + 1.0m
//    let fCard costSoFar message = 
//        costSoFar + 2.0m

//    // initial accumulator
//    let initialAcc = 0m

//    // call the fold
//    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift 


//deeplyNestedBox 100000 |> totalCostUsingFold  // no problem   100020.0M
//deeplyNestedBox 1000000 |> totalCostUsingFold // no problem  1000020.0M


//// =======================
//// Problems with fold
//// =======================

//// ---------------------------------
//// Define and test "descriptionUsingFold"
//// ---------------------------------

//let descriptionUsingFold gift =
//    let fBook descriptionSoFar (book:Book) = 
//        sprintf "'%s' %s" book.title descriptionSoFar

//    let fChocolate descriptionSoFar (choc:Chocolate) = 
//        sprintf "%A chocolate %s" choc.chocType descriptionSoFar

//    let fWrapped descriptionSoFar style = 
//        sprintf "%s wrapped in %A paper" descriptionSoFar style

//    let fBox descriptionSoFar = 
//        sprintf "%s in a box" descriptionSoFar 

//    let fCard descriptionSoFar message = 
//        sprintf "%s with a card saying '%s'" descriptionSoFar message

//    // initial accumulator
//    let initialAcc = ""

//    // main call
//    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift 

//birthdayPresent |> descriptionUsingFold  
//// "'Wolf Hall'  with a card saying 'Happy Birthday' wrapped in HappyBirthday paper"
//// CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"

//christmasPresent |> descriptionUsingFold  
//// "SeventyPercent chocolate  wrapped in HappyHolidays paper in a box"
//// CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"

//// ---------------------------------
//// Define and test "descriptionUsingFoldWithGenerator"
//// ---------------------------------

//let descriptionUsingFoldWithGenerator gift =

//    let fBook descriptionGenerator (book:Book) = 
//        descriptionGenerator (sprintf "'%s'" book.title)

//    let fChocolate descriptionGenerator (choc:Chocolate) = 
//        descriptionGenerator (sprintf "%A chocolate" choc.chocType)

//    let fWrapped descriptionGenerator style = 
//        let newDescriptionGenerator innerText =
//            let newInnerText = sprintf "%s wrapped in %A paper" innerText style
//            descriptionGenerator newInnerText 
//        newDescriptionGenerator 

//    let fBox descriptionGenerator = 
//        let newDescriptionGenerator innerText =
//            let newInnerText = sprintf "%s in a box" innerText 
//            descriptionGenerator newInnerText 
//        newDescriptionGenerator 

//    let fCard descriptionGenerator message = 
//        let newDescriptionGenerator innerText =
//            let newInnerText = sprintf "%s with a card saying '%s'" innerText message 
//            descriptionGenerator newInnerText 
//        newDescriptionGenerator 

//    // initial DescriptionGenerator
//    let initialAcc = fun innerText -> innerText 

//    // main call
//    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift 

//birthdayPresent |> descriptionUsingFoldWithGenerator  
//// CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"

//christmasPresent |> descriptionUsingFoldWithGenerator  
//// CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"

//(*
//RULE:
//* if your handlers need to do something before the recursion step - no prob
//* if your handlers need to do something AFTER the recursion - use a function as an acculator
//*)


//(*
//#time
//deeplyNestedBox 1000 |> descriptionUsingFoldWithGenerator  |> ignore
//#time
//// Real: 00:00:00.007, CPU: 00:00:00.015, GC gen0: 6, gen1: 1, gen2: 0

//#time
//deeplyNestedBox 10000 |> descriptionUsingFoldWithGenerator  |> ignore
//#time
//// Real: 00:00:01.741, CPU: 00:00:01.968, GC gen0: 127, gen1: 2, gen2: 2

//// descriptionUsingFoldWithGenerator is SLOOOW for deeply nested boxes!

//#time
//deeplyNestedBox 100000 |> descriptionUsingFoldWithGenerator  |> ignore
//#time
//// Real: 00:02:02.574, CPU: 00:02:14.093, GC gen0: 337, gen1: 211, gen2: 210
//*)

//// ---------------------------------
//// Define and test "descriptionUsingFoldWithGenerator_WithLambdas"
//// ---------------------------------

//// tidy up the implementation with lambdas
//let descriptionUsingFoldWithGenerator_WithLambdas gift =

//    let fBook descriptionGenerator (book:Book) = 
//        descriptionGenerator (sprintf "'%s'" book.title)

//    let fChocolate descriptionGenerator (choc:Chocolate) = 
//        descriptionGenerator (sprintf "%A chocolate" choc.chocType)

//    let fWrapped descriptionGenerator style = 
//        fun innerText ->
//            let newInnerText = sprintf "%s wrapped in %A paper" innerText style
//            descriptionGenerator newInnerText 

//    let fBox descriptionGenerator = 
//        fun innerText ->
//            let newInnerText = sprintf "%s in a box" innerText 
//            descriptionGenerator newInnerText 

//    let fCard descriptionGenerator message = 
//        fun innerText ->
//            let newInnerText = sprintf "%s with a card saying '%s'" innerText message 
//            descriptionGenerator newInnerText 

//    // initial DescriptionGenerator
//    let initialAcc = fun innerText -> innerText 

//    // main call
//    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift 

//birthdayPresent |> descriptionUsingFoldWithGenerator
//// CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"

//christmasPresent |> descriptionUsingFoldWithGenerator
//// CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"

//// ---------------------------------
//// Define and test "totalCostUsingFoldWithGenerator"
//// ---------------------------------

//let totalCostUsingFoldWithGenerator gift =   // no longer recursive! "rec" no longer needed.

//    let fBook costGenerator (book:Book) = 
//        costGenerator book.price
//    let fChocolate costGenerator (choc:Chocolate) = 
//        costGenerator choc.price
//    let fWrapped costGenerator style = 
//        fun innerCost ->
//            costGenerator (innerCost + 0.5m)
//    let fBox costGenerator = 
//        fun innerCost ->
//            costGenerator (innerCost + 1.0m)
//    let fCard costGenerator message = 
//        fun innerCost ->
//            costGenerator (innerCost + 2.0m)

//    // initial accumulator
//    let initialAcc = id

//    // call the fold
//    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift 


//deeplyNestedBox 100000 |> totalCostUsingFoldWithGenerator // no problem   100020.0M
//deeplyNestedBox 1000000 |> totalCostUsingFoldWithGenerator // no problem  1000020.0M

//(*
//left folder vs right fold - right fold can make using constructors
//*)

//// =======================
//// Introducing foldback
//// =======================

//let rec foldbackGift fBook fChocolate fWrapped fBox fCard generator gift :'r =
//    let recurse = foldbackGift fBook fChocolate fWrapped fBox fCard 
//    match gift with 
//    | Book book -> 
//        generator (fBook book)
//    | Chocolate choc -> 
//        generator (fChocolate choc)
//    | Wrapped (innerGift,style) -> 
//        let newGenerator innerVal =
//            let newInnerVal = fWrapped innerVal style
//            generator newInnerVal 
//        recurse newGenerator innerGift 
//    | Boxed innerGift -> 
//        let newGenerator innerVal =
//            let newInnerVal = fBox innerVal 
//            generator newInnerVal 
//        recurse newGenerator innerGift 
//    | WithACard (innerGift,message) -> 
//        let newGenerator innerVal =
//            let newInnerVal = fCard innerVal message 
//            generator newInnerVal 
//        recurse newGenerator innerGift 
//(*
//val foldbackGift :
//  fBook:(Book -> 'a) ->
//  fChocolate:(Chocolate -> 'a) ->
//  fWrapped:('a -> WrappingPaperStyle -> 'a) ->
//  fBox:('a -> 'a) ->
//  fCard:('a -> string -> 'a) ->
//  // accumulator
//  generator:('a -> 'r) -> 
//  // input value
//  gift:Gift -> 
//  // return value
//  'r
//*)

///// Bettert to define foldback in terms of fold
//let foldbackGift' fBook fChocolate fWrapped fBox fCard gift :'r =
//    let fBook' generator book =  
//        generator (fBook book)
//    let fChocolate' generator choc =
//        generator (fChocolate choc)
//    let fWrapped' generator style =
//        fun innerVal -> 
//            let newInnerVal = fWrapped innerVal style
//            generator newInnerVal 
//    let fBox' generator = 
//        fun innerVal -> 
//            let newInnerVal = fBox innerVal 
//            generator newInnerVal 
//    let fCard' generator message = 
//        fun innerVal -> 
//            let newInnerVal = fCard innerVal message 
//            generator newInnerVal 
//    let initialGenerator = id
//    foldGift fBook' fChocolate' fWrapped' fBox' fCard' initialGenerator gift 


//(*
//val foldbackGift' :
//    fBook:(Book -> 'r) ->
//    fChocolate:(Chocolate -> 'r) ->
//    fWrapped:('r -> WrappingPaperStyle -> 'r) ->
//    fBox:('r -> 'r) -> 
//    fCard:('r -> string -> 'r) -> 
//    // input value
//    gift:Gift -> 
//    // return value
//    'r
//*)

//let descriptionUsingFoldBack gift =
//    let fBook (book:Book) = 
//        sprintf "'%s'" book.title 
//    let fChocolate (choc:Chocolate) = 
//        sprintf "%A chocolate" choc.chocType
//    let fWrapped innerText style = 
//        sprintf "%s wrapped in %A paper" innerText style
//    let fBox innerText = 
//        sprintf "%s in a box" innerText 
//    let fCard innerText message = 
//        sprintf "%s with a card saying '%s'" innerText message 

//    // initial descriptionGenerator
//    let initialAcc = fun innerText -> innerText // could be replaced with id

//    // main call
//    foldbackGift fBook fChocolate fWrapped fBox fCard initialAcc gift 

//birthdayPresent |> descriptionUsingFoldBack
//// CORRECT "'Wolf Hall' wrapped in HappyBirthday paper with a card saying 'Happy Birthday'"

//christmasPresent |> descriptionUsingFoldBack
//// CORRECT "SeventyPercent chocolate in a box wrapped in HappyHolidays paper"

//// =======================
//// Compare sig of "descriptionUsingFoldBack" with original cata "descriptionUsingCata"
//// =======================

//let descriptionUsingCata gift =
//    let fBook (book:Book) = 
//        sprintf "'%s'" book.title 
//    let fChocolate (choc:Chocolate) = 
//        sprintf "%A chocolate" choc.chocType
//    let fWrapped (innerText,style) = 
//        sprintf "%s wrapped in %A paper" innerText style
//    let fBox innerText = 
//        sprintf "%s in a box" innerText
//    let fCard (innerText,message) = 
//        sprintf "%s with a card saying '%s'" innerText message
//    // call the catamorphism
//    cataGift fBook fChocolate fWrapped fBox fCard gift
    
//// =======================
//// Swapping parameter order for foldback
//// =======================

//// 
//let rec foldbackGiftWithAccLast fBook fChocolate fWrapped fBox fCard gift generator :'r =
////swapped =>                                                         ~~~~~~~~~~~~~~ 

//    let recurse = foldbackGiftWithAccLast fBook fChocolate fWrapped fBox fCard 

//    match gift with 
//    | Book book -> 
//        generator (fBook book)
//    | Chocolate choc -> 
//        generator (fChocolate choc)

//    | Wrapped (innerGift,style) -> 
//        let newGenerator innerVal =
//            let newInnerVal = fWrapped style innerVal 
////swapped =>                           ~~~~~~~~~~~~~~ 
//            generator newInnerVal 
//        recurse innerGift newGenerator  
////swapped =>    ~~~~~~~~~~~~~~~~~~~~~~ 

//    | Boxed innerGift -> 
//        let newGenerator innerVal =
//            let newInnerVal = fBox innerVal 
//            generator newInnerVal 
//        recurse innerGift newGenerator  
////swapped =>    ~~~~~~~~~~~~~~~~~~~~~~ 

//    | WithACard (innerGift,message) -> 
//        let newGenerator innerVal =
//            let newInnerVal = fCard message innerVal 
////swapped =>                        ~~~~~~~~~~~~~~~~ 
//            generator newInnerVal 
//        recurse innerGift newGenerator 
////swapped =>    ~~~~~~~~~~~~~~~~~~~~~~ 


//(*
//val foldbackGift :
//  fBook:(Book -> 'a) ->
//  fChocolate:(Chocolate -> 'a) ->
//  fWrapped:(WrappingPaperStyle -> 'a -> 'a) ->
//  fBox:('a -> 'a) ->
//  fCard:(string -> 'a -> 'a) ->
//  // input value
//  gift:Gift -> 
//  // accumulator
//  generator:('a -> 'r) -> 
//  // return value
//  'r
//*)


