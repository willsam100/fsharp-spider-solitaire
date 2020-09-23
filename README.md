# Spider Solitare 

Description:
An implementation of spider solitare. The structure is loosly modeled off OpenAi's gym. An extra layer that uses ML to solve the game. There are two ML approaches. The first is randomly with using MCTS. The second is via a network a request in addtion to MCTS. The network request (simlar to Open AI's gym) can use any approach. 

All code has been writen in F#, with a focus on immutable data stuctures and and accurate domain model. 

Install .NET Core: 
https://dotnet.microsoft.com/download


## The Games:

All of th game and rules are implemented in the `SpiderSolitare` project. `SpiderSolitare.fs` contains all of the game logic. 
Unit test and property based tests are in `SpiderSolitarTests`

`MonteCarloTreeSearch.fs` contains and implementationf the [MCTS algorithm](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search)

`Representation.fs` is an implementaiton for one-hot-encoding of the spider game. This is an ML industry standard for encoding data. 

## Solving the Game:

`Console` is the project that foucses on solving the game. 
The `Console.fs` is the root file and cotains the entry point to program. 
Running `dotnet run -- play 50` will kick of an MCTS solver to solve game number 50 where 50 is the seed number. 

Tunning the params to play the game will be required. 

Various file paths in the project may also need to be fixed as well as they are not yet cross-platfrom. 




