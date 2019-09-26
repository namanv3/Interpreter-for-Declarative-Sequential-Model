## Interpreter for Declarative Sequential Model

#### Team Members
- Naman Verma (160428)

#### Questions Answered (as of now)
- Questions 1 to 5

## Usage

Currently, using `$ ghci` is the best way to run the code.

```bash
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude>
```

In Prelude, load up Main.hs

```bash
Prelude> :l Main.hs
*Main> 
```

The file `Main.hs` has 21 test cases, labelled `p0` to `p20`. Each of these follows the syntax of Declarative Sequential Model. Some of these are semantically incorrect, however (such as `p11`). To run any one of these, the function `runC` is used.

```haskell
runC :: SStack -> SAS -> Int -> ExecutionContext
```

It takes a Semantic Stack and an SAS as input, along with the number of steps you want to run on the Stack. Its output is of the type `ExecutionContext`, which is basically a beefed up form of the tuple ` (SStack,SAS)`.

There's another function `startStack`, which takes a program as input, binds it with an empty Environment and creates a Semantic Stack.

```haskell
startStack :: Statement -> SStack
startStack program = SStack [(program,emptyEnv)]

```

So, to run the program `p12` for `5` steps, do the following:

```bash
*Main> runC (startStack p12) emptySAS 5
```

