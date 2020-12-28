# Skyrim alchemy helper

## Overview
A useless tool to help discover ingredient effects in Skyrim. 

The tool itself is completely useless to everyone except me: if you
want to know the effects of an ingredient and don't want to do the
work yourself, then just Google it.

I love discovering everything in my video games on my own, and I try
very hard to avoid learning something about a game in any way other
than playing the game. So why did I write this tool? Because to do so,
I had to do all of the thinking that I love to do when playing
Skyrim's alchemy minigame. This is my way of playing Skyrim while I'm
away from my PS4 (and of course it's an opportunity to flex my Haskell
muscle).

## How alchemy works in Skyrim

There are ingredients, all of which have exactly 4 distinct effects.
The effects are initially unknown to the player. The player can
combine up to three ingredients at a time, consuming them in the
process, and if any of them have a shared effect, then that effect is
revealed to the player. (The player can also eat an ingredient to find
out its first effect, and upgrading this ability fully allows the
player to learn everything about an ingredient by eating it... but why
waste perk points when I can math it out?)

For a simple example, consider two well-known ingredients: `Butterfly
Wing` and `Blue Mountain Flower`. Let's say you just started your
game, so you don't know their effects.

```
Blue Mountain Flower : ?, ?, ?, ?
Butterfly Wing       : ?, ?, ?, ?
```

When you combine them, you discover that they both have the `Restore
health` effect.

```
Blue Mountain Flower : Restore health, ?, ?, ?
Butterfly Wing       : Restore health, ?, ?, ?
```

You can also infer that the remaining six effects are distinct. For
example, consider `Wheat`:

```
Blue Mountain Flower : Restore health, ?, ?, ?
Butterfly Wing       : Restore health, ?, ?, ?
Wheat                : ?             , ?, ?, ?
```

When you combine `Blue Mountain Flower` and `Wheat`, you learn that
they both have `Restore health` and `Fortify health`.

```
Blue Mountain Flower : Restore Health, Fortify Health, ?, ?
Butterfly Wing       : Restore Health, ?             , ?, ?
Wheat                : Restore Health, Fortify Health, ?, ?
```

But that's not all. You can now infer that `Buttefly Wing` cannot
possibly have `Fortify Health`, since the only effect it shares with
`Blue Mountain Flower` is `Restore health`!

So even if two ingredients don't match, you still gain _some_ amount
of information from trying to combine them. If you could quantify
this, then you'd have an algorithm for solving Skyrim's alchemy: to
choose three ingredients to combine, pick the three that maximize the
expected information.

To be precise, every time you combine some ingredients, you lower the
number of ways that you could "fill in" unknown effects in a way
that's consistent with your observations so far. The amount of
"information-theory" information that you gain from this action is
then the log of the ratio between the initial number of possibilities
and the new number of possibilities.

It turns out that trying to math it out directly like this is just too
hard, even with computer assistance and a math degree. Maybe there's a
way, but I don't know it. So this tool does the next best thing:
heuristics.

## Solving alchemy heuristically

There aren't very many effects in Skyrim. I don't know how many, but
it would take a lot of creativity to come up with more than, say, 50.
There are many more ingredients than effects, probably. If there are M
effects and N ingredients, then you could learn all effects of all
ingredients with brute-force by using O(N^2) units of each ingredient
and combining each pair.

When you combine two ingredients, one of two things happens: either
you learn at least one effect on some ingredient, or you learn that
the ingredients are disjoint. Two disjoint ingredients cover 8
different effects. Twelve disjoint ingredients cover 48 different
effects. If you knew twelve disjoint ingredients, then you could rule
out 48 effects for any ingredient in just six steps (remember that you
can combine three ingredients at a time). If there are only 48 effects
in the game, that would mean you could learn the effects of all
ingredients in no more than 6N steps (if you knew this magical
partition of effects). Writing this in terms of N and M, that's O(NM),
which should be much better than O(N^2) since M is probably much
smaller than N.

So that's my first approximation: keep track of a largest set of
disjoint ingredients, and compare each new ingredient to each
ingredient in this set. You will either learn an effect on your
ingredient, or you will discover that it's disjoint from all
ingredients in your set and therefore you now have a bigger set of
disjoint ingredients.

All this only helps you if the set of effects can be partitioned by
ingredients. I doubt that's true. Instead of looking for a partition,
one can look for a minimum cover. This is the [set cover
problem](https://en.wikipedia.org/wiki/Set_cover_problem), and
obviously it's NP-hard. This is "obvious" because if it weren't
NP-hard, then Skyrim's alchemy would be too easy to be fun! I think at
the core of every fun game is a large search space that prevents
brute-force solutions and requires solving an NP-hard problem.

# This repository

## Overview

There are two primary files: `app/Main.hs` and `src/AlchemyData.hs`.
The latter defines a data structure for keeping track of alchemical
knowledge, and the former defines a command-line interface for
interacting with this data structure.

Usually 400+ line files offend my sensibilities, but I learned that
trying to come up with a file organization structure before writing
any code is a fool's errand. And there is a cost to switching between
files during development.

So I started with all of my code in `Main.hs` and later factored out
`AlchemyData` into its own file. I don't think `AlchemyData` should be
factored further: it's a big file, but the code scores high on
coherence. `Main.hs` could be factored just a little: the `Command`
data type, the command parsing and the monadic wrappers to
`AlchemyData` queries should be in the same file because I always
modify those three pieces together, but the `AlchemyData`
serialization and parsing can go into a separate file. Maybe I'll do
that later (or maybe I already did and forgot to update this text).

## Haskell thoughts, opinions and lessons

One of the things I love about Haskell is how incredibly flexible it
is. But that's also what makes writing Haskell difficult. In languages
like Java or C++, I can usually convince myself that there is one
"best" way of writing a snippet of code. I find this much harder in
Haskell. `let` or `where`? `x -> AlchemyData -> AlchemyData` or `x ->
State AlchemyData ()` or `Has (State AlchemyData) sig m => x -> m ()`?
A `get` followed by a large `let` block with pure bindings, or a `let`
that defines stateful computations? `lens` operators or standard named
functions? Point-free style or not?

Because of Haskell's flexibility (and its mathematical influences),
understanding Haskell code is hard. Anyone can read some Python and
glean some understanding of what it's doing. I think this Haskell code
is easy to read:

```hs
positives %= multiMapInsert effect ingredient
```

but if you don't know the `%=` operator from `lens`, you might
struggle to guess what this does. Someone new to Haskell might be
confused upon seeing that the signature of `multiMapInsert` takes
three arguments even though it is only applied to two here. The type
signature of `%=` might look helpful:

```hs
(%=) :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
```

but that's only until you get stuck looking at `ASetter`:

```
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
```

This code is also perfectly reasonable, but even more baffling to
someone unfamiliar with `lens`:

```hs
negativesNonComplete . traversed %= S.delete ingredient
```

I imagine this is even more confusing for someone who isn't yet
familiar with how monads are used to model statefulness. (The code
just deletes `ingredient` from every value in the
`negativesNonComplete` map (`negativesNonComplete` isn't a map
itself---it's a lens that lets you get and set the
`_negativesNonComplete` field on an `AlchemyData` value, and that
field is a map from effect names to sets of ingredients (the
`AlchemyData` value is implicit here))).

A few more examples:

```hs
-- Lists all effects and prints them on separate lines
listAllEffects >>= mapM_ (sendIO . print)

-- A parser that outputs the ListEffects command if the user
-- typed the "effects" keyword
listEffectsCommand =
  symbol "effects" >> MP.eof >> return ListEffects
  
-- A parser that outputs the LearnIngredientEffect command if
-- the user typed
-- "learn effect <ingredient name>: <effect name> [, <effect name>]*"
learnIngredientEffectCommand = do
  void (symbol "learn effect")
  ingName <- ingredientName
  void (symbol ":")
  effs <- effectName `MP.sepEndBy1` symbol ","
  return $ LearnIngredientEffect ingName (S.fromList effs)
  
  
-- A part of a larger computation that finds ingredients to
-- fill in missing effects, while prioritizing ingredients that
-- cover multiple such effects
extraIngs <- execState (S.empty @AD.IngredientName) $ fix $ \loop -> do
  ingsSoFar <- get
  effsSoFar <- fold <$> mapM listEffectsOf (S.toList ingsSoFar)
  let missingEffs = effsMissingFromClique `S.difference` effsSoFar

  unless (S.null missingEffs) $ do
    ingsForMissingEffs <- listIngredientsWithAnyOf missingEffs

    rankedIngs <- forM (S.toList ingsForMissingEffs) $ \ingToRank -> do
      ingEffs <- listEffectsOf ingToRank
      return (ingToRank, S.size $ S.intersection ingEffs missingEffs)

    modify $ S.insert $
      fst $ maximumBy (compare `on` snd) rankedIngs

    loop
```


I think my Haskell code is readable and well-written, and I have an
easy time orienting myself and understanding what the code is doing. I
put a lot of effort into that. But it took a long time and a lot of
learning to get to this point. So Haskell is both hard to read and
hard to write---until you know enough and it becomes pleasant and
(relatively) easy.
