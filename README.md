# TryT monad transformer

## Introduction

This monad transformer is born out of a personal preference for Try over Either for error handling.

Because the Cats community encourages using Cats' Either for error handling instead of Scala's Try, they provide an EitherT monad transformer, but no TryT.

However, I personally prefer using Try for error handling, as it seems to fit the job tighter: it is specific to error handling and can do nothing else.
It also makes complete sense that it is "success-biased", whereas the fact that Cats' Either is right-biased always seeemed dubious to me. If it's really left or right, there's no reason for it to be biased towards either side.
Whereas it makes complete sense that Try is success-biased because well, we want our programs to keep running.

Because of that, I found myself needing an equivalent of EitherT for Try, so I made it myself.
It really looks like a hybrid between EitherT and OptionT, with a little less functionnality.

The things that I've left out are mostly because either they don't apply to Try, or I do not understand them well enough yet.

All contributions or suggestions are welcome!

## Usage

There are two ways to wrap a value with the TryT wrapper. Either using the `pure` function on a raw value:
```scala
TryT.pure[IO, String]("foo")
// res0: utils.TryT[cats.effect.IO,String] = TryT(IO(Success(foo)))
```
Or, the preferred and more useful way, using the standard apply method:
```scala
val asyncValue = IO(Try(3))
TryT(asyncValue)
// res1: utils.TryT[cats.effect.IO,Int] = TryT(IO$1338210249)
```
From there, it's easy to operate on the TryT that acts like both monads (here, IO and Try) were fused together:
```scala
TryT(IO(Try(3))
  .map(_ + 3)
  .flatMap(number => Try { number / 6 })
  .tap(println)
```
To see all the available methods, you can either look at [the source code](https://github.com/Bertrand31/TryT-monad-transformer/blob/master/src/main/scala/tryt/TryT.scala) or [the specs](https://github.com/Bertrand31/TryT-monad-transformer/blob/master/src/test/scala/tryt/TryTSpec.scala).
