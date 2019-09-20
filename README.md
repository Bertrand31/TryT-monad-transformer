# TryT monad transformer

This monad transformer is born out of a -perceived- shortcoming of Cats.
Because it seems that the Cats community encourages using Cats' Either for error handling instead of Scala's Try, they provide an EitherT monad transformer, but no TryT.

However, I personally prefer using Try for error handling, as it seems to fit the job tighter: it is specific to error handling and can do nothing else.
It also makes complete sense that it is "success-biased", whereas the fact that Cats' Either is right-biased always seeemed dubious to me. If it's really left or right, there's no reason for it to be biased towards either side.
Whereas it makes complete sense that Try is success-biased because well, we want our programs to keep running.

Because of that, I found myself needing an equivalent of EitherT for Try, so I made it myself.
It really looks like a hybrid between EitherT and OptionT, with a little less functionnality.

The things that I've left out are mostly because either they don't apply to Try, or I do not understand them well enough yet.

All contributions or suggestions are welcome!
