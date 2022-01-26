# Twitch-Bot
This is a pet project of mine which I am using as a platform for learning Haskell.

It is a simple Twitch chatbot, which provides chat support for two awesome functions:
* `!runh <expr>` interprets and evaluates the expression and prints result back to chat
* `!djinn <command>` is an integrated [djinn](https://hackage.haskell.org/package/djinn)
	theorem prover by Lennart Augustsson. It calculates one of the possible function
	implementations based on its type provided by the user.

Bot is quite easily extendable. Every bot command has type of `StateT <commonState>
(MaybeT IO) String`. They can operate with general state and interact with IO if that is
necessary.

## TODO

- [X] Implement parser pattern in `Twitch/Message.hs`
- [X] Improve error handling around config parser
- [ ] Extract error types (`ExpectT` transformer) into standalone module
- [X] Implement haskell evaluation twitch chatbot command `!runh`
- [X] Incorporate `Djinn` into the bot and create `!djinn` chat command to invoke
- [ ] Instead of forcing evaluation in `runHString`, force first N letters of stringified response

