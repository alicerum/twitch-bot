# Twitch-Bot
This is a pet project of mine which I am using as a platform for learning Haskell.

It is a simple Twitch chatbot, which currently can only print some information in the chat in response to the `!echo` command.

However, chatbot can be extended quite easily.

## TODO

- [X] Implement parser pattern in `Twitch/Message.hs`
- [ ] Improve error handling around config parser
- [ ] Extract error types (`ExpectT` transformer) into standalone module
- [ ] Implement haskell evaluation twitch chatbot command `!runh`
